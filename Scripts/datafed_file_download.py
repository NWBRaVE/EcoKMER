"""Download primary files for a list of DataFed records specified via txt/csv/jsonl input."""

import os
import json
import csv
import argparse
import logging
import configparser


# Optional import: defer heavy / unavailable dependency for dry-run or test environments
try:  # pragma: no cover - import guard
    from datafed.CommandLib import API  # type: ignore
except Exception:  # ImportError or other issues
    API = None  # Will be validated at authenticate() time


CONFIG_SECTION = "DataFed Project Settings"
DRY_RUN_ENV = (
    "ECOKMER_DRY_RUN"  # allow env-based dry-run toggle (also set via --dry-run)
)


def create_config(filename):
    """Interactively create a config file (endpoint, download directory, context)."""
    print("Creating DataFed download configuration...")
    endpoint_uuid = input("Enter your Globus Endpoint UUID: ").strip()
    download_directory = (
        input("Enter your download directory path (e.g., Data/downloads/): ").strip()
        or "Data/downloads/"
    )
    context = input("Enter the DataFed context (e.g., p/brave): ").strip()

    # Validate download directory
    if not os.path.exists(download_directory):
        os.makedirs(download_directory)
        print(f"Created download directory at '{download_directory}'.")

    config = configparser.ConfigParser()
    config[CONFIG_SECTION] = {
        "globus_uuid": endpoint_uuid,
        "download_directory": download_directory,
        "context": context,
    }

    # Write the configuration to a file
    with open(filename, "w") as configfile:
        config.write(configfile)
    print(f"Configuration file created at '{filename}'.")


def read_config(filename):
    """Load config file into ConfigParser."""
    config = configparser.ConfigParser()
    config.read(filename)
    return config


def setup_logging(log_file):
    """Configure file + console logging (idempotent)."""
    logging.basicConfig(
        filename=log_file,
        filemode="a",
        format="%(asctime)s - %(levelname)s - %(message)s",
        level=logging.INFO,
    )
    # Console logging
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    formatter = logging.Formatter("%(levelname)s - %(message)s")
    console.setFormatter(formatter)
    logging.getLogger("").addHandler(console)


def authenticate():
    """Authenticate with DataFed using DF_USER / DF_PASS env vars.

    In dry-run mode (flag or env), this is skipped.
    """
    if is_dry_run():
        logging.info("[dry-run] Skipping authentication")
        return None
    if API is None:
        raise RuntimeError(
            "datafed library not available; install `datafed` or use --dry-run"
        )
    user = os.getenv("DF_USER")
    pwd = os.getenv("DF_PASS")
    if not user or not pwd:
        raise RuntimeError(
            "DF_USER / DF_PASS must be set in environment for authentication"
        )
    api = API()
    try:
        api.loginByPassword(user, pwd)
        logging.info("Authenticated with DataFed user '%s'", user)
        return api
    except Exception as e:  # pragma: no cover - network dependent
        logging.error("DataFed authentication failed: %s", e)
        raise RuntimeError(f"DataFed authentication failed: {e}")


def search_records(df_api, file_name, context):
    """Query for a record id matching the provided file_name within owner/context.

    GOTCHA: Returns a single record id (first match) or None; caller treats falsy as not found.
    """
    print("got to search_records")
    try:
        # Execute the query
        results = df_api.queryDirect(
            id=file_name, owner=context
        )  # pragma: no cover (remote)
        # Parse the results to extract record IDs
        if results and results[0] and results[0].item:
            record_id = results[0].item[0].id
            return record_id
        else:
            logging.info("No results found for '%s'", file_name)
    except Exception as e:
        logging.error(f"Error during search for '{file_name}': {e}")
        return []


def download_file(df_api, record_id, download_dir):
    """
    Downloads the primary file associated with a DataFed record.
    """
    try:
        # Fetch record metadata to find file paths
        record = df_api.dataView(record_id)  # pragma: no cover (remote)
        file_path = record[0].data[0].source
        if file_path:
            dest_path = os.path.join(download_dir, os.path.basename(file_path))
            df_api.dataGet([record_id], dest_path)
            logging.info("Downloaded '%s' to '%s'", file_path, dest_path)
    except Exception as e:
        logging.error(f"Failed to download record ID '{record_id}': {e}")


def download_record_files(df_api, file_name, download_dir, context):
    """Locate record by file_name then download its primary file (if found)."""
    record_id = search_records(df_api, file_name, context)
    print(record_id)
    if not record_id:
        logging.warning(f"No records found for file name '{file_name}'.")
        return
    # for record_id in record_ids:
    download_file(df_api, record_id, download_dir)


def detect_input_type(file_path):
    """
    Detects the input file type based on its extension.
    Supports .txt, .csv, and .jsonl.
    """
    _, ext = os.path.splitext(file_path.lower())
    if ext == ".txt":
        return "txt"
    elif ext == ".csv":
        return "csv"
    elif ext == ".jsonl":
        return "jsonl"
    else:
        logging.error(
            f"Unsupported input file extension: '{ext}'. Supported types are .txt, .csv, .jsonl."
        )
        raise RuntimeError(f"Unsupported input file extension: '{ext}'.")


def parse_input(input_path, column=None):
    """
    Parses input file based on its detected type and returns a list of file names.
    """
    input_type = detect_input_type(input_path)
    file_names = []
    if input_type == "txt":
        with open(input_path, "r") as f:
            file_names = [line.strip() for line in f if line.strip()]
    elif input_type == "csv":
        with open(input_path, "r", newline="") as f:
            reader = csv.DictReader(f)
            if not column or column not in reader.fieldnames:
                logging.error(f"Specified column '{column}' not found in CSV.")
                raise RuntimeError(f"Specified column '{column}' not found in CSV.")
            file_names = [row[column].strip() for row in reader if row[column].strip()]
    elif input_type == "jsonl":
        with open(input_path, "r") as f:
            for line in f:
                try:
                    data = json.loads(line)
                    name = data.get("name")  # Adjust key based on JSON structure
                    if name:
                        file_names.append(name.strip())
                except json.JSONDecodeError:
                    logging.warning("Invalid JSON line encountered and skipped.")
    return file_names


def is_dry_run():
    """Return True when dry-run mode active via env var or CLI flag-set attribute."""
    return os.environ.get(DRY_RUN_ENV) == "1" or getattr(is_dry_run, "_flag", False)


def run(argv=None):
    """CLI entry: parse args, load config, resolve input list, perform downloads unless dry-run."""
    parser = argparse.ArgumentParser(
        description="Download specified data from DataFed based on input file names."
    )
    parser.add_argument(
        "--config",
        default="www/DataFedSettings.cfg",
        help="Path to the configuration file.",
    )
    parser.add_argument(
        "--input", required=True, help="Path to the input file (txt, csv, jsonl)."
    )
    parser.add_argument("--column", help="Column name for file names in CSV input.")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Parse args & config only; skip remote operations.",
    )
    args = parser.parse_args(argv)

    if args.dry_run:
        is_dry_run._flag = True  # type: ignore

    config_file = args.config
    if not os.path.exists(config_file):
        print(f"No config file found at '{config_file}'. Creating a new one.")
        create_config(config_file)
        print(
            "Please review and update the configuration before re-running the script."
        )
        return 2  # indicate config bootstrap

    config = read_config(config_file)
    # logging setup
    configured_dl = config.get(
        CONFIG_SECTION, "download_directory", fallback="Data/downloads"
    )
    download_log = os.path.join(configured_dl, "logs", "download.log")
    os.makedirs(os.path.dirname(download_log), exist_ok=True)
    setup_logging(download_log)

    if is_dry_run():
        logging.info("[dry-run] Loaded config & skipping DataFed operations")
        return 0

    df_api = authenticate()
    globus_uuid = config.get(CONFIG_SECTION, "globus_uuid")
    df_api.endpointDefaultSet(globus_uuid)
    logging.info("Set default Globus endpoint to UUID: %s", globus_uuid)
    context = config.get(CONFIG_SECTION, "context")
    df_api.setContext(context)
    logging.info("Set DataFed context to: %s", context)

    file_names = parse_input(args.input, args.column)
    if not file_names:
        logging.error("No file names found in the input.")
        return 3

    download_dir = configured_dl if configured_dl.endswith("/") else configured_dl + "/"
    os.makedirs(download_dir, exist_ok=True)
    for file_name in file_names:
        download_record_files(df_api, file_name, download_dir, context)
    logging.info("All downloads completed.")
    return 0


def main(argv=None, embedded=False):
    """Main wrapper raising on failure when embedded (programmatic use)."""
    code = run(argv)
    if embedded or os.environ.get("ECOKMER_EMBEDDED") == "1":
        if code != 0:
            raise RuntimeError(f"datafed_file_download failed (code={code})")
    else:  # CLI
        import sys

        sys.exit(code)


if __name__ == "__main__":  # pragma: no cover
    main()
