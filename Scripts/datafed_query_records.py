"""Query DataFed collections/records using a JSON query definition and export record detail CSVs."""

import os
import json
import argparse
import logging
import configparser


try:
    import pandas as pd  # type: ignore
except Exception:  # pragma: no cover
    pd = None
try:
    from tqdm import tqdm  # type: ignore
except Exception:  # pragma: no cover

    def tqdm(x, **kwargs):
        return x


try:  # Optional at import so tests can dry-run without datafed installed
    from datafed.CommandLib import API  # type: ignore
except Exception:  # pragma: no cover
    API = None


CONFIG_SECTION = "DataFed Project Settings"
DRY_RUN_ENV = "ECOKMER_DRY_RUN"


def create_config(filename):
    """Interactively create config for query operations (endpoint, download dir, context)."""
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
    """Load config file (ini) into ConfigParser."""
    config = configparser.ConfigParser()
    config.read(filename)
    return config


def setup_logging(log_file):
    """Configure file + console logging (creates directories as needed)."""
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
    """Authenticate to DataFed using DF_USER/DF_PASS environment variables (skipped if dry-run)."""
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
        raise RuntimeError("DF_USER / DF_PASS must be set for authentication")
    api = API()
    try:
        api.loginByPassword(user, pwd)
        logging.info("Authenticated DataFed user '%s'", user)
        return api
    except Exception as e:  # pragma: no cover
        logging.error("DataFed authentication failed: %s", e)
        raise RuntimeError(f"DataFed authentication failed: {e}")


def load_schema(query_file):
    """Load JSON query definition specifying collection(s), filters, and pagination parameters."""
    if not os.path.exists(query_file):
        logging.error(f"Query file not found: {query_file}")
        raise FileNotFoundError(f"Query file not found: {query_file}")
    with open(query_file, "r") as f:
        schema = json.load(f)
    return schema


def search_records(df_api, query):
    """
    Searches DataFed records using the queryDirect function based on input query file.
    Returns a list of records.
    """
    try:
        # Define search parameters
        coll_mode = "json"  # Output format
        coll = query["coll"]  # Collection to search in
        id = query["id"]
        text = query["text"]
        tags = query[
            "text"
        ]  # GOTCHA: tags reuses 'text' value; verify this is intentional.
        schema = query["schema"]
        meta = query["meta"]
        meta_err = query["meta_err"]
        owner = query["owner"]
        creator = query["creator"]
        time_from = query["time_from"]
        time_to = query["time_to"]
        public = query["public"]
        category = query["category"]
        sort = query["sort"]
        sort_rev = query["sort_rev"]
        offset = query["offset"]
        count = query["count"]

        # Execute the query
        results = df_api.queryDirect(
            coll_mode,
            coll,
            id,
            text,
            tags,
            schema,
            meta,
            meta_err,
            owner,
            creator,
            time_from,
            time_to,
            public,
            category,
            sort,
            sort_rev,
            offset,
            count,
        )
    except Exception as e:
        logging.error(f"Error during search: {e}")
        return []
    return results


def get_coll_contents(df_api, query):
    """See what a collection's contents are, fetching all pages of results
    for each collection in query['coll'] using the specified context (query['owner']).

    Args:
        df_api: A reference to the DataFed API handle.
        query: A dict with keys:
               - 'coll': A list of one or more collection IDs (e.g. ['c/012345', 'c/67890'])
               - 'owner': The context/owner in which to list items

    Returns:
        A dict mapping each specified collection ID to a ListingReply-like object,
        where listing.items is fully combined from all pages. Example:
        {
          'c/012345': <ListingReply with all items>,
          'c/67890': <ListingReply with all items>
        }
    """
    coll_listings = {}
    for coll_id in query["coll"]:
        offset = 0
        all_items = []

        while True:
            ls_resp = df_api.collectionItemsList(
                coll_id, context=query["owner"], offset=offset
            )  # pragma: no cover

            ls_resp = ls_resp[0]
            if not ls_resp.item:
                # No more items or returned an empty page
                break

            all_items.extend(ls_resp.item)
            offset += ls_resp.count

            if offset >= ls_resp.total:
                # Reached or exceeded total number of records for this collection
                break

        # If for some reason the listing is empty or an error, all_items might be []
        coll_listings[coll_id] = all_items

    return coll_listings


def get_record_info(df_api, coll_listings, download_dir):
    """Fetch full record details for each listing item & write per-collection CSVs in download_dir."""
    for coll_id, items in coll_listings.items():
        if not items:
            logging.warning(f"Collection {coll_id} has no items.")
            continue
        download_loc = os.path.join(
            download_dir, f"{coll_id.replace('/', '_')}_records.csv"
        )
        records = []
        for item in tqdm(
            items, dynamic_ncols=True, desc=f"Getting details for {coll_id}"
        ):
            record_id = item.id
            try:
                full_record = df_api.dataView(record_id)
                records.append(full_record)
                logging.info("Fetched details for record ID: %s", record_id)
            except Exception as e:
                logging.error("Failed to retrieve record %s: %s", record_id, e)
        # Serialize
        # ser_records = make_serializable(records)
        rows = []
        for rec in records:
            print(rec[0].data[0])
            r = rec[0].data[0]
            base = {
                "id": r.id,
                "title": r.title,
                "alias": r.alias,
                "tags": r.tags,
                "dependencies": r.deps,
            }

            # expand metadata JSON string (if any)
            if r.metadata:
                try:
                    meta = json.loads(r.metadata)
                except json.JSONDecodeError:
                    meta = {}
            else:
                meta = {}

            # merge and append
            base.update(meta)
            rows.append(base)

        # make DataFrame
        df = pd.DataFrame(rows)

        df.to_csv(download_loc, index=False)
        logging.info(f"Wrote {len(df)} rows for collection {coll_id} to {download_loc}")


def make_serializable(obj):
    """
    Recursively convert non-serializable objects (e.g., API response objects)
    to dictionaries.
    """
    if isinstance(obj, (str, int, float, bool)) or obj is None:
        return obj
    elif isinstance(obj, list):
        return [make_serializable(item) for item in obj]
    elif isinstance(obj, dict):
        return {key: make_serializable(value) for key, value in obj.items()}
    elif hasattr(obj, "__dict__"):
        return make_serializable(obj.__dict__)
    else:
        return str(obj)


def is_dry_run():
    """Return True when dry-run mode active via env var or CLI flag attribute."""
    return os.environ.get(DRY_RUN_ENV) == "1" or getattr(is_dry_run, "_flag", False)


def run(argv=None):
    """CLI entry: parse config + query file, authenticate, enumerate collections, export record metadata."""
    parser = argparse.ArgumentParser(
        description="Download specified data from DataFed based on input file names."
    )
    parser.add_argument(
        "--config",
        default="www/DataFedSettings.cfg",
        help="Path to the configuration file.",
    )
    parser.add_argument("--query", required=True, help="Path to the query file json.")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Parse config / query only; skip remote API calls",
    )
    args = parser.parse_args(argv)

    if args.dry_run:
        is_dry_run._flag = True  # type: ignore

    config_file = args.config
    if not os.path.exists(config_file):
        print(f"No config file found at '{config_file}'.")
        create_config(config_file)
        print("Please update the configuration file and rerun the script.")
        return 2

    config_settings = read_config(config_file)
    download_dir = config_settings.get(
        CONFIG_SECTION, "download_directory", fallback="Data/downloads"
    )
    log_file = os.path.join(
        download_dir, "logs", "upload.log"
    )  # TODO: File named 'upload.log' for query tool; consider renaming.
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    setup_logging(log_file)

    query = load_schema(args.query)
    if is_dry_run():
        logging.info("[dry-run] Loaded config and query; skipping remote operations")
        return 0

    df_api = authenticate()
    globus_uuid = config_settings.get(CONFIG_SECTION, "globus_uuid")
    df_api.endpointDefaultSet(globus_uuid)
    logging.info(f"Set default Globus endpoint to UUID: {globus_uuid}")
    context = query["owner"]
    df_api.setContext(context)
    logging.info(f"Set DataFed context to: {context}")
    coll_contents = get_coll_contents(df_api, query)
    get_record_info(df_api, coll_contents, download_dir)
    return 0


def main(argv=None, embedded=False):
    """Module main wrapper for programmatic invocation (raises if non-zero exit when embedded)."""
    code = run(argv)
    if embedded or os.environ.get("ECOKMER_EMBEDDED") == "1":
        if code != 0:
            raise RuntimeError(f"datafed_query_records failed (code={code})")
    else:
        import sys

        sys.exit(code)


if __name__ == "__main__":  # pragma: no cover
    main()
