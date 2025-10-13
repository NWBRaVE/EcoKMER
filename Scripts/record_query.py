"""DataFed record and dependency graph exporter."""

import os
import json
import argparse
import logging
import configparser
from dataclasses import dataclass
from collections import defaultdict
from typing import Dict, List, Set, Optional, Tuple, Any

try:
    import pandas as pd  # type: ignore
except Exception:  # pragma: no cover
    pd = None
try:
    from tqdm import tqdm  # type: ignore
except Exception:  # pragma: no cover

    def tqdm(x, **kwargs):
        return x


try:
    from datafed.CommandLib import API  # type: ignore
except Exception:  # pragma: no cover
    API = None
CONFIG_SECTION = "DataFed Project Settings"
DRY_RUN_ENV = "ECOKMER_DRY_RUN"  # One-liner: Comprehensive record + dependency export & analysis tool for DataFed.

# DataFed dependency type constants
DEP_TYPE_MAP = {
    0: "DEP_IS_DERIVED_FROM",
    1: "DEP_IS_COMPONENT_OF",
    2: "DEP_IS_NEW_VERSION_OF",
}

# DataFed dependency direction constants
DEP_DIR_MAP = {0: "DEP_IN", 1: "DEP_OUT"}


@dataclass
class DependencyInfo:
    """Represents a dependency relationship with type and direction."""

    target_id: str
    dep_type: str
    direction: str

    def __str__(self) -> str:
        return f"{self.target_id}({self.dep_type}:{self.direction})"


@dataclass
class DataFedRecord:
    """Data class to represent a DataFed record with its metadata."""

    id: str
    title: str
    alias: str
    tags: List[str]
    dependencies: List[DependencyInfo]
    metadata: Dict[str, Any]
    collection_id: str


class DependencyTracer:
    """Handles dependency tracing and path reconstruction for DataFed records (in-memory graph)."""

    def __init__(self):
        self.records: Dict[str, DataFedRecord] = {}
        self.dependency_graph: Dict[str, List[str]] = defaultdict(list)
        self.reverse_dependency_graph: Dict[str, List[str]] = defaultdict(list)
        self.dependency_details: Dict[str, List[DependencyInfo]] = defaultdict(list)

    def add_record(self, record: DataFedRecord) -> None:
        """Add a record to the dependency tracer."""

        self.records[record.id] = record

        # Only track parent relationships in dependency_graph
        # The reverse_dependency_graph will be built automatically
        parent_ids = []

        for dep in record.dependencies:
            self.dependency_details[record.id].append(dep)

            if self._is_parent_relationship(dep):
                parent_ids.append(dep.target_id)
            elif self._is_child_relationship(dep):
                # For child relationships, the target is a child of this record
                # We'll add this record as a parent of the target in reverse_dependency_graph
                self.reverse_dependency_graph[record.id].append(dep.target_id)

        # Store parents for this record
        self.dependency_graph[record.id] = parent_ids

        # Build reverse graph: for each parent, add this record as a child
        for parent_id in parent_ids:
            self.reverse_dependency_graph[parent_id].append(record.id)

    def _is_parent_relationship(self, dep: DependencyInfo) -> bool:
        """
        Determine if this dependency represents a parent relationship.
        A parent is something this record depends on or is derived from.
        """
        # DEP_IS_DERIVED_FROM with DEP_OUT: This record is derived FROM the target (target is parent)
        if dep.dep_type == "DEP_IS_DERIVED_FROM" and dep.direction == "DEP_OUT":
            return True

        # DEP_IS_COMPONENT_OF with DEP_OUT: This record is a component OF the target (target is parent/container)
        elif dep.dep_type == "DEP_IS_COMPONENT_OF" and dep.direction == "DEP_OUT":
            return True

        # DEP_IS_NEW_VERSION_OF with DEP_OUT: This record is a new version OF the target (target is parent/original)
        elif dep.dep_type == "DEP_IS_NEW_VERSION_OF" and dep.direction == "DEP_OUT":
            return True

        return False

    def _is_child_relationship(self, dep: DependencyInfo) -> bool:
        """
        Determine if this dependency represents a child relationship.
        A child is something that depends on this record.
        """
        # DEP_IS_DERIVED_FROM with DEP_IN: The target is derived FROM this record (this record is parent, target is child)
        if dep.dep_type == "DEP_IS_DERIVED_FROM" and dep.direction == "DEP_IN":
            return True

        # DEP_IS_COMPONENT_OF with DEP_IN: The target is a component OF this record (this record is container, target is child)
        elif dep.dep_type == "DEP_IS_COMPONENT_OF" and dep.direction == "DEP_IN":
            return True

        # DEP_IS_NEW_VERSION_OF with DEP_IN: The target is a new version OF this record (this record is original, target is child)
        elif dep.dep_type == "DEP_IS_NEW_VERSION_OF" and dep.direction == "DEP_IN":
            return True

        return False

    def get_dependency_summary_for_record(self, record_id: str) -> Dict[str, Any]:
        """Get a summary of dependencies for a record."""
        if record_id not in self.records:
            return {}

        # Get the raw dependency details
        deps = self.dependency_details.get(record_id, [])

        # Get parents (records this record depends on)
        parent_records = self.dependency_graph.get(record_id, [])

        # Get children (records that depend on this record)
        child_records = self.reverse_dependency_graph.get(record_id, [])

        summary = {
            "total_dependencies": len(deps),
            "by_type": defaultdict(int),
            "by_direction": defaultdict(int),
            "parent_records": parent_records,
            "child_records": child_records,
        }

        for dep in deps:
            summary["by_type"][dep.dep_type] += 1
            summary["by_direction"][dep.direction] += 1

        return summary

    def find_dependency_chain_to_root(
        self, record_id: str, max_depth: int = 100
    ) -> List[str]:
        """
        Find the complete dependency chain from the given record back to its root dependency.
        Returns the path from the current record to its ultimate root.
        """
        if record_id not in self.records:
            return [record_id]

        visited = set()

        def trace_to_root(current_id: str) -> List[str]:
            if current_id in visited or len(visited) >= max_depth:
                # Circular dependency detected or max depth reached
                return [current_id]

            visited.add(current_id)
            current_path = [current_id]

            # Get string dependencies from processed graph
            dependencies = self.dependency_graph.get(current_id, [])

            # If no dependencies, this is a root record
            if not dependencies:
                return current_path

            # For records with multiple dependencies, trace the first valid one
            for dep_id in dependencies:
                if dep_id in self.records:
                    dep_path = trace_to_root(dep_id)
                    return current_path + dep_path

            # If dependencies exist but aren't in record set
            return current_path + dependencies

        return trace_to_root(record_id)

    def find_dependency_chain_from_root(self, record_id: str) -> List[str]:
        """
        Find the dependency chain from root to the given record.
        This reverses the chain found by find_dependency_chain_to_root.
        """
        chain_to_root = self.find_dependency_chain_to_root(record_id)
        return list(reversed(chain_to_root))

    def find_all_dependency_chains(self) -> Dict[str, Dict[str, Any]]:
        """Find dependency chains for all records with derived metadata (root/leaf flags, depths)."""
        chains = {}
        for record_id in self.records:
            chain_to_root = self.find_dependency_chain_to_root(record_id)
            chain_from_root = list(reversed(chain_to_root))

            chains[record_id] = {
                "chain_to_root": chain_to_root,
                "chain_from_root": chain_from_root,
                "depth_from_root": len(chain_from_root) - 1,
                "is_root": len(self.dependency_graph.get(record_id, [])) == 0,
                "is_leaf": record_id not in self.reverse_dependency_graph,
                "direct_dependents": self.reverse_dependency_graph.get(record_id, []),
                "chain_ids": "|".join(chain_to_root),
                "root_id": chain_to_root[-1] if chain_to_root else record_id,
            }
        return chains

    def get_dependency_groups(self) -> Dict[str, List[str]]:
        """
        Group records by their root dependency.
        Returns a dict where keys are root record IDs and values are lists of all records in that dependency tree.
        """
        chains = self.find_all_dependency_chains()
        groups = defaultdict(list)

        for record_id, chain_info in chains.items():
            root_id = chain_info["root_id"]
            groups[root_id].append(record_id)

        return dict(groups)

    def get_root_records(self) -> List[str]:
        """Get records that have no dependencies (root records)."""
        return [rid for rid, deps in self.dependency_graph.items() if not deps]

    def get_leaf_records(self) -> List[str]:
        """Get records that are not dependencies of any other record."""
        return [
            rid
            for rid in self.records.keys()
            if rid not in self.reverse_dependency_graph
        ]

    def detect_potential_circular_dependencies(self) -> List[Tuple[str, List[str]]]:
        """
        Detect potential circular dependencies.
        Returns a list of tuples (record_id, circular_path) for any detected cycles.
        """
        circular_deps = []

        def has_cycle(
            start_id: str, current_id: str, path: List[str], visited: Set[str]
        ) -> Optional[List[str]]:
            if current_id == start_id and len(path) > 1:
                return path + [current_id]

            if current_id in visited:
                return None

            visited.add(current_id)

            # Use string-converted dependency graph
            for dep_id in self.dependency_graph.get(current_id, []):
                if dep_id in self.records:
                    cycle = has_cycle(
                        start_id, dep_id, path + [current_id], visited.copy()
                    )
                    if cycle:
                        return cycle

            return None

        for record_id in self.records:
            cycle = has_cycle(record_id, record_id, [], set())
            if cycle:
                circular_deps.append((record_id, cycle))

        return circular_deps


class DataFedClient:
    """Thin DataFed client wrapper adding logging and dry-run behavior."""

    def __init__(self, config_settings: configparser.ConfigParser):
        self.config = config_settings
        self.api = None
        self.logger = logging.getLogger(__name__)

    # def authenticate(self) -> None:
    #     """Authenticate with DataFed."""
    #     self.api = API()
    #     username = input("DataFed username: ").strip()
    #     password = getpass.getpass("DataFed password: ").strip()
    #
    #     try:
    #         self.api.loginByPassword(username, password)
    #         self.logger.info("Successfully authenticated with DataFed.")
    #     except Exception as e:
    #         self.logger.error(f"Authentication failed: {e}")
    #         raise

    def authenticate(self, dry_run: bool = False) -> None:
        """Authenticate with DataFed using environment variables (unless dry-run)."""
        if dry_run:
            self.logger.info("[dry-run] Skipping authentication")
            return
        if API is None:
            raise RuntimeError(
                "datafed library not available; install `datafed` or use --dry-run"
            )
        user = os.getenv("DF_USER")
        pwd = os.getenv("DF_PASS")
        if user is None or pwd is None:
            raise RuntimeError("DF_USER and DF_PASS environment variables must be set")
        self.api = API()
        try:
            self.api.loginByPassword(user, pwd)
            self.logger.info("Successfully authenticated with DataFed.")
        except Exception as e:  # pragma: no cover
            self.logger.error(f"DataFed authentication failed: {e}")
            raise RuntimeError(f"DataFed authentication failed: {e}")

    def setup_endpoint_and_context(
        self, query: Dict[str, Any], dry_run: bool = False
    ) -> None:
        """Setup Globus endpoint and DataFed context."""
        if dry_run:
            self.logger.info("[dry-run] Skipping endpoint/context setup")
            return
        if not self.api:
            raise RuntimeError("API not initialized. Call authenticate() first.")

        globus_uuid = self.config.get("DataFed Project Settings", "globus_uuid")
        self.api.endpointDefaultSet(globus_uuid)
        self.logger.info(f"Set default Globus endpoint to UUID: {globus_uuid}")

        context = query["owner"]
        self.api.setContext(context)
        self.logger.info(f"Set DataFed context to: {context}")

    def get_collection_contents(
        self, collection_ids: List[str], owner: str
    ) -> Dict[str, List[Any]]:
        """Get contents of specified collections (paginates until all items retrieved)."""
        if not self.api:
            raise RuntimeError("API not initialized.")

        coll_listings = {}
        for coll_id in collection_ids:
            offset = 0
            all_items = []

            while True:
                try:
                    ls_resp = self.api.collectionItemsList(
                        coll_id, context=owner, offset=offset
                    )
                    ls_resp = ls_resp[0]

                    if not ls_resp.item:
                        break

                    all_items.extend(ls_resp.item)
                    offset += ls_resp.count

                    if offset >= ls_resp.total:
                        break

                except Exception as e:
                    self.logger.error(f"Error fetching collection {coll_id}: {e}")
                    break

            coll_listings[coll_id] = all_items
            self.logger.info(
                f"Retrieved {len(all_items)} items from collection {coll_id}"
            )

        return coll_listings

    def get_record_details(self, record_id: str) -> Optional[Any]:
        """Get detailed information for a specific record."""
        if not self.api:
            raise RuntimeError("API not initialized.")

        try:
            return self.api.dataView(record_id)
        except Exception as e:
            self.logger.error(f"Failed to retrieve record {record_id}: {e}")
            return None


class DataFedProcessor:
    """Main processor orchestrating collection fetch, record parsing, dependency graph & exports."""

    def __init__(self, config_file: str):
        self.config = self._load_config(config_file)
        self.client = DataFedClient(self.config)
        self.dependency_tracer = DependencyTracer()
        self.logger = logging.getLogger(__name__)
        self.download_dir = self.config.get(
            CONFIG_SECTION, "download_directory", fallback="Data/downloads"
        )

    def _load_config(self, config_file: str) -> configparser.ConfigParser:
        """Load configuration file."""
        if not os.path.exists(config_file):
            raise FileNotFoundError(f"Config file not found: {config_file}")

        config = configparser.ConfigParser()
        config.read(config_file)
        return config

    def _parse_record(self, raw_record: Any, collection_id: str) -> DataFedRecord:
        """Parse raw DataFed record into DataFedRecord object."""
        r = raw_record[0].data[0]

        # Parse metadata
        metadata = {}
        if r.metadata:
            try:
                metadata = json.loads(r.metadata)
            except json.JSONDecodeError:
                self.logger.warning(f"Failed to parse metadata for record {r.id}")

        # Extract full dependency information with integer-to-string conversion
        dependencies = []
        if r.deps:
            for i, dep in enumerate(r.deps):
                try:
                    if hasattr(dep, "id"):
                        # Convert integer type and direction to strings
                        dep_type_int = getattr(dep, "type", -1)
                        dep_dir_int = getattr(dep, "dir", -1)

                        dep_type_str = DEP_TYPE_MAP.get(
                            dep_type_int, f"UNKNOWN_TYPE_{dep_type_int}"
                        )
                        dep_dir_str = DEP_DIR_MAP.get(
                            dep_dir_int, f"UNKNOWN_DIR_{dep_dir_int}"
                        )

                        dep_info = DependencyInfo(
                            target_id=dep.id,
                            dep_type=dep_type_str,
                            direction=dep_dir_str,
                        )
                        dependencies.append(dep_info)
                    else:
                        print(f"  dep[{i}] has no 'id' attribute")
                except Exception as e:
                    print(f"Error parsing dependency {i} for {r.id}: {e}")

        # Extract tag strings
        tag_strings = []
        if r.tags:
            for tag in r.tags:
                tag_strings.append(str(tag))

        return DataFedRecord(
            id=r.id,
            title=r.title,
            alias=r.alias,
            tags=tag_strings,
            dependencies=dependencies,
            metadata=metadata,
            collection_id=collection_id,
        )

    def process_collections(self, query: Dict[str, Any], dry_run: bool = False) -> None:
        """Process collections and extract records (skips remote calls in dry-run)."""
        self.client.authenticate(dry_run=dry_run)
        self.client.setup_endpoint_and_context(query, dry_run=dry_run)
        if dry_run:
            self.logger.info(
                "[dry-run] Would process collections: %s",
                ",".join(query.get("coll", [])),
            )
            return

        # Get collection contents
        coll_contents = self.client.get_collection_contents(
            query["coll"], query["owner"]
        )

        # Process each collection
        for coll_id, items in coll_contents.items():
            if not items:
                self.logger.warning(f"Collection {coll_id} has no items.")
                continue

            self._process_collection_records(coll_id, items)
            self._save_collection_csv(coll_id)

    def _process_collection_records(self, collection_id: str, items: List[Any]) -> None:
        """Process records from a single collection."""
        for item in tqdm(items, dynamic_ncols=True, desc=f"Processing {collection_id}"):
            raw_record = self.client.get_record_details(item.id)
            if raw_record:
                record = self._parse_record(raw_record, collection_id)
                self.dependency_tracer.add_record(record)

    def _save_collection_csv(self, collection_id: str) -> None:
        """Save records from a single collection to CSV."""
        collection_records = [
            record
            for record in self.dependency_tracer.records.values()
            if record.collection_id == collection_id
        ]

        if not collection_records:
            return

        filename = os.path.join(
            self.download_dir, f"{collection_id.replace('/', '_')}_records.csv"
        )
        self._save_records_to_csv(collection_records, filename)
        self.logger.info(
            f"Saved {len(collection_records)} records from {collection_id} to {filename}"
        )

    def _safe_convert_to_string_list(self, obj_list) -> List[str]:
        """Safely convert a list of objects to strings."""
        if not obj_list:
            return []

        result = []
        for item in obj_list:
            try:
                if isinstance(item, str):
                    result.append(item)
                elif hasattr(item, "id"):
                    result.append(item.id)
                elif hasattr(item, "name"):
                    result.append(item.name)
                else:
                    result.append(str(item))
            except Exception as e:
                self.logger.warning(f"Failed to convert item to string: {e}")
                result.append(str(item))
        return result

    def create_merged_csv_with_dependencies(
        self, output_filename: str = "merged_records.csv"
    ) -> None:
        """Create a merged CSV with dependency chain metadata (requires pandas)."""
        if not self.dependency_tracer.records:
            self.logger.warning("No records to merge.")
            return
        if (
            pd is None
        ):  # GOTCHA: pandas is optional; merge output silently impossible without it.
            self.logger.error("pandas not installed; cannot create merged CSV.")
            return

        # Get dependency chains for all records
        dependency_chains = self.dependency_tracer.find_all_dependency_chains()

        # Prepare data for CSV
        rows = []
        for record in self.dependency_tracer.records.values():
            chain_info = dependency_chains.get(record.id, {})

            # Get dependency summary
            dep_summary = self.dependency_tracer.get_dependency_summary_for_record(
                record.id
            )

            row = {
                # Core record information
                "record_id": record.id,
                "title": record.title,
                "alias": record.alias,
                "tags": record.tags,
                "collection_id": record.collection_id,
                # Relationship information
                "parent_records": ",".join(dep_summary.get("parent_records", [])),
                "child_records": ",".join(dep_summary.get("child_records", [])),
                "dependency_chain": " -> ".join(
                    chain_info.get("chain_from_root", [record.id])
                ),
                "root_record": chain_info.get("root_id", record.id),
                "depth_from_root": chain_info.get("depth_from_root", 0),
                # Flags
                "is_root": len(dep_summary.get("parent_records", [])) == 0,
                "is_leaf": len(dep_summary.get("child_records", [])) == 0,
            }

            # Add metadata fields
            row.update(record.metadata)
            rows.append(row)

        # Sort by dependency groups and depth for better readability
        rows.sort(key=lambda x: (x["root_record"], x["depth_from_root"]))

        # Save to CSV
        output_path = os.path.join(self.download_dir, output_filename)
        df = pd.DataFrame(rows)
        df.to_csv(output_path, index=False)

        self.logger.info(
            f"Created merged CSV with {len(rows)} records at {output_path}"
        )

    def _save_records_to_csv(self, records: List[DataFedRecord], filename: str) -> None:
        """Save records to CSV file."""
        if not records:
            return

        rows = []
        for record in records:
            # Convert dependencies to strings
            deps_str = []
            if record.dependencies:
                for dep in record.dependencies:
                    deps_str.append(str(dep))

            # Convert tags to strings
            tags_str = []
            if record.tags:
                for tag in record.tags:
                    tags_str.append(str(tag))

            row = {
                "record_id": record.id,
                "title": record.title,
                "alias": record.alias,
                "tags": ",".join(tags_str),
                "dependencies": ",".join(deps_str),
            }
            row.update(record.metadata)
            rows.append(row)

        df = pd.DataFrame(rows)
        df.to_csv(filename, index=False)


def create_config(filename: str) -> None:
    """Create DataFed configuration file."""
    print("Creating DataFed download configuration...")
    endpoint_uuid = input("Enter your Globus Endpoint UUID: ").strip()
    download_directory = (
        input("Enter your download directory path (e.g., Data/downloads/): ").strip()
        or "Data/downloads/"
    )

    # Validate download directory
    if not os.path.exists(download_directory):
        os.makedirs(download_directory)
        print(f"Created download directory at '{download_directory}'.")

    config = configparser.ConfigParser()
    config["DataFed Project Settings"] = {
        "globus_uuid": endpoint_uuid,
        "download_directory": download_directory,
    }

    with open(filename, "w") as configfile:
        config.write(configfile)
    print(f"Configuration file created at '{filename}'.")


def setup_logging(log_file: str) -> None:
    """Setup logging configuration."""
    os.makedirs(os.path.dirname(log_file), exist_ok=True)

    logging.basicConfig(
        filename=log_file,
        filemode="a",
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        level=logging.INFO,
    )

    # Console logging
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    formatter = logging.Formatter("%(levelname)s - %(message)s")
    console.setFormatter(formatter)
    logging.getLogger("").addHandler(console)


def load_query(query_file: str) -> Dict[str, Any]:
    """Load query configuration from JSON file."""
    if not os.path.exists(query_file):
        raise FileNotFoundError(f"Query file not found: {query_file}")

    with open(query_file, "r") as f:
        return json.load(f)


def is_dry_run():
    """Return True when dry-run mode active via env var or CLI flag attribute."""
    return os.environ.get(DRY_RUN_ENV) == "1" or getattr(is_dry_run, "_flag", False)


def run(argv=None):
    """CLI entry: configure logging, process collections & optionally export merged dependency CSV."""
    parser = argparse.ArgumentParser(
        description="Download and process DataFed records."
    )
    parser.add_argument(
        "--config",
        default="www/DataFedSettings.cfg",
        help="Path to the configuration file.",
    )
    parser.add_argument("--query", required=True, help="Path to the query file json.")
    parser.add_argument(
        "--merge-dependencies",
        action="store_true",
        help="Create merged CSV with dependency analysis.",
    )
    parser.add_argument(
        "--output", default="merged_records.csv", help="Output filename for merged CSV."
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Load config/query only; skip remote API calls",
    )
    args = parser.parse_args(argv)

    if args.dry_run:
        is_dry_run._flag = True  # type: ignore

    if not os.path.exists(args.config):
        print(f"No config file found at '{args.config}'.")
        create_config(args.config)
        print("Please update the configuration file and rerun the script.")
        return 2
    try:
        query = load_query(args.query)
        processor = DataFedProcessor(args.config)
        log_file = os.path.join(
            processor.download_dir, "logs", "datafed_processing.log"
        )
        setup_logging(log_file)
        processor.process_collections(
            query, dry_run=is_dry_run()
        )  # TODO: Potential high memory usage if many collections/records loaded simultaneously.
        if args.merge_dependencies and not is_dry_run():
            processor.create_merged_csv_with_dependencies(args.output)
        return 0
    except Exception as e:  # pragma: no cover
        logging.error(f"Application error: {e}")
        return 1


def main(argv=None, embedded=False):
    code = run(argv)
    if embedded or os.environ.get("ECOKMER_EMBEDDED") == "1":
        if code != 0:
            raise RuntimeError(f"record_query failed (code={code})")
    else:
        import sys

        sys.exit(code)


if __name__ == "__main__":  # pragma: no cover
    main()
