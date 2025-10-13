## File: helper-load.R - Locate & source helper scripts (analysis/spatial/cleaning/date) for tests with dependency diagnostics
## TODO: Provide env var (e.g. ECO_TEST_SILENT=1) to suppress diagnostic messages.
## TODO: When project is package-ized, prefer devtools::load_all() instead of manual sourcing.
## TODO: Enforce minimum package versions (currently only presence is checked).

start_dir <- normalizePath(testthat::test_path(), mustWork = TRUE)
message("[helper-load] start_dir=", start_dir)
candidate_roots <- unique(c(
    start_dir,
    normalizePath(file.path(start_dir, ".."), mustWork = TRUE),
    normalizePath(file.path(start_dir, "..", ".."), mustWork = TRUE)
))
## GOTCHA: Upward search is fixed to three levels (testthat -> tests -> project root); deeper nesting won't be discovered.
message("[helper-load] candidate_roots=", paste(candidate_roots, collapse = " | "))

analysis_paths <- file.path(candidate_roots, "Helpers", "helper_fns_data_analysis.R")
spatial_paths <- file.path(candidate_roots, "Helpers", "helper_fns_spatial_map.R")
clean_paths <- file.path(candidate_roots, "Helpers", "helper_fns_cleaning.R")
## GOTCHA: Paths are hard-coded; adding new helper categories requires explicit inclusion here.
message("[helper-load] candidate_analysis=", paste(analysis_paths, collapse = " | "))
message("[helper-load] candidate_spatial=", paste(spatial_paths, collapse = " | "))
message("[helper-load] candidate_clean=", paste(clean_paths, collapse = " | "))

pick_first <- function(paths, label) { # GOTCHA: Hard fails (stop) on missing helper; consider testthat::skip() if optional
    f <- paths[file.exists(paths)]
    if (!length(f)) {
        stop("[helper-load] Missing required ", label, " helper. Looked in: ", paste(paths, collapse = ", "))
    }
    f[1]
}

analysis_file <- pick_first(analysis_paths, "analysis")
spatial_file <- pick_first(spatial_paths, "spatial")
clean_file <- pick_first(clean_paths, "cleaning")
message("[helper-load] using analysis helper: ", analysis_file)
message("[helper-load] using spatial helper: ", spatial_file)
message("[helper-load] using cleaning helper: ", clean_file)

# Optional additional helpers (e.g., newly added canonical utilities) -----------------
extra_helper_candidates <- file.path(candidate_roots, "Helpers", "parse_multi_date.R")
extra_helper <- extra_helper_candidates[file.exists(extra_helper_candidates)]
if (length(extra_helper)) {
    # Pick the first existing path (project root should appear before parent fallbacks)
    extra_helper <- extra_helper[1]
    message("[helper-load] using extra helper: ", extra_helper)
} else {
    message("[helper-load] no extra helper files detected (parse_multi_date.R not found along candidate roots)")
}
## GOTCHA: Only the single optional helper (parse_multi_date.R) is auto-detected; additional helpers must be manually added.

## Dependency diagnostics (do not install automatically; just inform):
needed_pkgs <- c("dplyr", "stringr", "tidyr", "purrr", "openxlsx", "ggplot2", "tibble") # TODO: Centralize dependency list (avoid drift vs production code)
missing <- needed_pkgs[!vapply(needed_pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1))]
if (length(missing)) {
    message("[helper-load] MISSING packages: ", paste(missing, collapse = ", "))
    message("[helper-load] Install with: install.packages(c(\"", paste(missing, collapse = '\", \"'), "\"))  OR run renv::restore() if using renv.")
} else {
    message("[helper-load] All required helper packages present.")
}
## GOTCHA: Version requirements are not validated; outdated packages could still cause subtle test failures.
## GOTCHA: Messages always printed (no silent mode yet) which can clutter CI logs.

## Source the first match (local=TRUE to avoid polluting global env during tests)
source(analysis_file, local = TRUE) # GOTCHA: local=TRUE prevents helpers from modifying .GlobalEnv options unexpectedly
source(spatial_file, local = TRUE)
source(clean_file, local = TRUE)
if (length(extra_helper)) source(extra_helper, local = TRUE) # TODO: Auto-detect and source future helper additions (glob pattern)
## GOTCHA: Sourcing order matters if helpers share symbol names; later files could mask earlier definitions silently.
message("[helper-load] helpers sourced successfully.")
