suppressPackageStartupMessages({
   ## Purpose: central package load (quiet). Gotcha: keep minimal; add libs here to avoid repeated shiny load warnings.
   library(shiny)
   library(shinyjs)
   library(shinyBS)
   library(shinyWidgets)
   library(shinycssloaders)
   library(shinythemes)
   library(shinyalert)
   library(prompter)
   library(shinydashboardPlus)
   library(shinyFiles)
   library(shinyjqui)
   library(rintrojs)
   library(shinybusy)
   library(shiny.blueprint)

   # JSON file creation
   library(jsonlite)

   # spatial map
   library(leaflet)
   library(leaflet.extras)

   # data processing
   library(dplyr)
   library(readxl)
   library(purrr)
   library(plotly)
   library(stringr)
   library(jsonlite)
   library(DT)

   # for python scripts
   library(reticulate)
})

## Centralized constants -------------------------------------------------------
# Central location for brittle DataFed record CSV paths (online mode). Consolidating
# here reduces scattered hard-coded literals and makes future refactors (e.g.,
# manifest-based discovery, config injection, dynamic collection IDs) simpler.
# NOTE: Do not mutate this list at runtime; treat as read-only. If collection IDs
# change upstream, update here and corresponding tests referencing expected names.
DATAFED_RECORD_PATHS <- list(
   ext = "./Data/downloads/c_525611220_records.csv",
   seq = "./Data/downloads/c_525611221_records.csv",
   par = "./Data/downloads/c_522418931_records.csv"
)

DATAFED_RECORD_BASENAMES <- vapply(DATAFED_RECORD_PATHS, basename, character(1))


## Reticulate: allow auto-detect; explicitly configure only if multiple envs cause mismatch.

# Source helper + static UI fragments
file_loads <- c(
   list.files("./Helpers", recursive = T, full.names = T),
   list.files("./Frontend_Static_UI", recursive = T, full.names = T)
)
for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)

# Ensure export helpers always available in global env for tests referencing add_export_config
exp_helper_path <- "./Helpers/export_helpers.R"
if (file.exists(exp_helper_path)) {
   source(exp_helper_path, local = FALSE)
}

## Python environment status (non-fatal): store for later UI / logic decisions
if (exists("py_env_status")) {
   py_status <- tryCatch(py_env_status(), error = function(e) list(message = paste("python-env status error:", e$message)))
   options(ecokmer.py_status = py_status)
   message("[python-env] ", py_status$message)
} else {
   message("[python-env] helper not loaded; skipping status check")
}
