# File: testthat.R - Orchestrates running test suite (package-aware fallback to test_dir)

if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package 'testthat' is required to run tests.")
}

if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for app-related tests.")
}

# Load app dependencies (simulate sourcing global if needed)
if (file.exists("global.R")) source("global.R")

pkg_name <- "ecokmer"
if (pkg_name %in% rownames(installed.packages())) {
    testthat::test_check(pkg_name)
} else {
    # Fallback: run tests directly from directory without package install
    message("Package '", pkg_name, "' not installed; running tests via test_dir().")
    testthat::test_dir("tests/testthat")
}
