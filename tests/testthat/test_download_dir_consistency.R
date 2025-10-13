# File: test_download_dir_consistency.R - Enforces canonical Data/downloads casing across source files

test_that("download directory casing is consistent", { # TEST: ensures no lowercase data/downloads references remain
    canonical <- "Data/downloads"
    expect_true(dir.exists(canonical) || dir.create(canonical, recursive = TRUE))

    # Files to scan: R and Python source under project root (exclude logs, renv, .Rproj.user, tests output)
    root <- here::here()
    patterns <- c("*.R", "*.r", "*.py")
    files <- unlist(lapply(patterns, function(p) list.files(root, pattern = glob2rx(p), recursive = TRUE, full.names = TRUE)))

    # Exclusions (dirs + this test file itself)
    exclude_dirs <- c("renv", "Data/downloads/logs", "tests/testthat/_snaps")
    this_file <- normalizePath(file.path(root, "tests/testthat/test_download_dir_consistency.R"), mustWork = FALSE)
    files <- files[!vapply(files, function(f) {
        nf <- normalizePath(f, mustWork = FALSE)
        is_self <- nf == this_file
        in_ex_dir <- any(grepl(paste0("^", normalizePath(file.path(root, exclude_dirs)), collapse = "|"), nf))
        is_self || in_ex_dir
    }, logical(1), USE.NAMES = FALSE)]

    # Read and check each file for the lowercase pattern 'data/downloads' (not preceded by a letter to avoid false positives in URLs)
    bad <- character(0)
    for (f in files) {
        txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
        # Pattern: start or non-letter before 'data/downloads' to reduce false positives.
        if (any(grepl("(^|[^A-Za-z])data/downloads", txt))) {
            bad <- c(bad, f)
        }
    }

    if (length(bad) > 0) {
        fail(paste0("Lowercase 'data/downloads' found in: ", paste(bad, collapse = ", ")))
    } else {
        succeed()
    }
})
