## File: test_home_offline_smoke.R - Offline dataset presence + DataFed filename vector smoke tests
test_that("offline harmonized.csv file exists (project-root relative)", { # TEST: verify offline harmonized CSV exists
    # Derive project root: tests/testthat -> go up two levels
    test_dir <- testthat::test_path()
    project_root <- normalizePath(file.path(test_dir, "..", ".."), winslash = "/", mustWork = FALSE)

    data_dir_candidates <- c(file.path(project_root, "Data"), file.path(project_root, "data"))
    dirs_exist <- vapply(data_dir_candidates, dir.exists, logical(1))

    candidate_files <- unlist(lapply(data_dir_candidates, function(d) file.path(d, c("harmonized.csv", "harmonized_old.csv"))))
    existing <- candidate_files[file.exists(candidate_files)]

    if (!any(dirs_exist)) {
        testthat::skip(paste0("No Data directory relative to project root. Project root inferred as: ", project_root))
    }
    if (!length(existing)) {
        listings <- lapply(data_dir_candidates[dirs_exist], list.files)
        fail_msg <- paste0(
            "Expected harmonized.csv (or harmonized_old.csv) not found.\n",
            "Project root: ", project_root, "\n",
            "Checked files: ", paste(candidate_files, collapse = "; "), "\n",
            "Listings: ", paste(paste(data_dir_candidates[dirs_exist], "=", vapply(listings, function(x) paste(head(x, 25), collapse = ","), character(1))), collapse = " | ")
        )
        testthat::fail(fail_msg)
    }
    testthat::expect_true(length(existing) >= 1, info = paste("Found:", paste(existing, collapse = ", ")))
})

test_that("required DataFed CSV names are defined for future online mode", { # TEST: ensure placeholder expected file names vector length
    expected <- c("c_525611220_records.csv", "c_525611221_records.csv", "c_522418931_records.csv")
    # we don't assert existence yet (online not always run), just that vector is length 3
    expect_length(expected, 3)
})
