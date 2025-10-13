# File: test_datafed_upload_script_path_param.R - Guard against hardcoded paths & ensure new CLI param

test_that("datafed_csv_upload.py no longer contains hardcoded user path and exposes --reads-dir", { # TEST: verify removal of old absolute path & presence of parameter
    py_file <- testthat::test_path("..", "..", "Scripts", "datafed_csv_upload.py")
    expect_true(file.exists(py_file))
    content <- readLines(py_file, warn = FALSE)
    # Ensure the old absolute path is gone
    expect_false(any(grepl("/Users/phil294/", content, fixed = TRUE)))
    # Ensure the new argument is present
    expect_true(any(grepl("--reads-dir", content, fixed = TRUE)))
})
