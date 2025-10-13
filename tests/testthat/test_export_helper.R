# File: test_export_helper.R - Plotly export config button injection tests

test_that("add_export_config adds expected buttons", { # TEST: verifies SVG button added with correct filename
    expect_true(exists("add_export_config"), "add_export_config should be loaded via global sourcing")
    # Minimal plotly object
    plt <- plotly::plot_ly(x = 1:3, y = 1:3)
    out <- add_export_config(plt, fname = "test_file")
    # Basic structure expectations
    expect_true(is.list(out$x$config))
    expect_equal(out$x$config$toImageButtonOptions$filename, "test_file")
    added <- out$x$config$modeBarButtonsToAdd
    expect_true(length(added) > 0, "No buttons were added to modeBarButtonsToAdd")
    # Safely extract names handling possible atomic vector elements
    button_names <- unlist(lapply(added, function(b) {
        if (is.list(b) && !is.null(b$name)) {
            b$name
        } else if (is.character(b) && length(b) == 1) {
            b
        } else {
            NA_character_
        }
    }), use.names = FALSE)
    expect_true("Download SVG" %in% button_names)
    # Ensure click JavaScript references svg + filename
    click_js <- added[[which(button_names == "Download SVG")]]$click
    expect_true(grepl("Plotly.downloadImage", click_js))
    expect_true(grepl("svg", click_js, ignore.case = TRUE))
    expect_true(grepl("test_file", click_js))
})
