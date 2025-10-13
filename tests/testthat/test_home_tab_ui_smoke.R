## File: test_home_tab_ui_smoke.R - UI build and ID presence for home_tab()

test_that("home_tab builds and contains expected core elements", { # TEST: ensures required input/output IDs present in rendered HTML
    if (!exists("home_tab")) {
        testthat::skip("home_tab() not available in test environment")
    }
    ui <- NULL
    expect_silent({
        ui <- home_tab()
    })
    html <- htmltools::renderTags(ui)$html

    # Helper to count non-overlapping matches
    count_matches <- function(pattern, x) {
        m <- gregexpr(pattern, x, perl = TRUE)[[1]]
        if (identical(m, -1L)) 0L else length(m)
    }

    expect_true(grepl('id=\"data_mode\"', html), "data_mode input missing")
    expect_equal(count_matches('id=\"pull_records\"', html), 1L, info = "pull_records button should appear exactly once")

    for (id in c("data_source_panel", "connection_status_badge", "dataset_summary")) {
        pattern <- paste0('id=\"', id, '\"')
        expect_true(grepl(pattern, html), info = paste("Missing UI placeholder for", id))
    }
})
