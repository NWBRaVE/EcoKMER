## File: test_process_data_date_range.R - Inclusive date filtering boundaries

test_that("date range filtering is inclusive of endpoints", { # TEST: endpoints included in filtered result
    expect_true(exists("process_data"))
    tmpdir <- tempdir()
    csv_path <- file.path(tmpdir, "daterange.csv")
    df <- data.frame(
        Sample = paste0("S", 1:10),
        Group1 = rep(c("A", "B"), each = 5),
        metric = 1:10,
        when = as.character(as.Date("2024-01-01") + 0:9),
        stringsAsFactors = FALSE
    )
    write.csv(df, csv_path, row.names = FALSE)

    cfg <- list(
        filepath = tmpdir,
        filename = basename(csv_path),
        sheetname = NULL,
        lineskips = 1,
        sample_column = "Sample",
        grouping_information = list(
            column_name = c("Group1"),
            values = list(c("A", "B"))
        ),
        y_values = c("metric"),
        log1p_y_values = character(),
        date_information = list(
            column_name = "when",
            dates_range = c("2024-01-03", "2024-01-08"),
            date_format = "%Y-%m-%d"
        )
    )

    res <- process_data(cfg)
    got <- sort(unique(format(res$when, "%Y-%m-%d")))
    expect_identical(got[1], "2024-01-03")
    expect_identical(tail(got, 1), "2024-01-08")
    expect_true(all(got %in% as.character(seq(as.Date("2024-01-03"), as.Date("2024-01-08"), by = "day"))))
})
