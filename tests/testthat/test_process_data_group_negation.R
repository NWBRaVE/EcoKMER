## File: test_process_data_group_negation.R - Grouping negation (!value) filter logic

test_that("grouping negation excludes specified values", { # TEST: negated grouping values excluded
    expect_true(exists("process_data"))
    tmpdir <- tempdir()
    csv_path <- file.path(tmpdir, "negation.csv")
    df <- data.frame(
        Sample = paste0("S", 1:6),
        Group = c("keep", "drop", "keep", "other", "drop", "keep"),
        metric = c(1, 2, 3, 4, 5, 6),
        when = rep("2024-01-01", 6),
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
            column_name = c("Group"),
            values = list(c("!drop"))
        ),
        y_values = c("metric"),
        log1p_y_values = character(),
        date_information = list(
            column_name = "when",
            dates_range = character(),
            date_format = "%Y-%m-%d"
        )
    )

    res <- process_data(cfg)
    kept_levels <- unique(res$Group)
    expect_false("drop" %in% kept_levels)
    expect_true(all(kept_levels %in% c("keep", "other")))
})
