## File: test_process_data.R - Core process_data behaviors (date filter, NA->0, log1p, grouping negation)

test_that("process_data basic pipeline works with csv and config list", { # TEST: end-to-end CSV pipeline including filters & transforms
    expect_true(exists("process_data"), "process_data should be loaded by helper-load.R")

    # fabricate temporary CSV
    tmpdir <- tempdir()
    csv_path <- file.path(tmpdir, "mini.csv")
    df <- data.frame(
        Sample.ID = c("S1", "S2", "S3", "S4"),
        GroupA = c("X", "Y", "Y", "Z"),
        GroupB = c("keep", "drop", "keep", "keep"),
        metric1 = c(NA, "N/A", 5, 10),
        metric2 = c(1, 2, 3, 4),
        when = c("2024-01-01", "2024-01-05", "2024-02-01", "2024-03-01"),
        stringsAsFactors = FALSE
    )
    write.csv(df, csv_path, row.names = FALSE)

    config <- list(
        filepath = tmpdir,
        filename = basename(csv_path),
        sheetname = NULL,
        lineskips = 1,
        sample_column = "Sample.ID",
        grouping_information = list(
            column_name = c("GroupA", "GroupB"),
            values = list(c("X", "Y"), c("!drop")) # negation for GroupB
        ),
        y_values = c("metric1", "metric2"),
        log1p_y_values = c("metric2"),
        date_information = list(
            column_name = "when",
            dates_range = c("2024-01-01", "2024-02-15"),
            date_format = "%Y-%m-%d"
        )
    )

    res <- process_data(config)

    # Date filtering removed row beyond range (2024-03-01)
    expect_true(!"2024-03-01" %in% format(res$when, "%Y-%m-%d"))
    # GroupB removed 'drop'
    expect_false("drop" %in% unique(attr(res, "all_columns")$GroupB))
    # NA and "N/A" replaced with 0 for metric1
    expect_true(all(res$metric1 %in% c(0, 5, 10)))
    # log1p transform applied to metric2 (check first value changed from 1 -> log1p(1)=~0.693)
    expect_gt(0.71, res$metric2[1])
    expect_lt(0.69, res$metric2[1])
})
