## File: test_process_data_errors.R - Defensive / error branch coverage for process_data

make_base_cfg <- function() { # HELPER: baseline config list for error manipulation
    fp <- testthat::test_path("fixtures", "harmonized_fixture.csv")
    list(
        filepath = dirname(fp),
        filename = basename(fp),
        sheetname = NULL,
        lineskips = 1,
        sample_column = "Parent.ID",
        grouping_information = list(
            column_name = c("Owen.s.Notation.for.sequencing"),
            values = list(character())
        ),
        y_values = character(),
        log1p_y_values = character(),
        date_information = list(
            column_name = "Date",
            dates_range = character(),
            date_format = "%Y-%m-%d"
        )
    )
}

test_that("rejects unsupported file extension", { # TEST: invalid file extension error message
    cfg <- make_base_cfg()
    cfg$filename <- "bad.txt"
    expect_error(process_data(cfg), "xlsx or csv", ignore.case = TRUE)
})

test_that("error when grouping column missing", { # TEST: missing grouping column triggers informative error
    cfg <- make_base_cfg()
    # Build a clean derivative CSV with the target grouping column removed.
    fp <- file.path(cfg$filepath, cfg$filename)
    df <- utils::read.csv(fp, comment.char = "#", stringsAsFactors = FALSE)
    expect_true("Owen.s.Notation.for.sequencing" %in% names(df))
    df$Owen.s.Notation.for.sequencing <- NULL
    tmp <- tempfile(fileext = ".csv")
    utils::write.csv(df, tmp, row.names = FALSE)
    cfg$filepath <- dirname(tmp)
    cfg$filename <- basename(tmp)
    # Manually capture the error to avoid any edition-specific expect_error quirks
    err <- tryCatch(
        {
            process_data(cfg)
            NULL
        },
        error = function(e) e
    )
    expect_false(is.null(err), "Expected an error when grouping column is missing")
    msg <- conditionMessage(err)
    # Rather than rely on the fragile parentheses pattern, assert presence of key tokens.
    has_grouping <- grepl("Grouping", msg, ignore.case = TRUE)
    has_missing <- grepl("missing", msg, ignore.case = TRUE)
    has_in_data <- grepl("in data", msg, ignore.case = TRUE)
    expect_true(
        all(c(has_grouping, has_missing, has_in_data)),
        paste0("Unexpected error message tokens missing: ", msg)
    )
})
