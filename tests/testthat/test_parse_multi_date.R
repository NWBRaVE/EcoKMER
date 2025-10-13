# File: test_parse_multi_date.R - Multi-format date parsing tests

test_that("parse_multi_date parses multiple common formats", { # TEST: core supported formats parsed to ISO strings
    x <- c(
        "2024-07-15", # %Y-%m-%d
        "07/16/2024", # %m/%d/%Y
        "2024/07/17", # %Y/%m/%d
        "18-Jul-2024", # %d-%b-%Y
        "19-07-2024", # %d-%m-%Y
        "07-20-2024", # %m-%d-%Y
        "not-a-date" # invalid
    )
    res <- parse_multi_date(x)
    expect_s3_class(res, "Date")
    expect_equal(length(res), length(x))
    # Check specific known parses
    expect_equal(as.character(res[1]), "2024-07-15")
    expect_equal(as.character(res[2]), "2024-07-16")
    expect_equal(as.character(res[3]), "2024-07-17")
    expect_equal(as.character(res[4]), "2024-07-18")
    expect_equal(as.character(res[5]), "2024-07-19")
    expect_equal(as.character(res[6]), "2024-07-20")
    expect_true(is.na(res[7]))
})

test_that("parse_multi_date is idempotent for Date input", { # TEST: Date input returned unchanged
    d <- as.Date(c("2024-01-01", "2024-02-02"))
    out <- parse_multi_date(d)
    expect_identical(out, d)
})

test_that("parse_multi_date handles factor input", { # TEST: factor coercion path works
    f <- factor(c("2024-03-01", "2024/03/02"))
    out <- parse_multi_date(f)
    expect_equal(as.character(out[1]), "2024-03-01")
    expect_equal(as.character(out[2]), "2024-03-02")
})

test_that("parse_multi_date returns NA for entirely unknown strings", { # TEST: unknown tokens yield all NA
    x <- c("xxx", "yyy")
    out <- parse_multi_date(x)
    expect_true(all(is.na(out)))
})
