# File: test_na_cleaning.R - Sentinel replacement cleaning tests

test_that("clean_string_sentinels converts sentinels to NA for char and factor", { # TEST: sentinel tokens converted to NA in char & factor columns
    df <- data.frame(
        char_col = c("A", "BLANK", "n/a", "keep", "Too low", "BD", "AB"),
        stringsAsFactors = FALSE
    )
    df$fact_col <- factor(c("X", "NA", "Blank", "Y", "N/A", "Z", "BLANK"))
    cleaned <- clean_string_sentinels(df)
    # char column expectations
    expect_true(is.na(cleaned$char_col[2]))
    expect_true(is.na(cleaned$char_col[3]))
    expect_true(is.na(cleaned$char_col[5]))
    expect_true(is.na(cleaned$char_col[6]))
    expect_true(is.na(cleaned$char_col[7]))
    # factor column expectations
    expect_true(is.na(cleaned$fact_col[2]))
    expect_true(is.na(cleaned$fact_col[3]))
    expect_true(is.na(cleaned$fact_col[5]))
    expect_true(is.na(cleaned$fact_col[7]))
})
