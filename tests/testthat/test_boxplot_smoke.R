# File: test_boxplot_smoke.R - Smoke tests for boxplot-related fallback and pivot logic

test_that("harmonized fixture loads and categorical fallback conditions detectable", { # TEST: detect non-numeric fallback path using fixture
    skip_on_cran()
    # Use dedicated fixture (subset) for stable schema & deterministic row count
    fixture_path <- testthat::test_path("fixtures", "harmonized_fixture.csv")
    expect_true(file.exists(fixture_path), info = paste("Missing fixture:", fixture_path))
    df <- suppressWarnings(read.csv(fixture_path, comment.char = "#"))
    expect_gt(ncol(df), 0)
    # Identify character-only candidate columns (simulate user picking only non-numeric vars)
    char_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    expect_true(length(char_cols) > 0)
    # Simulate fallback trigger: selecting first char col as measure/x
    grouping_var <- char_cols[1]
    x_axis_groups <- char_cols[1]
    pivot_cols <- setdiff(grouping_var, x_axis_groups)
    if (!length(pivot_cols)) {
        exp_long <- df %>%
            dplyr::mutate(.synthetic = .data[[x_axis_groups]]) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(".synthetic"), values_to = "Value", names_to = "Columns_of_Interest") %>%
            dplyr::mutate(Columns_of_Interest = x_axis_groups)
    }
    # Assert non-numeric path would be taken
    expect_false(is.numeric(exp_long$Value))
    expect_equal(unique(exp_long$Columns_of_Interest), x_axis_groups)
})

test_that("boxplot helper pivot logic handles overlap", { # TEST: ensure synthetic pivot works when grouping==x axis
    skip_on_cran()
    # Simulate the pivot logic for overlapping x and grouping var
    df <- data.frame(A = letters[1:5], B = letters[1:5], stringsAsFactors = FALSE)
    grouping_var <- c("A")
    x_axis_groups <- "A"
    pivot_cols <- setdiff(grouping_var, x_axis_groups)
    if (!length(pivot_cols)) {
        exp_long <- df %>%
            dplyr::mutate(.synthetic = !!rlang::sym(x_axis_groups)) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(".synthetic"), values_to = "Value", names_to = "Columns_of_Interest") %>%
            dplyr::mutate(Columns_of_Interest = x_axis_groups)
    }
    expect_equal(unique(exp_long$Columns_of_Interest), x_axis_groups)
    expect_equal(nrow(exp_long), nrow(df))
})
