## File: test_plot_helpers.R - Plot helper behavior & edge cases

make_processed <- function() { # HELPER: fabricate minimal processed_data-like object
    # synthetic minimal processed structure mimicking process_data output + attributes
    set.seed(1)
    df <- data.frame(
        Sample.ID = paste0("S", 1:6),
        GroupA = rep(c("G1", "G2"), each = 3),
        GroupB = c("A", "A", "B", "A", "B", "B"),
        metric1 = c(0, 5, 2, 4, 8, 1),
        metric2 = c(1, 2, 3, 4, 5, 6),
        when = as.Date("2024-01-01") + c(0, 1, 2, 30, 31, 40),
        stringsAsFactors = FALSE
    )
    attr(df, "config_info") <- list(dummy = TRUE)
    attr(df, "processed_info") <- list(
        grouping_info = list(column_name = c("GroupA", "GroupB")),
        grouping_values = list(c("G1", "G2"), c("A", "B")),
        y_values_mn = c("metric1", "metric2"),
        date_cname_mn = "when"
    )
    df
}

test_that("boxplot_data returns plots for multi-value groups", { # TEST: boxplot_data yields at least one ggplot when multi-level
    expect_true(exists("boxplot_data"), "boxplot_data should be loaded")
    pdat <- make_processed()
    res <- boxplot_data(pdat)
    expect_true("Category" %in% names(res))
    # At least one non-NULL ggplot object
    some_plot <- Filter(Negate(is.null), res$boxplot_plot)
    expect_gt(length(some_plot), 0)
})

test_that("lineplot_data returns plots and supports >1 predictor values", { # TEST: lineplot_data produces some plots
    expect_true(exists("lineplot_data"), "lineplot_data should be loaded")
    pdat <- make_processed()
    res <- lineplot_data(pdat)
    some_plot <- Filter(Negate(is.null), res$lineplot_plot)
    expect_gt(length(some_plot), 0)
})

test_that("boxplot_data returns NULL plot when only one predictor value", { # TEST: single-level grouping yields NULL plots
    expect_true(exists("boxplot_data"))
    single <- make_processed()
    single$GroupA <- "Only" # collapse to single level
    attr(single, "processed_info")$grouping_info$column_name <- c("GroupA")
    attr(single, "processed_info")$grouping_values <- list(c("Only"))
    single$GroupB <- NULL
    res <- boxplot_data(single)
    # Expect all plots NULL because only one unique value
    expect_true(all(vapply(res$boxplot_plot, is.null, logical(1))))
})

test_that("diversity_plot produces ggplot object", { # TEST: diversity_plot returns ggplot for multi-sequence input
    expect_true(exists("diversity_plot"))
    dat <- data.frame(
        Owen.s.Notation.for.sequencing = rep(c("SEQ1", "SEQ2"), each = 5),
        grouping = rep(c("A", "B"), times = 5),
        diversity = runif(10, 15, 20)
    )
    gp <- diversity_plot(dat, sequences = c("SEQ1", "SEQ2"), grouping_var = "grouping", file_name = "_ignore")
    expect_true("ggplot" %in% class(gp))
})

test_that("mult_cols_plot returns ggplot object with facets", { # TEST: mult_cols_plot returns faceted ggplot
    expect_true(exists("mult_cols_plot"))
    dat <- data.frame(
        Owen.s.Notation.for.sequencing = rep(c("SEQ1", "SEQ2"), each = 5),
        grouping = rep(c("A", "B"), times = 5),
        diversity = runif(10, 15, 20),
        kappa = runif(10, 0, 1)
    )
    gp <- mult_cols_plot(dat,
        sequences = c("SEQ1", "SEQ2"), grouping_var = "grouping",
        mult_cols = c("diversity", "kappa"), file_name = "_ignore"
    )
    expect_true("ggplot" %in% class(gp))
})
