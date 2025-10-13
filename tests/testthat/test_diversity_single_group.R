## File: test_diversity_single_group.R - Diversity plot single-group behavior

test_that("diversity_plot works with single observations per group", { # TEST: single-group produces ggplot object
    expect_true(exists("diversity_plot"))
    dat <- data.frame(
        Owen.s.Notation.for.sequencing = c("SEQ1", "SEQ1", "SEQ2", "SEQ2"),
        grouping = c("G1", "G2", "G1", "G2"),
        diversity = c(18.5, 19.0, 17.2, 16.8)
    )
    # Collapse one sequence to single observation per grouping by subsetting
    dat_single <- dat[dat$Owen.s.Notation.for.sequencing == "SEQ1", , drop = FALSE]
    gp <- diversity_plot(dat_single, sequences = c("SEQ1"), grouping_var = "grouping", file_name = "_ignore")
    expect_true("ggplot" %in% class(gp))
})
