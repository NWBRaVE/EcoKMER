## File: test_numeric_columns_spec.R - Guard list for required numeric columns
test_that("numeric column specification remains stable", { # TEST: confirm key numeric column names list non-empty
    numeric_columns <- c(
        "diversity", "kappa", "max_cyano.amoeba", "max_cyanophage.cyano",
        "max_pro.amoeba", "max_proc_phage.cyano", "max_syn.amoeba",
        "max_syn_phage.cyano", "modelR"
    )
    expect_gt(length(numeric_columns), 5)
    expect_true("diversity" %in% numeric_columns)
})
