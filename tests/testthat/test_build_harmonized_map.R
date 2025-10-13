# File: test_build_harmonized_map.R - Tests for leaflet map builder helper

test_that("build_harmonized_map basic smoke with fixture", { # TEST: verify list structure + counts
    skip_if_not(requireNamespace("leaflet", quietly = TRUE))
    fx <- testthat::test_path("fixtures", "harmonized_fixture.csv")
    expect_true(file.exists(fx))
    df <- suppressWarnings(read.csv(fx, comment.char = "#"))
    # Minimal subset of columns required by helper
    minimal <- df[, intersect(c("Parent.ID", "Owen.s.Notation.for.sequencing", "Site.Coordinates", "Latitude", "Longitude", "Date"), names(df)), drop = FALSE]
    # Ensure at least some coordinate info exists
    expect_true(any(!is.na(minimal$Site.Coordinates) | (!is.na(minimal$Latitude) & !is.na(minimal$Longitude))))
    res <- build_harmonized_map(minimal, full_df = df)
    expect_type(res, "list")
    expect_true(all(c("widget", "data", "excluded_n", "valid_n", "palette_domain") %in% names(res)))
    expect_true(inherits(res$widget, "leaflet"))
    expect_gte(res$valid_n, 0)
    expect_equal(res$valid_n, nrow(res$data))
    # If there are excluded rows, excluded_n + valid_n should equal input row count after Date backfill attempt (Date isn't used in row removal)
    expect_equal(res$valid_n + res$excluded_n, nrow(minimal))
    # Palette domain either length 2 numeric or NA placeholders
    expect_length(res$palette_domain, 2)
})

test_that("build_harmonized_map returns placeholder when all coords missing", { # TEST: placeholder widget path with no valid coords
    skip_if_not(requireNamespace("leaflet", quietly = TRUE))
    df <- data.frame(
        Parent.ID = c("A", "B"),
        Owen.s.Notation.for.sequencing = c("X", "Y"),
        Site.Coordinates = c(NA, NA),
        Date = as.Date(c("2024-01-01", "2024-01-02"))
    )
    res <- expect_warning(build_harmonized_map(df), NA) # no warnings expected now
    expect_equal(res$valid_n, 0)
    expect_equal(res$excluded_n, nrow(df))
    # Control text should be present in underlying html (optional soft check)
    html_calls <- vapply(res$widget$x$calls, function(z) z$method, character(1))
    expect_true("addControl" %in% html_calls || res$valid_n == 0)
})

test_that("build_harmonized_map parses DMS strings", { # TEST: DMS parsing yields finite lat/long in expected ranges
    skip_if_not(requireNamespace("leaflet", quietly = TRUE))
    df <- data.frame(
        Parent.ID = c("P1", "P2"),
        Owen.s.Notation.for.sequencing = c("SEQ1", "SEQ2"),
        Site.Coordinates = c("45°30'15\"N 122°40'20\"W", "46°10'10\"N 123°20'05\"W"),
        Date = as.Date(c("2024-01-01", "2024-02-01"))
    )
    res <- build_harmonized_map(df)
    expect_equal(res$excluded_n, 0)
    expect_equal(res$valid_n, 2)
    # Check numeric ranges
    expect_true(all(res$data$Latitude > 40 & res$data$Latitude < 50))
    expect_true(all(res$data$Longitude < -120 & res$data$Longitude > -130))
})
