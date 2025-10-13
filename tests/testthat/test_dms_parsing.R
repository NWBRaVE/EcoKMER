## File: test_dms_parsing.R - Unified DMS parsing helper tests

test_that("dms_coord_to_decimal parses full DMS with direction", { # TEST: full DMS with direction sign handling
    expect_equal(round(dms_coord_to_decimal("45°30'00\"N"), 5), 45.5)
    expect_equal(round(dms_coord_to_decimal("45 30 0 S"), 5), -45.5)
})

test_that("dms_coord_to_decimal tolerates missing seconds", { # TEST: missing seconds defaults to 0
    expect_equal(dms_coord_to_decimal("10°15'N"), 10 + 15 / 60)
})

test_that("parse_dms_pair returns list with lat/lon", { # TEST: pair parser returns named numeric list
    res <- parse_dms_pair("45°30'00\"N 122°40'00\"W")
    expect_named(res, c("latitude", "longitude"))
    expect_true(is.finite(res$latitude))
    expect_true(is.finite(res$longitude))
    expect_true(res$longitude < 0)
})

test_that("parse_dms_vector vectorizes input", { # TEST: vector parser returns 2-row data.frame
    df <- parse_dms_vector(c("45°0'0\"N 10°0'0\"E", "46°0'0\"N 11°0'0\"E"))
    expect_equal(nrow(df), 2)
    expect_equal(names(df), c("Latitude", "Longitude"))
    expect_true(all(df$Latitude > 40))
})
