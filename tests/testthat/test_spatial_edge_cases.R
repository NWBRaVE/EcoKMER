## File: test_spatial_edge_cases.R - Spatial helper malformed input edge cases

test_that("process_geographic_location2 returns NA list on malformed string", { # TEST: malformed DMS now tolerant (no error)
    expect_true(exists("process_geographic_location2"))
    res <- process_geographic_location2("bad-format")
    expect_true(is.list(res))
    expect_true(is.na(res$latitude) && is.na(res$longitude))
})

test_that("process_geographic_location gracefully returns NA list for blank", { # TEST: blank/NA returns NA lat/lon
    expect_true(exists("process_geographic_location"))
    res <- process_geographic_location(NA)
    expect_true(is.na(res$latitude) && is.na(res$longitude))
    res2 <- process_geographic_location("")
    expect_true(is.na(res2$latitude) && is.na(res2$longitude))
})

test_that("process_geographic_location tolerates lowercase n/a sentinel", { # TEST: 'n/a' sentinel yields NA
    res <- process_geographic_location_lat_long("n/a", "latitude")
    expect_true(is.na(res))
})
