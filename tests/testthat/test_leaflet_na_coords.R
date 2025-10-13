## File: test_leaflet_na_coords.R - Leaflet rendering with NA coordinates

test_that("leaflet map builds with NA coordinates present", { # TEST: NA coordinates skipped without failure
    skip_if_not(requireNamespace("leaflet", quietly = TRUE), "leaflet package required")
    expect_true(exists("process_geographic_location2"))
    dat <- data.frame(
        Parent.ID = c("P1", "P2", "P3"),
        Owen.s.Notation.for.sequencing = c("SEQ1", "SEQ2", "SEQ3"),
        Site.Coordinates = c("45°30'15\"N 122°40'20\"W", NA, "46°10'10\"N 123°20'05\"W"),
        Date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
    )
    proc <- dat %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            coords = if (!is.na(Site.Coordinates)) list(process_geographic_location2(Site.Coordinates)) else list(list(latitude = NA, longitude = NA)),
            Latitude = coords$latitude,
            Longitude = coords$longitude,
            sample = paste0(Parent.ID, ": ", Owen.s.Notation.for.sequencing),
            DateNumeric = as.numeric(Date)
        ) %>%
        dplyr::ungroup()

    pal <- leaflet::colorNumeric("YlOrRd", domain = range(proc$DateNumeric, na.rm = TRUE), na.color = "transparent")
    mp <- leaflet::leaflet(proc) %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(~Longitude, ~Latitude, layerId = ~sample, color = ~ pal(DateNumeric), fillColor = ~ pal(DateNumeric))

    expect_true(inherits(mp, "leaflet"))
    methods <- vapply(mp$x$calls, function(z) z$method, character(1))
    expect_true(any(grepl("addCircleMarker", methods)))
})
