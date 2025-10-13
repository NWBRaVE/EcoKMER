## File: test_fixture_plots_and_map.R - Fixture-driven smoke tests for plots + spatial helpers
## TODO: Consider snapshot tests for plot structure if stability desired.

fixture_path <- testthat::test_path("fixtures", "harmonized_fixture.csv")

make_config_from_fixture <- function(csv) { # HELPER: derive minimal config list from fixture header
    stopifnot(file.exists(csv))
    # Read just header (fast) to detect any DNA yield style column present
    hdr <- names(utils::read.csv(csv, nrows = 0, comment.char = "#"))
    yield_candidates <- grep("DNA.Yield", hdr, value = TRUE)
    # Pick first non-empty candidate, fallback to first column if none (will fail fast revealing mismatch)
    y_col <- if (length(yield_candidates)) yield_candidates[1] else hdr[1]
    # We'll read a small sample of the file to infer grouping values.
    sample_df <- try(utils::read.csv(csv, comment.char = "#", nrows = 50), silent = TRUE)
    grouping_col <- "Owen.s.Notation.for.sequencing"
    grouping_vals <- if (!inherits(sample_df, "try-error") && grouping_col %in% names(sample_df)) {
        unique(na.omit(sample_df[[grouping_col]]))
    } else {
        character()
    }
    list(
        filepath = dirname(csv),
        filename = basename(csv),
        sheetname = NULL,
        lineskips = 1,
        sample_column = "Parent.ID", # fixture uses Parent.ID instead of Sample.ID
        grouping_information = list(
            column_name = c(grouping_col),
            values = list(grouping_vals) # explicit values to retain rows
        ),
        y_values = c(y_col),
        log1p_y_values = character(),
        date_information = list(
            column_name = "Date", # present in fixture tail columns
            dates_range = character(),
            date_format = "%Y-%m-%d"
        )
    )
}

load_processed_fixture <- function() { # HELPER: process fixture with derived config
    cfg <- make_config_from_fixture(fixture_path)
    dat <- process_data(cfg)
    # Guard: ensure at least two predictor levels for boxplot to produce a plot (if fixture contains both)
    dat
}

test_that("fixture boxplot_data returns list structure", { # TEST: boxplot_data returns data.frame with non-null plots
    expect_true(file.exists(fixture_path), sprintf("Expected fixture at %s", fixture_path))
    pdat <- load_processed_fixture()
    expect_true(exists("boxplot_data"))
    res <- boxplot_data(pdat)
    expect_true(is.data.frame(res))
    expect_true(all(c("Category", "boxplot_plot") %in% names(res)))
    # Accept mixture of NULL / ggplot objects; just ensure vector present
    # If every element NULL, surface informative failure with counts of grouping levels
    non_null <- Filter(Negate(is.null), res$boxplot_plot)
    expect_gt(length(non_null), 0, sprintf("All boxplot entries NULL; grouping levels seen: %s", paste(unique(pdat[[attr(pdat, "processed_info")$grouping_info$column_name]]), collapse = ",")))
})

test_that("process_geographic_location2 handles basic DMS strings", { # TEST: DMS parsing wrapper returns finite lat/lon
    expect_true(exists("process_geographic_location2"))
    ex <- process_geographic_location2("45°30'15\"N 122°40'20\"W")
    expect_type(ex$latitude, "double")
    expect_type(ex$longitude, "double")
    expect_true(ex$latitude > 0)
    expect_true(ex$longitude < 0)
})

test_that("leaflet layering pipeline executes with minimal data", { # TEST: end-to-end minimal leaflet layering succeeds
    expect_true(requireNamespace("leaflet", quietly = TRUE), "Package 'leaflet' must be installed for this test")
    # Build minimal frame mimicking processed_data2 in harmonized_analysis_rm.R
    minimal <- data.frame(
        Parent.ID = c("P1", "P2"),
        Owen.s.Notation.for.sequencing = c("SEQ1", "SEQ2"),
        Site.Coordinates = c("45°30'15\"N 122°40'20\"W", "46°10'10\"N 123°20'05\"W"),
        Date = as.Date(c("2024-01-01", "2024-02-01"))
    )
    proc <- minimal %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            coords = list(process_geographic_location2(Site.Coordinates)),
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
    # Verify a circle marker adding call exists
    raw_calls <- mp$x$calls
    calls <- if (length(raw_calls)) vapply(raw_calls, function(z) z$method, character(1)) else character()
    expect_true(length(calls) > 0, "Leaflet widget has no recorded calls")
    expect_true(any(grepl("addCircleMarker", calls)), paste0("Expected a circle marker adding call, saw: ", paste(calls, collapse = ", ")))
})
