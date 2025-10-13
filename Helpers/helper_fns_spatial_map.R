## File: helper_fns_spatial_map.R - Spatial map & coordinate parsing helpers
## TODO: Consolidate legacy DMS functions; deprecate older process_geographic_location* variants after UI migration.
utils::globalVariables(c("parse_multi_date"))

# Unified DMS parsing utilities (canonical) ------------------------------------
# These supersede earlier duplicated helpers (dms_to_dd/dms_to_dd_map2/ process_geographic_location* ).
# Legacy functions are left in place below for backward compatibility but new code
# should rely on: dms_coord_to_decimal(), parse_dms_pair(), parse_dms_vector().

if (!exists("dms_coord_to_decimal", inherits = FALSE)) {
   dms_coord_to_decimal <- function(x, direction = NULL, warn = FALSE) { # FUNCTION: parse single DMS coordinate (flexible parts) to decimal
      if (is.null(x) || is.na(x) || !nzchar(x)) {
         return(NA_real_)
      }
      # Normalize any unicode prime/double-prime or curly quotes to standard ASCII ' and "
      x <- gsub("[\u2032\u2019]", "'", x) # various prime / apostrophe forms
      x <- gsub("[\u2033\u201D]", "\"", x) # double-prime / curly double quote
      x <- trimws(x)
      # Auto-extract trailing N/S/E/W if not explicitly provided
      if (is.null(direction)) {
         m <- regexec("^(.+?)([NSEW])$", x, ignore.case = TRUE)
         g <- regmatches(x, m)[[1]]
         if (length(g) == 3) {
            x <- trimws(g[2])
            direction <- toupper(g[3])
         }
      } else {
         direction <- toupper(direction)
      }
      parts <- unlist(strsplit(x, "[°'\" ]+"))
      parts <- parts[nzchar(parts)]
      if (!length(parts)) {
         return(NA_real_)
      }
      # Allow formats missing seconds or minutes+seconds
      if (length(parts) == 1) parts <- c(parts, "0", "0")
      if (length(parts) == 2) parts <- c(parts, "0")
      suppressWarnings(nums <- as.numeric(parts[1:3]))
      if (any(is.na(nums))) {
         if (warn) warning("Non-numeric DMS component in '", x, "'")
         return(NA_real_)
      }
      dd <- nums[1] + nums[2] / 60 + nums[3] / 3600
      if (!is.null(direction) && direction %in% c("S", "W")) dd <- -dd
      dd
   }
}

if (!exists("parse_dms_pair", inherits = FALSE)) {
   parse_dms_pair <- function(location, warn = FALSE) { # FUNCTION: parse "lat lon" DMS pair into numeric list
      if (is.null(location) || is.na(location) || !nzchar(trimws(location))) {
         return(list(latitude = NA_real_, longitude = NA_real_))
      }
      # Unicode normalization for primes/quotes
      location <- gsub("[\u2032\u2019]", "'", location)
      location <- gsub("[\u2033\u201D]", "\"", location)
      parts <- strsplit(trimws(location), "\\s+")[[1]]
      if (length(parts) < 2) {
         if (warn) warning("Invalid location (expected 'lat lon'): ", location)
         return(list(latitude = NA_real_, longitude = NA_real_))
      }
      list(
         latitude = dms_coord_to_decimal(parts[1], warn = warn),
         longitude = dms_coord_to_decimal(parts[2], warn = warn)
      )
   }
}

if (!exists("parse_dms_vector", inherits = FALSE)) {
   parse_dms_vector <- function(vec, warn = FALSE) { # FUNCTION: vectorized wrapper returning data.frame Latitude/Longitude
      res <- purrr::map(vec, ~ parse_dms_pair(.x, warn = warn))
      data.frame(
         Latitude = vapply(res, function(x) x$latitude, numeric(1)),
         Longitude = vapply(res, function(x) x$longitude, numeric(1))
      )
   }
}

# Back-compat thin wrappers (deprecated):
if (!exists("process_geographic_location2", inherits = FALSE)) {
   process_geographic_location2 <- function(location) parse_dms_pair(location) # FUNCTION: back-compat thin wrapper
}

# Robust function to convert DMS (degree-minute-second) to decimal degrees.
dms_to_dd <- function(dms, direction) { # FUNCTION: strict DMS -> decimal conversion (expects 3 parts)
   # Split using a regex that removes common DMS symbols and extra spaces.
   parts <- unlist(strsplit(dms, "[°'\" ]+"))

   # Validate that we obtained at least 3 numeric components.
   if (length(parts) < 3) {
      stop("Invalid DMS format. Expected format 'dd mm ss' with direction suffix")
   }

   degrees <- as.numeric(parts[1])
   minutes <- as.numeric(parts[2])
   seconds <- as.numeric(parts[3])

   if (any(is.na(c(degrees, minutes, seconds)))) {
      stop("One or more components of the DMS string are not numeric")
   }

   dd <- degrees + minutes / 60 + seconds / 3600

   # Reverse sign for West or South directions.
   if (direction %in% c("W", "S")) {
      dd <- -dd
   }
   return(dd)
}


# Process a geographic location string into latitude and longitude.
process_geographic_location <- function(location) { # FUNCTION: convert DMS pair string to list(latitude, longitude)
   # Expecting "lat_string lon_string", e.g. "45°30'15\"N 122°40'20\"W"
   if (is.na(location) || location == "") {
      latitude <- NA
      longitude <- NA
   } else {
      # Use trimming to reduce extra spaces.
      location <- trimws(location)

      # Split into two parts.
      parts <- strsplit(location, "\\s+")[[1]]
      if (length(parts) < 2) {
         stop("Invalid location format; expected both latitude and longitude")
      }

      lat <- parts[1]
      lon <- parts[2]

      # Extract the direction (last character) and remove it.
      lat_dir <- substr(lat, nchar(lat), nchar(lat))
      lon_dir <- substr(lon, nchar(lon), nchar(lon))
      lat_numeric <- substr(lat, 1, nchar(lat) - 1)
      lon_numeric <- substr(lon, 1, nchar(lon) - 1)

      latitude <- dms_to_dd(lat_numeric, lat_dir)
      longitude <- dms_to_dd(lon_numeric, lon_dir)
   }
   return(list(latitude = latitude, longitude = longitude))
}

process_geographic_location_lat_long <- function(location, measurement) { # FUNCTION: return only latitude or longitude from DMS pair
   # Expecting "lat_string lon_string", e.g. "45°30'15\"N 122°40'20\"W"
   if (is.na(location) || location == "" || location == "n/a") {
      latitude <- NA
      longitude <- NA
   } else {
      # Use trimming to reduce extra spaces.
      location <- trimws(location)

      # Split into two parts.
      parts <- strsplit(location, "\\s+")[[1]]
      if (length(parts) < 2) {
         stop("Invalid location format; expected both latitude and longitude")
      }

      lat <- parts[1]
      lon <- parts[2]

      # Extract the direction (last character) and remove it.
      lat_dir <- substr(lat, nchar(lat), nchar(lat))
      lon_dir <- substr(lon, nchar(lon), nchar(lon))
      lat_numeric <- substr(lat, 1, nchar(lat) - 1)
      lon_numeric <- substr(lon, 1, nchar(lon) - 1)

      latitude <- dms_to_dd(lat_numeric, lat_dir)
      longitude <- dms_to_dd(lon_numeric, lon_dir)
   }

   if (measurement == "latitude") {
      return(latitude)
   } else {
      return(longitude)
   }
}


## (Deprecated process_field_data helper removed; superseded by harmonized workflow)

# map 2

# Helper function to convert DMS to decimal degrees.
dms_to_dd_map2 <- function(dms, direction) { # FUNCTION: tolerant legacy DMS -> decimal (returns NA on malformed)
   if (is.null(dms) || is.na(dms) || !nzchar(dms)) {
      return(NA_real_)
   }
   parts <- unlist(strsplit(dms, "[°'\" ]+"))
   parts <- parts[nzchar(parts)]
   if (length(parts) < 3) {
      return(NA_real_)
   }
   suppressWarnings(nums <- as.numeric(parts[1:3]))
   if (any(is.na(nums))) {
      return(NA_real_)
   }
   dd <- nums[1] + nums[2] / 60 + nums[3] / 3600
   direction <- toupper(direction)
   if (direction %in% c("W", "S")) dd <- -dd
   dd
}

safe_parse_dms_pair_legacy <- function(location) { # FUNCTION: legacy safe pair parser returning list
   if (is.null(location) || is.na(location) || !nzchar(trimws(location))) {
      return(list(latitude = NA_real_, longitude = NA_real_))
   }
   location <- gsub("[\u2032\u2019]", "'", location)
   location <- gsub("[\u2033\u201D]", "\"", location)
   parts <- strsplit(trimws(location), "[[:space:]]+")[[1]] # split on any whitespace
   parts <- parts[nzchar(parts)]
   if (length(parts) < 2) {
      return(list(latitude = NA_real_, longitude = NA_real_))
   }
   lat <- parts[1]
   lon <- parts[2]
   lat_dir <- substr(lat, nchar(lat), nchar(lat))
   lon_dir <- substr(lon, nchar(lon), nchar(lon))
   lat_num <- substr(lat, 1, nchar(lat) - 1)
   lon_num <- substr(lon, 1, nchar(lon) - 1)
   list(
      latitude = dms_to_dd_map2(lat_num, lat_dir),
      longitude = dms_to_dd_map2(lon_num, lon_dir)
   )
}

process_geographic_location2 <- function(location) { # FUNCTION: legacy parsing kept for backwards compatibility
   location <- trimws(location)
   location <- gsub("[\u2032\u2019]", "'", location)
   location <- gsub("[\u2033\u201D]", "\"", location)
   parts <- strsplit(location, "\\s+")[[1]]
   lat <- parts[1]
   lon <- parts[2]
   lat_dir <- substr(lat, nchar(lat), nchar(lat))
   lon_dir <- substr(lon, nchar(lon), nchar(lon))
   lat_numeric <- substr(lat, 1, nchar(lat) - 1)
   lon_numeric <- substr(lon, 1, nchar(lon) - 1)
   latitude <- dms_to_dd_map2(lat_numeric, lat_dir)
   longitude <- dms_to_dd_map2(lon_numeric, lon_dir)
   return(list(latitude = latitude, longitude = longitude))
}

#------------------------------------------------------------
# Harmonized map builder (testable helper)
#------------------------------------------------------------
# Contract:
#  input_df: data.frame containing columns Parent.ID, Owen.s.Notation.for.sequencing,
#            and either (Latitude & Longitude) numeric or Site.Coordinates DMS strings.
#  full_df: optional full harmonized dataset to backfill Date / Latitude / Longitude.
#  debug: logical to emit diagnostics via cat (prefixed [build_harmonized_map]).
# Returns list with:
#  $widget  leaflet htmlwidget
#  $data    processed data used for markers (after coordinate filtering)
#  $excluded_n count of excluded rows due to missing coordinates
#  $valid_n number of plotted markers
#  $palette_domain numeric length-2 vector (may be NA_real_ if not available)
build_harmonized_map <- function(input_df, full_df = NULL, debug = FALSE) { # FUNCTION: generate leaflet widget + processed data & stats from harmonized subset
   stopifnot(is.data.frame(input_df))
   pr <- function(...) if (isTRUE(debug)) cat("[build_harmonized_map]", ..., "\n")
   map_df <- input_df

   # Backfill Date from full_df if absent
   if (!"Date" %in% names(map_df) && !is.null(full_df)) {
      candidate_dates <- c("Date", "Date.filtered", "Date.of.Sample.Collection", "Date.of.Sampling")
      key_cols <- intersect(c("Parent.ID", "Owen.s.Notation.for.sequencing"), names(map_df))
      if (length(key_cols) == 2 && all(key_cols %in% names(full_df))) {
         full_key <- paste0(full_df[[key_cols[1]]], "|", full_df[[key_cols[2]]])
         sub_key <- paste0(map_df[[key_cols[1]]], "|", map_df[[key_cols[2]]])
         for (cand in candidate_dates) {
            if (cand %in% names(full_df)) {
               map_df$Date <- full_df[[cand]][match(sub_key, full_key)]
               pr("Attached Date from", cand, "nonNA=", sum(!is.na(map_df$Date)))
               break
            }
         }
      }
   }

   # Date parsing now delegated to global parse_multi_date() helper (Helpers/parse_multi_date.R)
   # Keeping no local redefinition here to avoid future drift.

   need_parse <- !all(c("Latitude", "Longitude") %in% names(map_df))
   parsed_list <- if (need_parse && "Site.Coordinates" %in% names(map_df)) {
      purrr::map(map_df[["Site.Coordinates"]], ~ tryCatch(parse_dms_pair(.x), error = function(e) list(latitude = NA_real_, longitude = NA_real_)))
   } else {
      vector("list", length = nrow(map_df))
   }
   lat_vec <- if (need_parse) purrr::map_dbl(parsed_list, ~ .x$latitude) else suppressWarnings(as.numeric(map_df[["Latitude"]]))
   lon_vec <- if (need_parse) purrr::map_dbl(parsed_list, ~ .x$longitude) else suppressWarnings(as.numeric(map_df[["Longitude"]]))
   date_vec <- if ("Date" %in% names(map_df)) parse_multi_date(map_df[["Date"]]) else as.Date(rep(NA_character_, nrow(map_df)))
   processed <- dplyr::mutate(map_df,
      parsed_coords = parsed_list,
      Latitude = lat_vec,
      Longitude = lon_vec,
      sample = paste0(map_df[["Parent.ID"]], ": ", map_df[["Owen.s.Notation.for.sequencing"]]),
      Date = date_vec,
      DateNumeric = as.numeric(date_vec)
   )

   valid_coord <- with(processed, !is.na(Latitude) & !is.na(Longitude))
   excluded_n <- sum(!valid_coord)
   plotted <- processed[valid_coord, , drop = FALSE]
   dp <- determine_date_palette(plotted$DateNumeric)
   date_domain <- dp$date_domain
   pal <- dp$pal
   legend_title <- dp$legend_title

   if (!nrow(plotted)) {
      widget <- leaflet::leaflet() |>
         leaflet::addTiles() |>
         leaflet::addControl("No valid geographic points to display", position = "topright")
      return(list(widget = widget, data = plotted, excluded_n = excluded_n, valid_n = 0L, palette_domain = date_domain))
   }

   widget <- leaflet::leaflet(plotted) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(~Longitude, ~Latitude,
         layerId = ~sample,
         label = ~ paste0("Sample: ", sample, " | Date: ", format(Date, "%Y-%m-%d")),
         color = ~ pal(DateNumeric), fillColor = ~ pal(DateNumeric),
         fillOpacity = 1, stroke = TRUE, weight = 1
      )
   if (!dp$no_date_flag) {
      widget <- widget |>
         leaflet::addLegend(
            position = "bottomright", pal = pal, values = ~DateNumeric, title = legend_title,
            labFormat = function(type, cuts, p) {
               format(as.Date(cuts, origin = "1970-01-01"), "%Y-%m-%d")
            }, opacity = 1
         )
   }

   list(widget = widget, data = plotted, excluded_n = excluded_n, valid_n = nrow(plotted), palette_domain = date_domain)
}

# Global variable bindings (for R CMD check notes suppression when using dplyr non-standard eval)
utils::globalVariables(c(
   "Site.Coordinates", "parsed_coords", "Latitude", "Longitude", "Parent.ID",
   "Owen.s.Notation.for.sequencing", "Date", "DateNumeric", "sample"
))

#------------------------------------------------------------
# Date palette helper (centralized logic)
#------------------------------------------------------------
determine_date_palette <- function(date_numeric_vec) { # FUNCTION: build color palette safely from numeric date vector (skip legend when no dates)
   res <- list(
      pal = leaflet::colorNumeric(palette = c("#999999"), domain = c(0, 1), na.color = "transparent"),
      date_domain = c(NA_real_, NA_real_),
      legend_title = "No Date",
      no_date_flag = TRUE
   )
   if (is.null(date_numeric_vec) || !length(date_numeric_vec)) {
      return(res)
   }
   finite_vals <- date_numeric_vec[is.finite(date_numeric_vec)]
   if (!length(finite_vals)) {
      return(res)
   }
   rng <- range(finite_vals, na.rm = TRUE)
   if (length(rng) != 2 || any(!is.finite(rng))) {
      return(res)
   }
   # Single date -> expand artificial domain
   if (rng[1] == rng[2]) {
      expanded <- c(rng[1] - 0.5, rng[2] + 0.5)
      res$pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = expanded, na.color = "transparent")
      res$date_domain <- rng
      res$legend_title <- "Sample Date"
      res$no_date_flag <- FALSE
      return(res)
   }
   res$pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = rng, na.color = "transparent")
   res$date_domain <- rng
   res$legend_title <- "Sample Date"
   res$no_date_flag <- FALSE
   res
}
