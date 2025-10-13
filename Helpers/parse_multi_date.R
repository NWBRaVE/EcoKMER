## File: parse_multi_date.R - Multi-format Date parser (tries several regex patterns sequentially)
## TODO: Support locale-specific month names or ISO week formats if future data requires.
parse_multi_date <- function(x) { # FUNCTION: parse heterogeneous date strings to Date, preserving already-Date inputs
    if (inherits(x, "Date")) {
        return(x)
    }
    if (is.null(x)) {
        return(as.Date(NA))
    }
    if (is.factor(x)) x <- as.character(x)
    # Normalize whitespace & strip time portion if present (e.g., 'YYYY-MM-DD HH:MM:SS')
    x <- trimws(x)
    has_time <- grepl("\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}", x)
    if (any(has_time)) {
        # Extract date portion safely
        x[has_time] <- sub("^(\\d{4}-\\d{2}-\\d{2})\\s+.*$", "\\1", x[has_time])
    }
    n <- length(x)
    res_chr <- rep(NA_character_, n)
    abbr_months <- tolower(month.abb)
    # Regex patterns
    rx_iso <- "^(\\d{4})-(\\d{2})-(\\d{2})$" # Y-m-d
    rx_slash_mdy <- "^(\\d{2})/(\\d{2})/(\\d{4})$" # m/d/Y
    rx_slash_ymd <- "^(\\d{4})/(\\d{2})/(\\d{2})$" # Y/m/d
    rx_abbr <- "^(\\d{2})-([A-Za-z]{3})-(\\d{4})$" # d-Mon-Y
    rx_hyphen_num <- "^(\\d{2})-(\\d{2})-(\\d{4})$" # dd-mm-YYYY or mm-dd-YYYY
    debug <- isTRUE(getOption("ecokmer.dateparse.debug", FALSE))
    for (i in seq_len(n)) {
        val <- x[i]
        if (is.na(val) || val == "") next
        iso <- NA_character_
        if (grepl(rx_iso, val)) {
            m <- regexec(rx_iso, val)
            parts <- regmatches(val, m)[[1]]
            iso <- sprintf("%s-%s-%s", parts[2], parts[3], parts[4])
        } else if (grepl(rx_slash_mdy, val)) {
            m <- regexec(rx_slash_mdy, val)
            parts <- regmatches(val, m)[[1]]
            iso <- sprintf("%s-%s-%s", parts[4], parts[2], parts[3])
        } else if (grepl(rx_slash_ymd, val)) {
            m <- regexec(rx_slash_ymd, val)
            parts <- regmatches(val, m)[[1]]
            iso <- sprintf("%s-%s-%s", parts[2], parts[3], parts[4])
        } else if (grepl(rx_abbr, val)) {
            m <- regexec(rx_abbr, val)
            parts <- regmatches(val, m)[[1]]
            mon_idx <- match(tolower(parts[3]), abbr_months)
            if (!is.na(mon_idx)) iso <- sprintf("%s-%02d-%s", parts[4], mon_idx, parts[2])
        } else if (grepl(rx_hyphen_num, val)) {
            m <- regexec(rx_hyphen_num, val)
            parts <- regmatches(val, m)[[1]]
            a <- as.integer(parts[2])
            b <- as.integer(parts[3])
            y <- parts[4]
            # If a>12 -> day-first; else if b>12 -> month-first; else default day-first (test expectation for 19-07-2024)
            if (a > 12 && b <= 12) {
                iso <- sprintf("%s-%02d-%02d", y, b, a)
            } else if (b > 12 && a <= 12) {
                iso <- sprintf("%s-%02d-%02d", y, a, b)
            } else {
                iso <- sprintf("%s-%02d-%02d", y, b, a) # day-first default
            }
            if (debug) message(sprintf("[parse_multi_date][amb-num] raw=%s a=%d b=%d y=%s iso=%s", val, a, b, y, iso))
        }
        res_chr[i] <- iso
    }
    out <- suppressWarnings(as.Date(res_chr))
    # Secondary fallback: if still NA but original input looked like extended ISO datetime, try cutting first 10 chars
    still_na <- is.na(out) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
    if (any(still_na)) {
        tmp_dates <- substr(x[still_na], 1, 10)
        suppressWarnings(out2 <- as.Date(tmp_dates))
        out[still_na] <- out2
    }
    # Fallback attempt for any remaining NA: base as.Date() (may help exotic ISO-like forms)
    need <- is.na(out) & !is.na(res_chr)
    if (any(need)) {
        fb <- suppressWarnings(as.Date(res_chr[need]))
        out[need] <- fb
    }
    # Detailed per-row final tracing only if an explicit verbose flag is set (more granular than general debug)
    if (isTRUE(getOption("ecokmer.dateparse.verbose", FALSE))) {
        for (i in seq_len(n)) {
            message(sprintf("[parse_multi_date][final2] idx=%d input=%s iso=%s output=%s", i, as.character(x[i]), res_chr[i], as.character(out[i])))
        }
    }
    out
}
