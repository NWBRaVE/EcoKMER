# File: helper_fns_cleaning.R - Lightweight data cleaning helpers

clean_string_sentinels <- function(df,
                                   sentinels = c("BLANK", "Blank", "N/A", "NA", "n/a", "Too low", "BD", "AB")) { # FUNCTION: replace sentinel tokens in character/factor columns with NA preserving factors
    if (!is.data.frame(df)) {
        return(df)
    }
    for (col in names(df)) {
        if (is.character(df[[col]]) || is.factor(df[[col]])) {
            # Convert factor to character temporarily to avoid level warnings
            was_factor <- is.factor(df[[col]])
            vec <- as.character(df[[col]])
            vec[vec %in% sentinels] <- NA
            if (was_factor) {
                df[[col]] <- factor(vec)
            } else {
                df[[col]] <- vec
            }
        }
    }
    df
}
