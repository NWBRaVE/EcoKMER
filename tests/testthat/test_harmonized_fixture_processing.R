# File: test_harmonized_fixture_processing.R - Fixture schema + parsing sanity (allows extra columns)
# TODO: Add explicit test for newly required columns if downstream features depend on them.

library(testthat)

fixture_path <- test_path("fixtures", "harmonized_fixture.csv")

test_that("fixture file exists", { # TEST: fixture CSV present
    expect_true(file.exists(fixture_path))
})

raw_df <- read.csv(fixture_path, comment.char = "#", stringsAsFactors = FALSE)

# Core column subset (UI defaults) – full fixture now includes entire schema; we only assert presence of these.
CORE_COLUMNS <- c(
    "Parent.ID",
    "Site.Coordinates",
    "Owen.s.Notation.for.sequencing",
    "Date",
    "DNA.Yield..ng.µl."
)

test_that("core columns present in full-schema fixture", { # TEST: core schema columns present
    missing <- setdiff(CORE_COLUMNS, names(raw_df))
    expect_true(length(missing) == 0, paste("Missing:", paste(missing, collapse = ", ")))
})

# Uniqueness: allow deliberate *_DUP duplicate pattern only.

test_that("Parent.ID mostly unique (duplicates allowed only when intentional)", { # TEST: duplicate IDs only allowed with _DUP suffix
    dup_ids <- raw_df$Parent.ID[duplicated(raw_df$Parent.ID)]
    # Allow a single deliberate duplicate pattern with suffix _DUP used to model grouping behavior.
    expect_true(all(grepl("_DUP$", dup_ids)),
        info = paste("Unexpected non-suffixed duplicates:", paste(unique(setdiff(dup_ids, grep("_DUP$", dup_ids, value = TRUE))), collapse = ", "))
    )
})

# Date parsing via base as.Date (no extra deps).
test_that("Date column parses to Date class", { # TEST: Date column coercible to Date
    parsed <- suppressWarnings(as.Date(raw_df$Date))
    expect_s3_class(parsed, "Date")
    expect_false(all(is.na(parsed)), info = "All dates failed to parse")
})

# Numeric coercion: expect at least one NA (blank control row) + variety.

suppressWarnings({
    dna_numeric <- as.numeric(raw_df$DNA.Yield..ng.µl.)
})

test_that("DNA.Yield..ng.µl. coerces to numeric with at least one NA (control row)", { # TEST: numeric coercion + presence of control NA
    expect_true(is.numeric(dna_numeric))
    expect_true(any(is.na(dna_numeric)), "Expected an NA from blank control row in DNA.Yield..ng.µl.")
    expect_gt(max(dna_numeric, na.rm = TRUE), 50) # ensure magnitude variety for slider tests
})

# Coordinates: must include DMS + blank/NA for error paths.

test_that("Site.Coordinates includes DMS pattern and blank", { # TEST: coordinates include both DMS and blank for error path
    coords <- raw_df$Site.Coordinates
    has_dms <- any(grepl("°", coords, fixed = TRUE))
    has_blank <- any(is.na(coords) | coords == "")
    expect_true(has_dms, info = "No DMS-style coordinate present")
    expect_true(has_blank, info = "No blank/NA coordinate present for error path test")
})

# Add new required columns by extending CORE_COLUMNS if logic begins to depend on them.

# Character sanity: ensure strings not silently coerced to factors.

test_that("String columns are character (not factors)", { # TEST: ensure character columns not auto-factor
    expect_true(is.character(raw_df$Parent.ID))
    expect_true(is.character(raw_df$Owen.s.Notation.for.sequencing))
})
