## File: helper_fns_python_env.R - Python env/module availability helpers (non-installing)
## TODO: Add optional virtualenv/conda path reporting for richer diagnostics.

py_required_packages <- function() { # FUNCTION: return vector of required python module names
    c("datafed", "pandas", "tqdm", "openpyxl")
}

py_env_status <- function() { # FUNCTION: inspect availability & versions of required Python modules via reticulate
    # If reticulate missing, return early with guidance.
    if (!requireNamespace("reticulate", quietly = TRUE)) {
        pkgs <- py_required_packages()
        return(list(
            available = setNames(rep(FALSE, length(pkgs)), pkgs),
            versions = setNames(rep(NA_character_, length(pkgs)), pkgs),
            all_ok = FALSE,
            critical_ok = FALSE,
            message = "reticulate not installed: install.packages('reticulate') then pip install -r requirements.txt"
        ))
    }
    pkgs <- py_required_packages()
    av <- vapply(pkgs, reticulate::py_module_available, logical(1))
    vers <- setNames(rep(NA_character_, length(pkgs)), pkgs)
    for (p in pkgs[av]) {
        # Best-effort version extraction
        vers[p] <- tryCatch(
            {
                mod <- reticulate::import(p, delay_load = TRUE)
                v <- tryCatch(mod$`__version__`, error = function(e) NA_character_)
                if (is.null(v)) NA_character_ else as.character(v)
            },
            error = function(e) NA_character_
        )
    }
    critical_ok <- av["datafed"] %||% FALSE
    all_ok <- all(av)
    status_parts <- paste0(pkgs, "=", ifelse(av, "OK", "MISSING"))
    msg <- sprintf(
        "Python deps: %s (critical: %s)",
        paste(status_parts, collapse = ", "),
        if (critical_ok) "OK" else "MISSING datafed"
    )
    list(
        available = av,
        versions = vers,
        all_ok = all_ok,
        critical_ok = critical_ok,
        message = msg
    )
}

# Convenience infix for NULL coalescing without importing extras
`%||%` <- function(a, b) if (is.null(a)) b else a # FUNCTION: null coalesce helper
