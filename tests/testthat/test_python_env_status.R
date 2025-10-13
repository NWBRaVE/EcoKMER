# File: test_python_env_status.R - Python module availability/status helpers

test_that("py_required_packages returns expected core set", { # TEST: required package vector contents
    expect_true(exists("py_required_packages"))
    pkgs <- py_required_packages()
    expect_true(is.character(pkgs))
    expect_true(all(c("datafed", "pandas", "tqdm", "openpyxl") %in% pkgs))
})

test_that("py_env_status structure is correct", { # TEST: structure + names of status list
    skip_if_not(
        requireNamespace("reticulate", quietly = TRUE),
        "reticulate not installed"
    )
    expect_true(exists("py_env_status"))
    st <- py_env_status()
    expect_true(is.list(st))
    expect_true(all(c("available", "versions", "all_ok", "critical_ok", "message") %in% names(st)))
    req <- py_required_packages()
    expect_equal(sort(names(st$available)), sort(req))
    expect_true(is.logical(st$available))
    expect_true(is.character(st$versions))
    expect_equal(length(st$versions), length(req))
})

test_that("critical_ok logic consistent", { # TEST: critical_ok aligns with datafed availability
    skip_if_not(
        requireNamespace("reticulate", quietly = TRUE),
        "reticulate not installed"
    )
    st <- py_env_status()
    if (st$critical_ok) {
        expect_true(st$available["datafed"]) # must be TRUE
    } else {
        expect_false(st$available["datafed"]) # must be FALSE if critical not ok
    }
})
