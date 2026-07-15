#!/usr/bin/env Rscript
options(repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"
))

required_packages <- c(
    "xml2",          # explicit — rvest depends on it
    "data.table",
    "ggplot2",
    "scales",
    "lubridate",
    "openxlsx2",
    "lemon",
    "httr",
    "rvest",
    "stringr",
    "R.utils",
    "ggh4x",
    "chromote"
)

install.packages(
    pkgs = required_packages,
    Ncpus = max(1L, parallel::detectCores() - 1L),
    dependencies = c("Depends", "Imports", "LinkingTo")
)

# Check 1: installed
installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
    stop("CRITICAL: Failed to install: ", paste(missing, collapse = ", "))
}

# Check 2: loadable (catches broken shared libs, ICU mismatches, etc.)
load_errors <- character()
for (pkg in required_packages) {
    tryCatch(
        library(pkg, character.only = TRUE, quietly = TRUE),
        error = function(e) {
            load_errors <<- c(load_errors, sprintf("  %s -> %s", pkg, conditionMessage(e)))
        }
    )
}
if (length(load_errors) > 0) {
    stop("CRITICAL: Installed but failed to load:\n", paste(load_errors, collapse = "\n"))
}

cat("All packages installed and loadable:\n")
cat(paste(required_packages, collapse = ", "), "\n")
