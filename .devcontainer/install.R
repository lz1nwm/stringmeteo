#!/usr/bin/env Rscript

# Use Posit Package Manager binaries for Ubuntu Noble.
# The rocker/r-ver:4.6.0 image also sets a CRAN/P3M mirror, but this keeps
# the repository explicit and aligned with Ubuntu 24.04 Noble.
options(repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"
))

required_packages <- c(
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
    "ggh4x"
)

install.packages(
    pkgs = required_packages,
    Ncpus = max(1L, parallel::detectCores() - 1L),
    dependencies = c("Depends", "Imports", "LinkingTo")
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
    stop(
        paste(
            "CRITICAL ERROR: Packages failed:",
            paste(missing, collapse = ", ")
        )
    )
}

cat("Installed packages successfully:\n")
cat(paste(required_packages, collapse = ", "), "\n")