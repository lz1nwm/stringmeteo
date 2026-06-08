#!/usr/local/bin/Rscript

# Set repository mirror explicitly matched to R 4.6 on Ubuntu
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"))

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
    dependencies = c("Depends", "Imports", "LinkingTo"),
)

# Verification check
missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
    stop(paste("CRITICAL ERROR: Packages failed:", paste(missing, collapse = ", ")))
}