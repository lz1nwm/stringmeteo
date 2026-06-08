FROM ghcr.io/rocker-org/r-ver:4.6.0

ENV DEBIAN_FRONTEND=noninteractive

# System libraries commonly needed by httr, rvest/xml2, curl, openssl,
# graphics/font packages, and spreadsheet/chart dependencies.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

COPY install.R /tmp/install.R

RUN Rscript /tmp/install.R