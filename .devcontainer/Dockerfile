FROM ghcr.io/rocker-org/r-ver:4.6.0

ENV DEBIAN_FRONTEND=noninteractive

# Install the system libraries used by the R packages, plus the tools required
# to add Google's Chrome APT repository.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gnupg \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    fonts-liberation \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL https://dl.google.com/linux/linux_signing_key.pub \
        | gpg --dearmor \
        > /etc/apt/keyrings/google-chrome.gpg \
    && echo "deb [arch=amd64 signed-by=/etc/apt/keyrings/google-chrome.gpg] https://dl.google.com/linux/chrome/deb/ stable main" \
        > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        google-chrome-stable \
    && google-chrome-stable --version \
    && rm -rf /var/lib/apt/lists/*

# Tell chromote exactly which browser binary to use.
ENV CHROMOTE_CHROME=/usr/bin/google-chrome-stable

COPY install.R /tmp/install.R

RUN Rscript /tmp/install.R

# Fail the image build immediately if R/chromote cannot locate Chrome.
RUN Rscript -e \
    'stopifnot(requireNamespace("chromote", quietly = TRUE)); print(chromote::find_chrome())'

WORKDIR /workspace