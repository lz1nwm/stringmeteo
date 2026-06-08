# Start with tidyverse v 4.3.1 from rocker
FROM ghcr.io/rocker-org/devcontainer/tidyverse:4.6
# You can use other rocker images: https://rocker-project.org/images/devcontainer/images.html
# For example for working with geospatial packages:
#FROM ghcr.io/rocker-org/devcontainer/geospatial:4.3

# Relabel docker (otherwise it will have the rocker description)
LABEL org.opencontainers.image.description="R 4.6"

# Install any additional desired packages (edit the install.R script to add packages)

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*
    
COPY .devcontainer/install.R /install.R
RUN Rscript /install.R

WORKDIR /app

# Run Rscript by default if no command is specified
CMD ["Rscript"]