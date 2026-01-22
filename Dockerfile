FROM rocker/shiny:4.3.0

## System dependencies (if needed for some packages)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

## Install R packages
RUN R -e "install.packages(c('shiny', 'bslib', 'plotly', 'quantmod', 'PerformanceAnalytics', 'xts', 'httr', 'jsonlite'), repos = 'https://cloud.r-project.org')"

## Copy app
WORKDIR /srv/shiny-server
COPY app.R /srv/shiny-server/app.R
COPY README.md /srv/shiny-server/README.md

EXPOSE 3838

CMD [\"/usr/bin/shiny-server\"]

