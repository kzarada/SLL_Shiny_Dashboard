FROM rocker/shiny-verse:4.5.2 

RUN apt-get update && apt-get install -y \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libprotobuf-dev \
    protobuf-compiler \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages(c('s2', 'sf', 'leaflet', 'fresh', 'shinydashboard', 'shinybrowser', 'vroom', 'fs', 'zoo', 'shinyWidgets', 'shinyfullscreen', 'bsicons', 'fontawesome'), repos='https://cloud.r-project.org')"

RUN rm -rf /srv/shiny-server/*

COPY app/app.R /srv/shiny-server/app.R
COPY app/www/ /srv/shiny-server/www/

EXPOSE 3838

# Run app directly
CMD ["/usr/bin/shiny-server"]
