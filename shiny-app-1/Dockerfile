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


RUN R -e "install.packages(c('s2', 'sf', 'leaflet', 'fresh', 'shinydashboard', 'vroom', 'fs', 'zoo', 'shinyWidgets', 'shinyfullscreen', 'bsicons', 'fontawesome'), repos='https://cloud.r-project.org')"


COPY app/ /srv/shiny-server/

EXPOSE 3838

# Run app directly
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host='0.0.0.0', port=3838)"]
