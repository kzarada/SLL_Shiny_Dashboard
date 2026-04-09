FROM rocker/tidyverse:4.5.2 

# Install cron + system dependencies for common R packages
RUN apt-get update && apt-get install -y \
    cron \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY scripts/ /app/scripts/
COPY crontab /etc/cron.d/my-cron

RUN R -e 'install.packages(c("tidyverse", "httr", "jsonlite", "vroom", "zoo"), repos="http://cran.rstudio.com")'

RUN chmod 0644 /etc/cron.d/my-cron && \
    crontab /etc/cron.d/my-cron

RUN touch /var/log/cron.log

CMD ["cron", "-f"]