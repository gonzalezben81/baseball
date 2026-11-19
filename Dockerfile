
FROM rocker/shiny:latest

WORKDIR /srv/shiny-server

# Install application dependencies
RUN R -e "install.packages(c('shiny'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('DT'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('RSQLite'), repos='https://cloud.r-project.org')"

# Copy Shiny application
COPY / /srv/shiny-server/

# Expose Shiny Server
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
