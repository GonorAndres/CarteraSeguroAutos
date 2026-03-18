FROM rocker/shiny-verse:4.3.3

# System dependencies for sf/leaflet
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev libgeos-dev libproj-dev libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/app

# Install renv and restore packages
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(prompt = FALSE)"

# Copy application
COPY . .

ENV PORT=8080
EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', port=as.integer(Sys.getenv('PORT', 8080)), host='0.0.0.0')"]
