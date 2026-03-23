FROM rocker/shiny-verse:4.3.3

# System dependencies (must match CI for consistent builds)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libgdal-dev libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev cmake \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m shinyuser

WORKDIR /srv/shiny-server/app

# Install renv and restore packages
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
# Use RSPM binaries for faster installs (no source compilation)
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(prompt = FALSE)"

# Copy application
COPY . .
RUN chown -R shinyuser:shinyuser /srv/shiny-server/app

USER shinyuser

ENV PORT=8080
EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', port=as.integer(Sys.getenv('PORT', 8080)), host='0.0.0.0')"]
