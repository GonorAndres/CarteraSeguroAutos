# ==============================================================================
# GLOBAL.R - Configuracion global del dashboard
# Carga de paquetes, datos y modulos
# ==============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(scales)
library(plotly)
library(DT)
library(shinyWidgets)

# Source utils
source("R/utils_theme.R")
source("R/utils_metrics.R")
source("R/utils_data.R")
source("R/utils_export.R")

# Source modules
source("R/mod_sidebar_filters.R")
source("R/mod_resumen.R")
source("R/mod_loss_ratio.R")
source("R/mod_frecuencia.R")
source("R/mod_severidad.R")
source("R/mod_temporal.R")
source("R/mod_geografico.R")
source("R/mod_segmentacion.R")
source("R/mod_pricing_glm.R")
source("R/mod_ibnr.R")
source("R/mod_scenario.R")
source("R/mod_fraud.R")
source("R/mod_datos.R")

# Cargar datos una vez al inicio
APP_DATA <- tryCatch(
  {
    raw <- load_data()
    raw$polizas <- enrich_polizas(raw$polizas)
    # Cargar pagos de desarrollo si existe la tabla
    con <- dbConnect(SQLite(), "data/siniestralidad.db")
    on.exit(dbDisconnect(con), add = TRUE)
    if ("pagos_desarrollo" %in% dbListTables(con)) {
      raw$pagos <- dbGetQuery(con, "SELECT * FROM pagos_desarrollo") %>%
        as_tibble()
    } else {
      raw$pagos <- tibble()
    }
    raw
  },
  error = function(e) {
    message("Error cargando datos: ", e$message)
    list(
      polizas = tibble(),
      siniestros = tibble(),
      pagos = tibble()
    )
  }
)

cat(sprintf("Datos cargados: %s polizas, %s siniestros, %s pagos\n",
            format(nrow(APP_DATA$polizas), big.mark = ","),
            format(nrow(APP_DATA$siniestros), big.mark = ","),
            format(nrow(APP_DATA$pagos), big.mark = ",")))
