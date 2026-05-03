# ==============================================================================
# TEMA Y ESTILOS CENTRALIZADOS
# Paleta de colores, formateo y defaults de visualizacion
# ==============================================================================

library(bslib)

# --- Paleta de colores ---
PALETTE <- list(
  primary   = "#2E86AB",
  secondary = "#A23B72",
  accent    = "#F18F01",
  danger    = "#C73E1D",
  success   = "#6A994E",
  bg_light  = "#F8F9FA",
  text_dark = "#2D3436",
  muted     = "#6C757D"
)

# Paleta ordenada para graficos categoricos
PALETTE_CATEGORICAL <- c(
  PALETTE$primary, PALETTE$secondary, PALETTE$accent,
  PALETTE$danger, PALETTE$success, PALETTE$muted,
  "#4ECDC4", "#556270", "#C7F464"
)

# --- Tema bslib Bootstrap 5 ---
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary   = PALETTE$primary,
  secondary = PALETTE$secondary,
  success   = PALETTE$success,
  danger    = PALETTE$danger,
  warning   = PALETTE$accent,
  font_scale = 0.95,
  `enable-rounded` = TRUE
)

# --- KPI target thresholds ---
KPI_TARGETS <- list(
  loss_ratio = list(bueno = 0.70, alerta = 0.80),
  frecuencia = list(bueno = 0.08, alerta = 0.10),
  severidad_media = list(bueno = 24000, alerta = 30000)
)

# --- Actuarial configuration constants ---
FRAUD_CONFIG <- list(
  score_threshold      = 0.7,
  mahal_weight         = 0.4,
  rules_weight         = 0.6,
  multiple_claims_days = 60,
  inception_days       = 30,
  severity_multiplier  = 3.0,
  reporting_delay_days = 10,
  sum_insured_ratio    = 0.90,
  n_rules              = 5
)

PRICING_CONFIG <- list(
  commercial_loading = 1.40
)

IBNR_CONFIG <- list(
  default_elr = 0.75,
  elr_min     = 0.60,
  elr_max     = 0.90
)

#' Color semaforo para KPIs
#' @param value valor del KPI
#' @param bueno umbral bueno (por debajo = verde)
#' @param alerta umbral alerta (por debajo = amarillo, arriba = rojo)
#' @return string: "success", "warning", "danger"
kpi_color <- function(value, bueno, alerta) {
  if (is.na(value)) return("secondary")
  if (value <= bueno) "success"
  else if (value <= alerta) "warning"
  else "danger"
}

#' Icono de tendencia (flecha arriba/abajo)
#' @param delta cambio porcentual
#' @param invert TRUE si menor es mejor (ej: loss ratio)
#' @return shiny icon
trend_icon <- function(delta, invert = FALSE) {
  if (is.na(delta) || is.null(delta)) return(shiny::icon("minus"))
  positive <- delta > 0
  if (invert) positive <- !positive
  if (positive) shiny::icon("arrow-up") else shiny::icon("arrow-down")
}

#' Formato moneda MXN
format_currency_mxn <- function(x, digits = 0) {
  if (is.na(x) || is.infinite(x)) return("--")
  paste0("$", format(round(x, digits), big.mark = ",", scientific = FALSE))
}

#' Formato moneda MXN en millones
format_currency_millions <- function(x) {
  if (is.na(x) || is.infinite(x)) return("--")
  paste0("$", format(round(x / 1e6, 1), big.mark = ","), "M")
}

#' Formato porcentaje
format_pct <- function(x, digits = 2) {
  if (is.na(x) || is.infinite(x)) return("--")
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

#' Formato numero con separador de miles
format_num <- function(x) {
  if (is.na(x) || is.infinite(x)) return("--")
  format(x, big.mark = ",", scientific = FALSE)
}

#' Aplicar layout default a plotly
plotly_default_layout <- function(p, title = NULL, xlab = NULL, ylab = NULL) {
  layout_args <- list(
    p = p,
    font = list(family = "system-ui, -apple-system, sans-serif"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent",
    margin = list(l = 60, r = 20, t = 40, b = 60),
    xaxis = list(
      title = xlab,
      gridcolor = "#E9ECEF",
      zerolinecolor = "#E9ECEF"
    ),
    yaxis = list(
      title = ylab,
      gridcolor = "#E9ECEF",
      zerolinecolor = "#E9ECEF"
    )
  )
  if (!is.null(title)) layout_args$title <- list(text = title, x = 0.02)
  do.call(plotly::layout, layout_args)
}

#' Crear plotly bar chart estandar
plotly_bar <- function(data, x, y, color = PALETTE$primary,
                       orientation = "v", text_format = NULL, ...) {
  p <- plotly::plot_ly(
    data,
    x = x, y = y,
    type = "bar",
    orientation = orientation,
    marker = list(color = color, line = list(width = 0)),
    ...
  )
  if (!is.null(text_format)) {
    p <- p %>% plotly::layout(yaxis = list(tickformat = text_format))
  }
  p %>% plotly_default_layout()
}

# --- Column label lookup for DT tables ---
COLUMN_LABELS <- c(
  canal_venta = "Canal de Venta",
  tipo_vehiculo = "Tipo Veh\u00edculo",
  marca_vehiculo = "Marca",
  modelo_vehiculo = "Modelo",
  n_polizas = "Polizas",
  prima_total = "Prima Total",
  n_siniestros = "Siniestros",
  siniestros_total = "Siniestros Total",
  loss_ratio = "Loss Ratio",
  frecuencia = "Frecuencia",
  rango_edad = "Rango Edad",
  segmento_edad = "Segmento Edad",
  segmento_score = "Score Crediticio",
  tipo_siniestro = "Tipo Siniestro",
  severidad_media = "Severidad Media",
  severidad_mediana = "Severidad Mediana",
  severidad_sd = "Desv. Est\u00e1ndar",
  severidad_min = "M\u00ednimo",
  severidad_max = "M\u00e1ximo",
  n = "Cantidad",
  anio_suscripcion = "A\u00f1o"
)

# Human-readable metric labels for geographic module
METRIC_LABELS <- c(
  loss_ratio = "Loss Ratio",
  frecuencia = "Frecuencia",
  severidad_media = "Severidad Media (MXN)",
  n_polizas = "Polizas"
)

# Helper to rename columns for DT display
humanize_colnames <- function(df) {
  current <- names(df)
  new_names <- ifelse(current %in% names(COLUMN_LABELS), COLUMN_LABELS[current], current)
  setNames(df, new_names)
}

# Clean plotly config (hide modebar)
plotly_clean <- function(p) {
  p %>% plotly::config(displayModeBar = FALSE)
}
