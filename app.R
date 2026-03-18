# ==============================================================================
# DASHBOARD SINIESTRALIDAD - SEGUROS AUTO MEXICO
# Autor: Andres Gonzalez Ortega
# Plataforma de analisis actuarial de portafolio auto
# ==============================================================================

source("global.R")

ui <- page_navbar(
  title = "Siniestralidad Auto Mexico",
  theme = app_theme,
  fillable = TRUE,
  sidebar = sidebar(
    title = "Filtros",
    width = 280,
    sidebarFiltersUI("filters")
  ),

  # --- Pestañas de analisis ---
  nav_panel("Resumen",       icon = icon("gauge"),         resumenUI("resumen")),
  nav_panel("Loss Ratio",    icon = icon("chart-line"),     lossRatioUI("lr")),
  nav_panel("Frecuencia",    icon = icon("chart-bar"),      frecuenciaUI("freq")),
  nav_panel("Severidad",     icon = icon("dollar-sign"),    severidadUI("sev")),
  nav_panel("Temporal",      icon = icon("calendar"),       temporalUI("temp")),
  nav_panel("Geografico",    icon = icon("map"),            geograficoUI("geo")),
  nav_panel("Segmentacion",  icon = icon("users"),          segmentacionUI("seg")),

  # --- Modulos actuariales avanzados ---
  nav_menu(
    "Actuarial",
    icon = icon("calculator"),
    nav_panel("Pricing GLM",     icon = icon("tags"),           pricingGlmUI("pricing")),
    nav_panel("Reservas IBNR",   icon = icon("layer-group"),    ibnrUI("ibnr")),
    nav_panel("Escenarios",      icon = icon("sliders"),        scenarioUI("scenario")),
    nav_panel("Fraude",          icon = icon("shield-halved"),   fraudUI("fraud"))
  ),

  nav_panel("Datos",         icon = icon("table"),          datosUI("datos")),

  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/GonorAndres/CarteraSeguroAutos",
      target = "_blank",
      icon("github"), "GitHub"
    )
  )
)

server <- function(input, output, session) {
  # Filtros globales
  filtered <- sidebarFiltersServer("filters", APP_DATA)

  # Modulos de analisis
  resumenServer("resumen", filtered)
  lossRatioServer("lr", filtered)
  frecuenciaServer("freq", filtered)
  severidadServer("sev", filtered)
  temporalServer("temp", filtered)
  geograficoServer("geo", filtered)
  segmentacionServer("seg", filtered)

  # Modulos actuariales avanzados
  pricingGlmServer("pricing", filtered)
  ibnrServer("ibnr", filtered)
  scenarioServer("scenario", filtered)
  fraudServer("fraud", filtered)

  # Datos
  datosServer("datos", filtered)
}

shinyApp(ui, server)
