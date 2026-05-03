# ==============================================================================
# DASHBOARD SINIESTRALIDAD - SEGUROS AUTO MEXICO
# Autor: Andres Gonzalez Ortega
# Plataforma de analisis actuarial de portafolio auto
# ==============================================================================

source("global.R")

ui <- page_navbar(
  title = "Siniestralidad Auto Mexico",
  theme = app_theme,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    if (nzchar(Sys.getenv("GOOGLE_ANALYTICS_ID", ""))) tagList(
      tags$script(async = NA, src = paste0("https://www.googletagmanager.com/gtag/js?id=", Sys.getenv("GOOGLE_ANALYTICS_ID"))),
      tags$script(HTML(sprintf("window.dataLayer=window.dataLayer||[];function gtag(){dataLayer.push(arguments);}gtag('js',new Date());gtag('config','%s');", Sys.getenv("GOOGLE_ANALYTICS_ID"))))
    ),
    if (nzchar(Sys.getenv("POSTHOG_KEY", ""))) tags$script(HTML(sprintf("!function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split('.');2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement('script')).type='text/javascript',p.crossOrigin='anonymous',p.async=!0,p.src=s.api_host+'/static/array.js',(r=t.getElementsByTagName('script')[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a='posthog',u.people=u.people||[],u.toString=function(t){var e='posthog';return'posthog'!==a&&(e+='.'+a),t||(e+=' (stub)'),e},u.people.toString=function(){return u.toString(1)+'.people (stub)'},o='init capture register register_once unregister opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing identify alias people.set people.set_once set_config reset get_distinct_id getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags group updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures getActiveMatchingSurveys getSurveys onFeatureFlags onSessionId'.split(' '),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);posthog.init('%s',{api_host:'https://us.i.posthog.com',autocapture:false,capture_pageview:true});", Sys.getenv("POSTHOG_KEY"))))
  ),
  sidebar = sidebar(
    title = "Filtros",
    width = 250,
    sidebarFiltersUI("filters")
  ),

  # --- Pestañas de analisis ---
  nav_panel("Resumen",       icon = icon("gauge"),         resumenUI("resumen")),
  nav_panel("Loss Ratio",    icon = icon("chart-line"),     lossRatioUI("lr")),
  nav_panel("Frecuencia",    icon = icon("chart-bar"),      frecuenciaUI("freq")),
  nav_panel("Severidad",     icon = icon("dollar-sign"),    severidadUI("sev")),
  nav_panel("Temporal",      icon = icon("calendar"),       temporalUI("temp")),
  nav_panel("Geogr\u00e1fico",    icon = icon("map"),            geograficoUI("geo")),
  nav_panel("Segmentaci\u00f3n",  icon = icon("users"),          segmentacionUI("seg")),

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
