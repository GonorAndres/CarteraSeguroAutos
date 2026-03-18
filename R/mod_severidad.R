# ==============================================================================
# MODULO: Analisis de Severidad
# ==============================================================================

severidadUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Severidad por Tipo de Siniestro"),
        plotlyOutput(ns("plot_tipo"), height = "350px")
      ),
      card(
        card_header("Distribucion de Severidad"),
        plotlyOutput(ns("plot_hist"), height = "350px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Severidad por Año"),
        plotlyOutput(ns("plot_anio"), height = "350px")
      ),
      card(
        card_header("Severidad por Tipo de Vehiculo"),
        plotlyOutput(ns("plot_vehiculo"), height = "350px")
      )
    ),
    card(
      card_header("Estadisticas de Severidad"),
      DTOutput(ns("tabla_sev"))
    )
  )
}

severidadServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$plot_tipo <- renderPlotly({
      d <- filtered_data()
      s <- calc_severity(d$siniestros, tipo_siniestro) %>%
        arrange(desc(severidad_media))
      plot_ly(s, y = ~reorder(tipo_siniestro, severidad_media),
              x = ~severidad_media, type = "bar", orientation = "h",
              marker = list(color = PALETTE$danger)) %>%
        plotly_default_layout(xlab = "Severidad Media (MXN)", ylab = "")
    })

    output$plot_hist <- renderPlotly({
      d <- filtered_data()
      plot_ly(d$siniestros, x = ~monto_siniestro, type = "histogram",
              nbinsx = 50,
              marker = list(color = PALETTE$primary, line = list(width = 0.5, color = "white"))) %>%
        plotly_default_layout(xlab = "Monto Siniestro (MXN)", ylab = "Frecuencia")
    })

    output$plot_anio <- renderPlotly({
      d <- filtered_data()
      s <- calc_severity(d$siniestros, anio_ocurrencia) %>%
        arrange(anio_ocurrencia)
      plot_ly(s, x = ~factor(anio_ocurrencia), y = ~severidad_media,
              type = "bar", marker = list(color = PALETTE$accent)) %>%
        plotly_default_layout(xlab = "Año", ylab = "Severidad Media (MXN)")
    })

    output$plot_vehiculo <- renderPlotly({
      d <- filtered_data()
      sin_veh <- d$siniestros %>%
        left_join(d$polizas %>% select(poliza_id, tipo_vehiculo), by = "poliza_id")
      s <- calc_severity(sin_veh, tipo_vehiculo) %>%
        arrange(desc(severidad_media))
      plot_ly(s, y = ~reorder(tipo_vehiculo, severidad_media),
              x = ~severidad_media, type = "bar", orientation = "h",
              marker = list(color = PALETTE$secondary)) %>%
        plotly_default_layout(xlab = "Severidad Media (MXN)", ylab = "")
    })

    output$tabla_sev <- renderDT({
      d <- filtered_data()
      s <- calc_severity(d$siniestros, tipo_siniestro) %>%
        arrange(desc(severidad_media))
      datatable(s, options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE) %>%
        formatCurrency(c("severidad_media", "severidad_mediana", "severidad_sd",
                         "severidad_min", "severidad_max"),
                       currency = "$", digits = 0)
    })
  })
}
