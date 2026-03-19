# ==============================================================================
# MODULO: Analisis de Frecuencia
# ==============================================================================

frecuenciaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Analisis de Frecuencia"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Frecuencia por Tipo de Vehiculo"),
        plotlyOutput(ns("plot_vehiculo"), height = "350px")
      ),
      card(
        card_header("Frecuencia por Rango de Edad"),
        plotlyOutput(ns("plot_edad"), height = "350px")
      )
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Frecuencia por Genero"),
        plotlyOutput(ns("plot_genero"), height = "350px")
      ),
      card(
        card_header("Frecuencia por Canal de Venta"),
        plotlyOutput(ns("plot_canal"), height = "350px")
      )
    ),
    card(
      card_header("Detalle de Frecuencia"),
      DTOutput(ns("tabla_freq"))
    )
  )
}

frecuenciaServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$plot_vehiculo <- renderPlotly({
      d <- filtered_data()
      f <- calc_frequency(d$polizas, d$siniestros, tipo_vehiculo) %>%
        arrange(desc(frecuencia))
      plot_ly(f, y = ~reorder(tipo_vehiculo, frecuencia), x = ~frecuencia,
              type = "bar", orientation = "h",
              marker = list(color = PALETTE$primary)) %>%
        plotly_default_layout(xlab = "Frecuencia", ylab = "") %>%
        layout(xaxis = list(tickformat = ".2%")) %>%
        plotly_clean()
    })

    output$plot_edad <- renderPlotly({
      d <- filtered_data()
      f <- calc_frequency(d$polizas, d$siniestros, rango_edad) %>%
        arrange(rango_edad)
      plot_ly(f, x = ~rango_edad, y = ~frecuencia, type = "bar",
              marker = list(color = PALETTE$secondary)) %>%
        plotly_default_layout(xlab = "Rango de Edad", ylab = "Frecuencia") %>%
        layout(yaxis = list(tickformat = ".2%")) %>%
        plotly_clean()
    })

    output$plot_genero <- renderPlotly({
      d <- filtered_data()
      f <- calc_frequency(d$polizas, d$siniestros, genero)
      plot_ly(f, x = ~genero, y = ~frecuencia, type = "bar",
              marker = list(color = c(PALETTE$primary, PALETTE$secondary))) %>%
        plotly_default_layout(xlab = "Genero", ylab = "Frecuencia") %>%
        layout(yaxis = list(tickformat = ".2%")) %>%
        plotly_clean()
    })

    output$plot_canal <- renderPlotly({
      d <- filtered_data()
      f <- calc_frequency(d$polizas, d$siniestros, canal_venta) %>%
        arrange(desc(frecuencia))
      plot_ly(f, y = ~reorder(canal_venta, frecuencia), x = ~frecuencia,
              type = "bar", orientation = "h",
              marker = list(color = PALETTE$accent)) %>%
        plotly_default_layout(xlab = "Frecuencia", ylab = "") %>%
        layout(xaxis = list(tickformat = ".2%")) %>%
        plotly_clean()
    })

    output$tabla_freq <- renderDT({
      d <- filtered_data()
      f <- calc_frequency(d$polizas, d$siniestros, tipo_vehiculo, rango_edad) %>%
        arrange(desc(frecuencia))
      datatable(humanize_colnames(f), options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE) %>%
        formatPercentage("Frecuencia", 2)
    })
  })
}
