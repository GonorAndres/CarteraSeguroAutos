# ==============================================================================
# MODULO: Analisis de Loss Ratio
# ==============================================================================

lossRatioUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Loss Ratio por Canal de Venta"),
        plotlyOutput(ns("plot_canal"), height = "350px")
      ),
      card(
        card_header("Loss Ratio por Tipo de Vehiculo"),
        plotlyOutput(ns("plot_vehiculo"), height = "350px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Loss Ratio por Marca"),
        plotlyOutput(ns("plot_marca"), height = "350px")
      ),
      card(
        card_header("Loss Ratio por Año de Suscripcion"),
        plotlyOutput(ns("plot_anio"), height = "350px")
      )
    ),
    card(
      card_header("Detalle Loss Ratio"),
      DTOutput(ns("tabla_detalle"))
    )
  )
}

lossRatioServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$plot_canal <- renderPlotly({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, canal_venta) %>%
        arrange(desc(loss_ratio))
      plot_ly(lr, y = ~reorder(canal_venta, loss_ratio), x = ~loss_ratio,
              type = "bar", orientation = "h",
              marker = list(color = PALETTE$primary)) %>%
        plotly_default_layout(xlab = "Loss Ratio", ylab = "") %>%
        layout(xaxis = list(tickformat = ".1%"))
    })

    output$plot_vehiculo <- renderPlotly({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, tipo_vehiculo) %>%
        arrange(desc(loss_ratio))
      plot_ly(lr, y = ~reorder(tipo_vehiculo, loss_ratio), x = ~loss_ratio,
              type = "bar", orientation = "h",
              marker = list(color = PALETTE$secondary)) %>%
        plotly_default_layout(xlab = "Loss Ratio", ylab = "") %>%
        layout(xaxis = list(tickformat = ".1%"))
    })

    output$plot_marca <- renderPlotly({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, marca_vehiculo) %>%
        arrange(desc(loss_ratio))
      plot_ly(lr, y = ~reorder(marca_vehiculo, loss_ratio), x = ~loss_ratio,
              type = "bar", orientation = "h",
              marker = list(color = PALETTE$accent)) %>%
        plotly_default_layout(xlab = "Loss Ratio", ylab = "") %>%
        layout(xaxis = list(tickformat = ".1%"))
    })

    output$plot_anio <- renderPlotly({
      d <- filtered_data()
      if (!"anio_suscripcion" %in% names(d$polizas)) return(plotly_empty())
      lr <- calc_loss_ratio(d$polizas, d$siniestros, anio_suscripcion) %>%
        arrange(anio_suscripcion)
      plot_ly(lr, x = ~factor(anio_suscripcion), y = ~loss_ratio,
              type = "bar", marker = list(color = PALETTE$success)) %>%
        plotly_default_layout(xlab = "Año", ylab = "Loss Ratio") %>%
        layout(yaxis = list(tickformat = ".1%"))
    })

    output$tabla_detalle <- renderDT({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, canal_venta, tipo_vehiculo) %>%
        arrange(desc(loss_ratio))
      datatable(lr, options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE) %>%
        formatPercentage("loss_ratio", 2) %>%
        formatCurrency(c("prima_total", "siniestros_total"), currency = "$", digits = 0)
    })
  })
}
