# ==============================================================================
# MODULO: Segmentacion de Riesgo
# ==============================================================================

segmentacionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Segmentacion de Riesgo"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Loss Ratio por Segmento de Edad"),
        plotlyOutput(ns("plot_edad"), height = "350px")
      ),
      card(
        card_header("Frecuencia por Score Crediticio"),
        plotlyOutput(ns("plot_score"), height = "350px")
      )
    ),
    card(
      card_header("Matriz de Riesgo: Edad x Tipo Vehiculo"),
      plotlyOutput(ns("plot_heatmap"), height = "450px")
    ),
    card(
      card_header("Rentabilidad por Segmento"),
      DTOutput(ns("tabla_segmentos"))
    )
  )
}

segmentacionServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$plot_edad <- renderPlotly({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, segmento_edad) %>%
        arrange(loss_ratio)

      colors <- sapply(lr$loss_ratio, function(v) {
        if (is.na(v)) PALETTE$muted
        else if (v <= 0.70) PALETTE$success
        else if (v <= 0.80) PALETTE$accent
        else PALETTE$danger
      })

      plot_ly(lr, y = ~reorder(segmento_edad, loss_ratio), x = ~loss_ratio,
              type = "bar", orientation = "h",
              marker = list(color = colors)) %>%
        plotly_default_layout(xlab = "Loss Ratio", ylab = "") %>%
        layout(xaxis = list(tickformat = ".1%")) %>%
        plotly_clean()
    })

    output$plot_score <- renderPlotly({
      d <- filtered_data()
      f <- calc_frequency(d$polizas, d$siniestros, segmento_score) %>%
        arrange(frecuencia)

      plot_ly(f, x = ~segmento_score, y = ~frecuencia, type = "bar",
              marker = list(color = PALETTE$secondary)) %>%
        plotly_default_layout(xlab = "Segmento Score", ylab = "Frecuencia") %>%
        layout(yaxis = list(tickformat = ".2%")) %>%
        plotly_clean()
    })

    output$plot_heatmap <- renderPlotly({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, segmento_edad, tipo_vehiculo)

      matrix_data <- lr %>%
        select(segmento_edad, tipo_vehiculo, loss_ratio) %>%
        pivot_wider(names_from = tipo_vehiculo, values_from = loss_ratio)

      rows <- matrix_data$segmento_edad
      cols <- setdiff(names(matrix_data), "segmento_edad")
      vals <- as.matrix(matrix_data[, cols])

      plot_ly(z = vals, x = cols, y = rows, type = "heatmap",
              colorscale = list(c(0, PALETTE$success), c(0.5, PALETTE$accent), c(1, PALETTE$danger)),
              text = apply(vals, c(1, 2), function(v) if (is.na(v)) "N/D" else format_pct(v)),
              hoverinfo = "text",
              showscale = TRUE) %>%
        plotly_default_layout(xlab = "Tipo Vehiculo", ylab = "Segmento Edad") %>%
        layout(coloraxis = list(colorbar = list(title = "Loss Ratio"))) %>%
        plotly_clean()
    })

    output$tabla_segmentos <- renderDT({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, segmento_edad, tipo_vehiculo) %>%
        arrange(desc(loss_ratio))

      datatable(humanize_colnames(lr), options = list(pageLength = 20, scrollX = TRUE),
                rownames = FALSE) %>%
        formatPercentage("Loss Ratio", 2) %>%
        formatCurrency(c("Prima Total", "Siniestros Total"), currency = "$", digits = 0)
    })
  })
}
