# ==============================================================================
# MODULO: Analisis Temporal
# ==============================================================================

temporalUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("An\u00e1lisis Temporal"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Siniestros por Mes (Año sobre Año)"),
        plotlyOutput(ns("plot_mensual"), height = "350px")
      ),
      card(
        card_header("Severidad Mensual Promedio"),
        plotlyOutput(ns("plot_sev_mes"), height = "350px")
      )
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Composici\u00f3n por Tipo y Mes"),
        plotlyOutput(ns("plot_tipo_mes"), height = "350px")
      ),
      card(
        card_header("D\u00edas de Reporte (Lag)"),
        plotlyOutput(ns("plot_lag"), height = "350px")
      )
    ),
    card(
      card_header("Resumen Mensual"),
      DTOutput(ns("tabla_mensual"))
    )
  )
}

temporalServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$plot_mensual <- renderPlotly({
      d <- filtered_data()
      mensual <- d$siniestros %>%
        mutate(anio = year(fecha_siniestro), mes = month(fecha_siniestro)) %>%
        count(anio, mes, name = "n")

      plot_ly(mensual, x = ~mes, y = ~n, color = ~factor(anio),
              colors = PALETTE_CATEGORICAL,
              type = "scatter", mode = "lines+markers") %>%
        plotly_default_layout(xlab = "Mes", ylab = "Siniestros") %>%
        layout(xaxis = list(dtick = 1)) %>%
        plotly_clean()
    })

    output$plot_sev_mes <- renderPlotly({
      d <- filtered_data()
      sev_mes <- d$siniestros %>%
        mutate(anio = year(fecha_siniestro), mes = month(fecha_siniestro)) %>%
        group_by(anio, mes) %>%
        summarise(sev_media = mean(monto_siniestro, na.rm = TRUE), .groups = "drop")

      plot_ly(sev_mes, x = ~mes, y = ~sev_media, color = ~factor(anio),
              colors = PALETTE_CATEGORICAL,
              type = "scatter", mode = "lines+markers") %>%
        plotly_default_layout(xlab = "Mes", ylab = "Severidad Media (MXN)") %>%
        layout(xaxis = list(dtick = 1)) %>%
        plotly_clean()
    })

    output$plot_tipo_mes <- renderPlotly({
      d <- filtered_data()
      tipo_mes <- d$siniestros %>%
        mutate(
          anio = year(fecha_siniestro),
          mes = month(fecha_siniestro),
          periodo = paste0(anio, "-", sprintf("%02d", mes))
        ) %>%
        count(periodo, tipo_siniestro, name = "n")

      plot_ly(tipo_mes, x = ~periodo, y = ~n, color = ~tipo_siniestro,
              colors = PALETTE_CATEGORICAL,
              type = "bar") %>%
        plotly_default_layout(xlab = "Periodo", ylab = "Siniestros") %>%
        layout(barmode = "stack", xaxis = list(tickangle = -45)) %>%
        plotly_clean()
    })

    output$plot_lag <- renderPlotly({
      d <- filtered_data()
      lag_data <- d$siniestros %>%
        mutate(dias_reporte = as.numeric(fecha_reporte - fecha_siniestro))

      plot_ly(lag_data, x = ~dias_reporte, type = "histogram",
              nbinsx = 16,
              marker = list(color = PALETTE$primary)) %>%
        plotly_default_layout(xlab = "D\u00edas entre Ocurrencia y Reporte", ylab = "Frecuencia") %>%
        plotly_clean()
    })

    output$tabla_mensual <- renderDT({
      d <- filtered_data()
      resumen <- d$siniestros %>%
        mutate(anio = year(fecha_siniestro), mes = month(fecha_siniestro)) %>%
        group_by(anio, mes) %>%
        summarise(
          n_siniestros = n(),
          monto_total = sum(monto_siniestro, na.rm = TRUE),
          severidad_media = mean(monto_siniestro, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(anio, mes)

      datatable(resumen, options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE) %>%
        formatCurrency(c("monto_total", "severidad_media"), currency = "$", digits = 0)
    })
  })
}
