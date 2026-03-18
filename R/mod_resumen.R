# ==============================================================================
# MODULO: Resumen Ejecutivo
# ==============================================================================

resumenUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Total Polizas", value = textOutput(ns("vb_polizas")),
        showcase = icon("file-contract"), theme = "primary"
      ),
      value_box(
        title = "Total Siniestros", value = textOutput(ns("vb_siniestros")),
        showcase = icon("car-burst"), theme = "danger"
      ),
      value_box(
        title = "Loss Ratio", value = textOutput(ns("vb_lr")),
        showcase = icon("percent"), theme = "info"
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Frecuencia", value = textOutput(ns("vb_freq")),
        showcase = icon("chart-line"), theme = "warning"
      ),
      value_box(
        title = "Severidad Promedio", value = textOutput(ns("vb_sev")),
        showcase = icon("dollar-sign"), theme = "info"
      ),
      value_box(
        title = "Prima Total", value = textOutput(ns("vb_prima")),
        showcase = icon("money-bill-wave"), theme = "success"
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Loss Ratio por Canal de Venta"),
        plotlyOutput(ns("plot_lr_canal"), height = "320px")
      ),
      card(
        card_header("Distribucion de Siniestros por Tipo"),
        plotlyOutput(ns("plot_dist_tipo"), height = "320px")
      )
    ),
    card(
      card_header("Tendencia Mensual de Siniestros"),
      plotlyOutput(ns("plot_tendencia"), height = "320px")
    )
  )
}

resumenServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    kpis <- reactive({
      d <- filtered_data()
      calc_kpis(d$polizas, d$siniestros)
    })

    output$vb_polizas <- renderText(format_num(kpis()$n_polizas))
    output$vb_siniestros <- renderText(format_num(kpis()$n_siniestros))
    output$vb_lr <- renderText(format_pct(kpis()$loss_ratio))
    output$vb_freq <- renderText(format_pct(kpis()$frecuencia))
    output$vb_sev <- renderText(format_currency_mxn(kpis()$severidad_media))
    output$vb_prima <- renderText(format_currency_millions(kpis()$prima_total))

    output$plot_lr_canal <- renderPlotly({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, canal_venta)

      plot_ly(lr, x = ~canal_venta, y = ~loss_ratio, type = "bar",
              marker = list(color = PALETTE$primary)) %>%
        plotly_default_layout(xlab = "Canal", ylab = "Loss Ratio") %>%
        layout(yaxis = list(tickformat = ".1%"))
    })

    output$plot_dist_tipo <- renderPlotly({
      d <- filtered_data()
      dist <- d$siniestros %>%
        count(tipo_siniestro, name = "n")

      plot_ly(dist, labels = ~tipo_siniestro, values = ~n, type = "pie",
              marker = list(colors = PALETTE_CATEGORICAL[1:nrow(dist)])) %>%
        plotly_default_layout()
    })

    output$plot_tendencia <- renderPlotly({
      d <- filtered_data()
      mensual <- d$siniestros %>%
        mutate(anio = year(fecha_siniestro), mes = month(fecha_siniestro)) %>%
        count(anio, mes, name = "n_siniestros")

      if (length(unique(mensual$anio)) > 1) {
        plot_ly(mensual, x = ~mes, y = ~n_siniestros, color = ~factor(anio),
                colors = PALETTE_CATEGORICAL, type = "scatter", mode = "lines+markers") %>%
          plotly_default_layout(xlab = "Mes", ylab = "Siniestros")
      } else {
        plot_ly(mensual, x = ~mes, y = ~n_siniestros, type = "scatter",
                mode = "lines+markers",
                line = list(color = PALETTE$primary),
                marker = list(color = PALETTE$primary)) %>%
          plotly_default_layout(xlab = "Mes", ylab = "Siniestros")
      }
    })
  })
}
