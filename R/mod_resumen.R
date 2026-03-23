# ==============================================================================
# MODULO: Resumen Ejecutivo
# ==============================================================================

resumenUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Resumen Ejecutivo"),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 4),
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
      col_widths = breakpoints(sm = 12, md = 4),
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

    card(
      class = "mb-4 border-0",
      card_body(
        class = "py-3",
        tags$div(
          class = "lh-lg",
          style = "max-width: 960px;",

          tags$p(
            "Se parte de datos sint\u00e9ticos generados con par\u00e1metros de la AMIS y CONDUSEF;",
            "as\u00ed se evita la falta de datos reales pero las cifras se ajustan de manera veros\u00edmil",
            "al mercado mexicano de autos (140 mil p\u00f3lizas, 5 a\u00f1os de operaci\u00f3n, renovaciones e inflaci\u00f3n).",
            "Cada", tags$strong("p\u00f3liza"), "representa un contrato donde el asegurado paga una",
            tags$strong("prima"), "(el precio de su cobertura)",
            "a cambio de que la aseguradora responda ante un", tags$strong("siniestro"),
            "(colisi\u00f3n, robo, da\u00f1os, incendio).",
            "La pregunta central del portafolio es:",
            tags$em("\u00bflo que se cobra en primas alcanza para cubrir lo que se paga en siniestros?")
          ),

          tags$p(
            "Los indicadores de arriba resumen la respuesta. El", tags$strong("Loss Ratio"),
            "es la proporci\u00f3n entre lo que se paga en siniestros y lo que se cobra en primas:",
            "si es 70%, de cada peso cobrado se destinan 70 centavos a pagar reclamos.",
            "Un loss ratio por debajo de ~75% indica un portafolio t\u00e9cnicamente sano;",
            "por encima, la operaci\u00f3n pierde dinero. La", tags$strong("Frecuencia"),
            "mide qu\u00e9 tan seguido ocurren siniestros por p\u00f3liza expuesta, y la",
            tags$strong("Severidad"), "mide cu\u00e1nto cuesta en promedio cada uno.",
            "Prima = Frecuencia x Severidad es la ecuaci\u00f3n fundamental de tarificaci\u00f3n en seguros."
          ),

          tags$p(
            "Las primeras pesta\u00f1as descomponen estos indicadores desde distintos \u00e1ngulos.",
            tags$strong("Loss Ratio"), "los segmenta por canal, veh\u00edculo y a\u00f1o.",
            tags$strong("Frecuencia"), "y", tags$strong("Severidad"),
            "detallan patrones por tipo de veh\u00edculo, edad del conductor y tipo de siniestro.",
            tags$strong("Temporal"), "revela estacionalidad (en M\u00e9xico, junio-octubre es temporada alta por lluvias).",
            tags$strong("Geogr\u00e1fico"), "muestra c\u00f3mo var\u00eda el riesgo entre estados (CDMX y EdoMex concentran la mayor siniestralidad).",
            tags$strong("Segmentaci\u00f3n"), "cruza edad contra veh\u00edculo para identificar celdas de riesgo."
          ),

          tags$p(
            "El men\u00fa", tags$strong("Actuarial"), "contiene modelos m\u00e1s profundos.",
            tags$strong("Pricing GLM"), "ajusta modelos estad\u00edsticos (Poisson para frecuencia, Gamma para severidad)",
            "que calculan la prima t\u00e9cnica por perfil de riesgo.",
            tags$strong("Reservas IBNR"), "estima cu\u00e1nto dinero falta por pagar en siniestros ya ocurridos",
            "pero a\u00fan no reportados o no completamente desarrollados, usando tri\u00e1ngulos de desarrollo Chain Ladder",
            "y Bornhuetter-Ferguson.",
            tags$strong("Escenarios"), "simula miles de futuros posibles con Monte Carlo para calcular",
            "el VaR y TVaR: las p\u00e9rdidas m\u00e1ximas esperadas en el peor 0.5% de los casos.",
            "Finalmente,", tags$strong("Fraude"), "asigna un score de anomal\u00eda a cada siniestro",
            "combinando distancia estad\u00edstica (Mahalanobis) con reglas de negocio."
          ),

          tags$p(
            class = "text-muted mb-0",
            "Usa los filtros del panel izquierdo para acotar por fecha, estado, tipo de veh\u00edculo,",
            "canal o edad. Todos los gr\u00e1ficos y m\u00e9tricas se actualizan en tiempo real."
          )
        )
      )
    ),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Loss Ratio por Canal de Venta"),
        plotlyOutput(ns("plot_lr_canal"), height = "320px")
      ),
      card(
        card_header("Distribuci\u00f3n de Siniestros por Tipo"),
        plotlyOutput(ns("plot_dist_tipo"), height = "380px")
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
        layout(yaxis = list(tickformat = ".1%")) %>%
        plotly_clean()
    })

    output$plot_dist_tipo <- renderPlotly({
      d <- filtered_data()
      validate(need(nrow(d$siniestros) > 0, "Sin siniestros para mostrar."))
      dist <- d$siniestros %>%
        count(tipo_siniestro, name = "n")

      plot_ly(dist, labels = ~tipo_siniestro, values = ~n, type = "pie",
              marker = list(colors = PALETTE_CATEGORICAL[1:nrow(dist)])) %>%
        plotly_default_layout() %>%
        layout(legend = list(orientation = "v", x = 1.02, y = 0.5)) %>%
        plotly_clean()
    })

    output$plot_tendencia <- renderPlotly({
      d <- filtered_data()
      mensual <- d$siniestros %>%
        mutate(anio = year(fecha_siniestro), mes = month(fecha_siniestro)) %>%
        count(anio, mes, name = "n_siniestros")

      if (length(unique(mensual$anio)) > 1) {
        plot_ly(mensual, x = ~mes, y = ~n_siniestros, color = ~factor(anio),
                colors = PALETTE_CATEGORICAL, type = "scatter", mode = "lines+markers") %>%
          plotly_default_layout(xlab = "Mes", ylab = "Siniestros") %>%
          plotly_clean()
      } else {
        plot_ly(mensual, x = ~mes, y = ~n_siniestros, type = "scatter",
                mode = "lines+markers",
                line = list(color = PALETTE$primary),
                marker = list(color = PALETTE$primary)) %>%
          plotly_default_layout(xlab = "Mes", ylab = "Siniestros") %>%
          plotly_clean()
      }
    })
  })
}
