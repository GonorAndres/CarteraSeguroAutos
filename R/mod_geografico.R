# ==============================================================================
# MODULO: Analisis Geografico
# Uses leaflet if available, falls back to plotly
# ==============================================================================

HAS_LEAFLET <- requireNamespace("leaflet", quietly = TRUE) &&
               requireNamespace("rnaturalearth", quietly = TRUE)

geograficoUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Analisis Geografico"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(8, 4)),
      card(
        card_header(
          "Mapa de Riesgo por Estado",
          radioButtons(ns("metric"), NULL,
                       choices = c("Loss Ratio" = "loss_ratio",
                                   "Frecuencia" = "frecuencia",
                                   "Severidad" = "severidad_media",
                                   "Polizas" = "n_polizas"),
                       selected = "loss_ratio", inline = TRUE)
        ),
        if (HAS_LEAFLET) leaflet::leafletOutput(ns("mapa"), height = "500px")
        else plotlyOutput(ns("plot_mapa_fallback"), height = "500px")
      ),
      card(
        card_header("Detalle Estado"),
        htmlOutput(ns("estado_detalle"))
      )
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      card(
        card_header("Ranking de Estados"),
        plotlyOutput(ns("plot_ranking"), height = "400px")
      ),
      card(
        card_header("Frecuencia vs Severidad por Estado"),
        plotlyOutput(ns("plot_scatter"), height = "400px")
      )
    )
  )
}

geograficoServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    estado_stats <- reactive({
      d <- filtered_data()
      lr <- calc_loss_ratio(d$polizas, d$siniestros, estado)
      fr <- calc_frequency(d$polizas, d$siniestros, estado)
      sv <- d$siniestros %>%
        left_join(d$polizas %>% select(poliza_id, estado), by = "poliza_id") %>%
        group_by(estado) %>%
        summarise(severidad_media = mean(monto_siniestro, na.rm = TRUE), .groups = "drop")

      lr %>%
        left_join(fr %>% select(estado, frecuencia), by = "estado") %>%
        left_join(sv, by = "estado") %>%
        mutate(severidad_media = replace_na(severidad_media, 0))
    })

    # Leaflet map (when available)
    if (HAS_LEAFLET) {
      output$mapa <- leaflet::renderLeaflet({
        tryCatch({
          mexico_sf <- rnaturalearth::ne_states(country = "mexico", returnclass = "sf")
          name_map <- c(
            "Ciudad de Mexico" = "Distrito Federal",
            "Estado de Mexico" = "México",
            "Nuevo Leon" = "Nuevo León",
            "Michoacan" = "Michoacán",
            "Queretaro" = "Querétaro",
            "Yucatan" = "Yucatán",
            "San Luis Potosi" = "San Luis Potosí",
            "Coahuila" = "Coahuila de Zaragoza",
            "Veracruz" = "Veracruz de Ignacio de la Llave"
          )
          stats <- estado_stats()
          stats$name_sf <- ifelse(stats$estado %in% names(name_map),
                                   name_map[stats$estado], stats$estado)
          mexico_sf <- mexico_sf %>%
            dplyr::left_join(stats, by = c("name" = "name_sf"))

          metric <- input$metric %||% "loss_ratio"
          vals <- mexico_sf[[metric]]
          vals[is.na(vals)] <- 0
          pal <- leaflet::colorNumeric(
            c(PALETTE$success, PALETTE$accent, PALETTE$danger), vals, na.color = "#E0E0E0"
          )
          leaflet::leaflet(mexico_sf) %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(-102, 23.5, 5) %>%
            leaflet::addPolygons(fillColor = ~pal(vals), weight = 1, color = "white",
                                 fillOpacity = 0.7, label = ~paste0(name, ": ", round(vals, 3)))
        }, error = function(e) {
          leaflet::leaflet() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(-102, 23.5, 5)
        })
      })
    } else {
      # Fallback: plotly treemap/bar
      output$plot_mapa_fallback <- renderPlotly({
        stats <- estado_stats()
        metric <- input$metric %||% "loss_ratio"
        stats <- stats %>% arrange(desc(.data[[metric]]))

        colors <- sapply(stats[[metric]], function(v) {
          if (is.na(v)) PALETTE$muted
          else if (metric == "loss_ratio") {
            if (v <= 0.70) PALETTE$success else if (v <= 0.80) PALETTE$accent else PALETTE$danger
          } else PALETTE$primary
        })

        plot_ly(stats, y = ~reorder(estado, .data[[metric]]),
                x = ~.data[[metric]], type = "bar", orientation = "h",
                marker = list(color = colors),
                text = ~estado, customdata = ~estado,
                hovertemplate = "%{text}: %{x:.3f}<extra></extra>") %>%
          plotly_default_layout(xlab = METRIC_LABELS[metric] %||% metric, ylab = "") %>%
          layout(yaxis = list(tickfont = list(size = 11))) %>%
          plotly_clean()
      })
    }

    output$estado_detalle <- renderUI({
      stats <- estado_stats()
      # Show top state by selected metric
      metric <- input$metric %||% "loss_ratio"
      top <- stats %>% arrange(desc(.data[[metric]])) %>% slice(1)
      if (nrow(top) == 0) return(tags$p("Sin datos"))
      tags$div(
        tags$h5(top$estado),
        tags$hr(),
        tags$p(tags$strong("Polizas: "), format_num(top$n_polizas)),
        tags$p(tags$strong("Siniestros: "), format_num(top$n_siniestros)),
        tags$p(tags$strong("Loss Ratio: "), format_pct(top$loss_ratio)),
        tags$p(tags$strong("Frecuencia: "), format_pct(top$frecuencia)),
        tags$p(tags$strong("Severidad: "), format_currency_mxn(top$severidad_media)),
        tags$p(tags$strong("Prima: "), format_currency_mxn(top$prima_total))
      )
    })

    output$plot_ranking <- renderPlotly({
      stats <- estado_stats()
      metric <- input$metric %||% "loss_ratio"
      stats <- stats %>% arrange(desc(.data[[metric]]))
      plot_ly(stats, y = ~reorder(estado, .data[[metric]]),
              x = ~.data[[metric]], type = "bar", orientation = "h",
              marker = list(color = PALETTE$primary)) %>%
        plotly_default_layout(xlab = METRIC_LABELS[metric] %||% metric, ylab = "") %>%
        plotly_clean()
    })

    output$plot_scatter <- renderPlotly({
      stats <- estado_stats()
      plot_ly(stats, x = ~frecuencia, y = ~severidad_media,
              text = ~estado, type = "scatter", mode = "markers+text",
              textposition = "top center",
              marker = list(size = ~sqrt(n_polizas) / 3,
                            color = PALETTE$primary, opacity = 0.7)) %>%
        plotly_default_layout(xlab = "Frecuencia", ylab = "Severidad Media (MXN)") %>%
        layout(xaxis = list(tickformat = ".1%")) %>%
        plotly_clean()
    })
  })
}
