# ==============================================================================
# MODULO: Deteccion de Fraude / Anomalias
# Basado en distancia de Mahalanobis y reglas heuristicas
# ==============================================================================

fraudUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Deteccion de Fraude y Anomalias"),

    # Value boxes
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Siniestros Analizados",
        value = textOutput(ns("vb_total")),
        showcase = icon("magnifying-glass"),
        theme = "primary"
      ),
      value_box(
        title = "Flaggeados (Score > 0.7)",
        value = textOutput(ns("vb_flagged")),
        showcase = icon("flag"),
        theme = "danger"
      ),
      value_box(
        title = "% Flaggeados",
        value = textOutput(ns("vb_pct_flagged")),
        showcase = icon("percent"),
        theme = "warning"
      ),
      value_box(
        title = "Score Promedio",
        value = textOutput(ns("vb_avg_score")),
        showcase = icon("gauge-high"),
        theme = "info"
      )
    ),

    # Graficos y tabla
    navset_card_tab(
      title = "Resultados del Analisis",
      nav_panel(
        "Top Anomalias",
        DTOutput(ns("table_anomalies"))
      ),
      nav_panel(
        "Distribucion de Flags",
        plotlyOutput(ns("plot_flags"), height = "420px")
      ),
      nav_panel(
        "Distribucion de Score",
        plotlyOutput(ns("plot_score_hist"), height = "420px")
      ),
      nav_panel(
        "Score vs Monto",
        plotlyOutput(ns("plot_score_scatter"), height = "420px")
      )
    )
  )
}

fraudServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    # --- Calcular scores de anomalia ---
    scored_data <- reactive({
      tryCatch({
        d <- filtered_data()
        siniestros <- d$siniestros
        polizas <- d$polizas

        req(nrow(siniestros) > 5)
        req(nrow(polizas) > 0)

        # Preparar datos: unir con polizas para suma_asegurada y fecha_inicio
        df <- siniestros %>%
          left_join(
            polizas %>% select(poliza_id, suma_asegurada, fecha_inicio),
            by = "poliza_id"
          ) %>%
          mutate(
            dias_reporte = as.numeric(fecha_reporte - fecha_siniestro)
          ) %>%
          filter(
            !is.na(monto_siniestro),
            !is.na(dias_reporte),
            !is.na(deducible)
          )

        req(nrow(df) > 5)

        # ================================================================
        # MAHALANOBIS POR TIPO DE SINIESTRO
        # ================================================================
        df <- df %>%
          group_by(tipo_siniestro) %>%
          mutate(
            mahal_dist = tryCatch({
              cols <- cbind(monto_siniestro, dias_reporte, deducible)
              mu <- colMeans(cols, na.rm = TRUE)
              sigma <- cov(cols, use = "pairwise.complete.obs")
              # Regularizar si la matriz es singular
              if (det(sigma) < 1e-10) {
                sigma <- sigma + diag(1e-6, ncol(sigma))
              }
              mahalanobis(cols, center = mu, cov = sigma)
            }, error = function(e) {
              rep(0, n())
            })
          ) %>%
          ungroup()

        # Percentil de Mahalanobis (global)
        df <- df %>%
          mutate(
            mahal_percentile = percent_rank(mahal_dist)
          )

        # ================================================================
        # FLAGS BASADAS EN REGLAS
        # ================================================================

        # Flag 1: Multiples siniestros en la misma poliza dentro de 60 dias
        df <- df %>%
          group_by(poliza_id) %>%
          arrange(poliza_id, fecha_siniestro) %>%
          mutate(
            flag_multiple = {
              n_claims <- n()
              if (n_claims < 2) {
                rep(FALSE, n_claims)
              } else {
                fechas <- fecha_siniestro
                result <- logical(n_claims)
                for (i in seq_len(n_claims)) {
                  diffs <- abs(as.numeric(fechas[i] - fechas[-i]))
                  result[i] <- any(diffs <= 60)
                }
                result
              }
            }
          ) %>%
          ungroup()

        # Flag 2: Siniestro dentro de 30 dias del inicio de poliza
        df <- df %>%
          mutate(
            flag_inception = !is.na(fecha_inicio) &
              as.numeric(fecha_siniestro - fecha_inicio) <= 30 &
              as.numeric(fecha_siniestro - fecha_inicio) >= 0
          )

        # Flag 3: Severidad > 3x mediana por tipo de siniestro
        medianas_tipo <- df %>%
          group_by(tipo_siniestro) %>%
          summarise(mediana_tipo = median(monto_siniestro, na.rm = TRUE), .groups = "drop")

        df <- df %>%
          left_join(medianas_tipo, by = "tipo_siniestro") %>%
          mutate(
            flag_severity = monto_siniestro > 3 * mediana_tipo
          ) %>%
          select(-mediana_tipo)

        # Flag 4: Retraso en reporte > 10 dias
        df <- df %>%
          mutate(
            flag_delay = dias_reporte > 10
          )

        # Flag 5: Monto > 90% de suma asegurada
        df <- df %>%
          mutate(
            flag_sum_insured = !is.na(suma_asegurada) & suma_asegurada > 0 &
              monto_siniestro > 0.90 * suma_asegurada
          )

        # ================================================================
        # SCORE COMPUESTO
        # ================================================================
        df <- df %>%
          mutate(
            n_flags = as.integer(flag_multiple) + as.integer(flag_inception) +
              as.integer(flag_severity) + as.integer(flag_delay) +
              as.integer(flag_sum_insured),
            score_fraude = 0.4 * mahal_percentile + 0.6 * (n_flags / 5)
          ) %>%
          arrange(desc(score_fraude))

        df
      }, error = function(e) {
        showNotification(
          paste("Error en analisis de fraude:", e$message),
          type = "error"
        )
        tibble()
      })
    })

    # --- Value boxes ---
    output$vb_total <- renderText({
      df <- scored_data()
      if (nrow(df) == 0) return("--")
      format_num(nrow(df))
    })

    output$vb_flagged <- renderText({
      df <- scored_data()
      if (nrow(df) == 0) return("--")
      format_num(sum(df$score_fraude > 0.7, na.rm = TRUE))
    })

    output$vb_pct_flagged <- renderText({
      df <- scored_data()
      if (nrow(df) == 0) return("--")
      pct <- mean(df$score_fraude > 0.7, na.rm = TRUE)
      format_pct(pct)
    })

    output$vb_avg_score <- renderText({
      df <- scored_data()
      if (nrow(df) == 0) return("--")
      sprintf("%.3f", mean(df$score_fraude, na.rm = TRUE))
    })

    # --- Tabla de anomalias ---
    output$table_anomalies <- renderDT({
      df <- scored_data()
      req(nrow(df) > 0)

      tabla <- df %>%
        select(
          siniestro_id, poliza_id, tipo_siniestro,
          monto_siniestro, dias_reporte,
          flag_multiple, flag_inception, flag_severity,
          flag_delay, flag_sum_insured,
          n_flags, score_fraude
        ) %>%
        head(500)

      datatable(
        tabla,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(11, "desc")),
          columnDefs = list(
            list(className = "dt-center", targets = 5:9),
            list(className = "dt-right", targets = c(3, 4, 10, 11))
          )
        ),
        colnames = c(
          "Siniestro ID", "Poliza ID", "Tipo Siniestro",
          "Monto Siniestro", "Dias Reporte",
          "Flag Multiple", "Flag Inicio", "Flag Severidad",
          "Flag Retraso", "Flag Suma Aseg.",
          "Num Flags", "Score Fraude"
        )
      ) %>%
        formatCurrency("monto_siniestro", currency = "$", digits = 0) %>%
        formatRound("score_fraude", digits = 3) %>%
        formatStyle(
          "score_fraude",
          backgroundColor = styleInterval(
            c(0.3, 0.5, 0.7),
            c("white", "#FFF3CD", "#FFDDAA", "#F8D7DA")
          )
        ) %>%
        formatStyle(
          columns = c("flag_multiple", "flag_inception", "flag_severity",
                       "flag_delay", "flag_sum_insured"),
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#F8D7DA", "white"))
        )
    })

    # --- Plot: Distribucion de flags ---
    output$plot_flags <- renderPlotly({
      df <- scored_data()
      req(nrow(df) > 0)

      flag_counts <- tibble(
        Flag = c(
          "Multiple (60 dias)",
          "Inicio poliza (30 dias)",
          "Severidad (>3x med.)",
          "Retraso reporte (>10d)",
          "Suma asegurada (>90%)"
        ),
        Cantidad = c(
          sum(df$flag_multiple, na.rm = TRUE),
          sum(df$flag_inception, na.rm = TRUE),
          sum(df$flag_severity, na.rm = TRUE),
          sum(df$flag_delay, na.rm = TRUE),
          sum(df$flag_sum_insured, na.rm = TRUE)
        ),
        Porcentaje = c(
          mean(df$flag_multiple, na.rm = TRUE),
          mean(df$flag_inception, na.rm = TRUE),
          mean(df$flag_severity, na.rm = TRUE),
          mean(df$flag_delay, na.rm = TRUE),
          mean(df$flag_sum_insured, na.rm = TRUE)
        )
      ) %>%
        arrange(desc(Cantidad))

      plot_ly(
        flag_counts,
        y = ~reorder(Flag, Cantidad),
        x = ~Cantidad,
        type = "bar",
        orientation = "h",
        marker = list(color = PALETTE$danger),
        text = ~paste0(format_num(Cantidad), " (", format_pct(Porcentaje), ")"),
        textposition = "outside"
      ) %>%
        plotly_default_layout(
          title = "Siniestros por Tipo de Flag",
          xlab = "Numero de Siniestros",
          ylab = NULL
        )
    })

    # --- Plot: Histograma de score ---
    output$plot_score_hist <- renderPlotly({
      df <- scored_data()
      req(nrow(df) > 0)

      plot_ly(
        x = df$score_fraude,
        type = "histogram",
        marker = list(
          color = PALETTE$primary,
          line = list(color = "white", width = 0.5)
        ),
        nbinsx = 50
      ) %>%
        add_trace(
          x = c(0.7, 0.7), y = c(0, max(table(cut(df$score_fraude, 50)))),
          type = "scatter", mode = "lines",
          line = list(color = PALETTE$danger, width = 2, dash = "dash"),
          name = "Umbral (0.7)",
          showlegend = TRUE
        ) %>%
        plotly_default_layout(
          title = "Distribucion del Score de Fraude",
          xlab = "Score de Fraude",
          ylab = "Frecuencia"
        ) %>%
        layout(
          shapes = list(
            list(
              type = "line", x0 = 0.7, x1 = 0.7,
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = PALETTE$danger, width = 2, dash = "dash")
            )
          ),
          annotations = list(
            list(
              x = 0.72, y = 0.95, yref = "paper",
              text = "Umbral 0.7", showarrow = FALSE,
              font = list(color = PALETTE$danger, size = 11)
            )
          ),
          showlegend = FALSE
        )
    })

    # --- Plot: Score vs Monto (scatter) ---
    output$plot_score_scatter <- renderPlotly({
      df <- scored_data()
      req(nrow(df) > 0)

      # Limitar a 2000 puntos para rendimiento
      df_plot <- if (nrow(df) > 2000) {
        df %>% slice_sample(n = 2000)
      } else {
        df
      }

      plot_ly(
        df_plot,
        x = ~score_fraude,
        y = ~monto_siniestro,
        color = ~tipo_siniestro,
        colors = PALETTE_CATEGORICAL,
        type = "scatter",
        mode = "markers",
        marker = list(size = 5, opacity = 0.6),
        text = ~paste0(
          "Siniestro: ", siniestro_id,
          "<br>Poliza: ", poliza_id,
          "<br>Monto: ", format_currency_mxn(monto_siniestro),
          "<br>Tipo: ", tipo_siniestro,
          "<br>Score: ", sprintf("%.3f", score_fraude),
          "<br>Flags: ", n_flags
        ),
        hoverinfo = "text"
      ) %>%
        plotly_default_layout(
          title = "Score de Fraude vs Monto del Siniestro",
          xlab = "Score de Fraude",
          ylab = "Monto Siniestro (MXN)"
        ) %>%
        layout(
          shapes = list(
            list(
              type = "line", x0 = 0.7, x1 = 0.7,
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = PALETTE$danger, width = 1.5, dash = "dash")
            )
          ),
          legend = list(x = 0.01, y = 0.99)
        )
    })
  })
}
