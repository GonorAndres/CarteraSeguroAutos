# ==============================================================================
# MODULO: Deteccion de Fraude / Anomalias
# Basado en distancia de Mahalanobis y reglas heuristicas
# ==============================================================================

fraudUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Detecci\u00f3n de Fraude y Anomal\u00edas"),

    # Value boxes
    layout_columns(
      col_widths = breakpoints(sm = 6, md = 3),
      value_box(
        title = "Siniestros Analizados",
        value = textOutput(ns("vb_total")),
        showcase = icon("magnifying-glass"),
        theme = "primary"
      ),
      value_box(
        title = paste0("Flaggeados (Score > ", FRAUD_CONFIG$score_threshold, ")"),
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
      title = "Resultados del An\u00e1lisis",
      nav_panel(
        "Top Anomal\u00edas",
        DTOutput(ns("table_anomalies"))
      ),
      nav_panel(
        "Distribuci\u00f3n de Flags",
        plotlyOutput(ns("plot_flags"), height = "420px")
      ),
      nav_panel(
        "Distribuci\u00f3n de Score",
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
            mahal_dist = {
              nn <- n()
              if (nn < 10) {
                rep(NA_real_, nn)
              } else {
                tryCatch({
                  cols <- cbind(monto_siniestro, dias_reporte, deducible)
                  mu <- colMeans(cols, na.rm = TRUE)
                  sigma <- cov(cols, use = "pairwise.complete.obs")
                  if (det(sigma) < 1e-10) {
                    sigma <- sigma + diag(1e-6, ncol(sigma))
                  }
                  mahalanobis(cols, center = mu, cov = sigma)
                }, error = function(e) {
                  rep(NA_real_, nn)
                })
              }
            }
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

        # Flag 1: Multiples siniestros en la misma poliza dentro de window
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
                  result[i] <- any(diffs <= FRAUD_CONFIG$multiple_claims_days)
                }
                result
              }
            }
          ) %>%
          ungroup()

        # Flag 2: Siniestro dentro de inception window del inicio de poliza
        df <- df %>%
          mutate(
            flag_inception = !is.na(fecha_inicio) &
              as.numeric(fecha_siniestro - fecha_inicio) <= FRAUD_CONFIG$inception_days &
              as.numeric(fecha_siniestro - fecha_inicio) >= 0
          )

        # Flag 3: Severidad > multiplier x mediana por tipo de siniestro
        medianas_tipo <- df %>%
          group_by(tipo_siniestro) %>%
          summarise(mediana_tipo = median(monto_siniestro, na.rm = TRUE), .groups = "drop")

        df <- df %>%
          left_join(medianas_tipo, by = "tipo_siniestro") %>%
          mutate(
            flag_severity = monto_siniestro > FRAUD_CONFIG$severity_multiplier * mediana_tipo
          ) %>%
          select(-mediana_tipo)

        # Flag 4: Retraso en reporte > threshold
        df <- df %>%
          mutate(
            flag_delay = dias_reporte > FRAUD_CONFIG$reporting_delay_days
          )

        # Flag 5: Monto > ratio of suma asegurada
        df <- df %>%
          mutate(
            flag_sum_insured = !is.na(suma_asegurada) & suma_asegurada > 0 &
              monto_siniestro > FRAUD_CONFIG$sum_insured_ratio * suma_asegurada &
              tipo_siniestro != "Robo Total"
          )

        # ================================================================
        # SCORE COMPUESTO
        # ================================================================
        df <- df %>%
          mutate(
            n_flags = as.integer(flag_multiple) + as.integer(flag_inception) +
              as.integer(flag_severity) + as.integer(flag_delay) +
              as.integer(flag_sum_insured),
            score_fraude = FRAUD_CONFIG$mahal_weight * mahal_percentile +
              FRAUD_CONFIG$rules_weight * (n_flags / FRAUD_CONFIG$n_rules)
          ) %>%
          arrange(desc(score_fraude))

        df
      }, error = function(e) {
        showNotification(
          paste("Error en an\u00e1lisis de fraude:", e$message),
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
      format_num(sum(df$score_fraude > FRAUD_CONFIG$score_threshold, na.rm = TRUE))
    })

    output$vb_pct_flagged <- renderText({
      df <- scored_data()
      if (nrow(df) == 0) return("--")
      pct <- mean(df$score_fraude > FRAUD_CONFIG$score_threshold, na.rm = TRUE)
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
            c(0.3, 0.5, FRAUD_CONFIG$score_threshold),
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
          paste0("M\u00faltiple (", FRAUD_CONFIG$multiple_claims_days, " d\u00edas)"),
          paste0("Inicio p\u00f3liza (", FRAUD_CONFIG$inception_days, " d\u00edas)"),
          paste0("Severidad (>", FRAUD_CONFIG$severity_multiplier, "x med.)"),
          paste0("Retraso reporte (>", FRAUD_CONFIG$reporting_delay_days, "d)"),
          paste0("Suma asegurada (>", FRAUD_CONFIG$sum_insured_ratio * 100, "%)")
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
          xlab = "N\u00famero de Siniestros",
          ylab = NULL
        )
    })

    # --- Plot: Histograma de score ---
    output$plot_score_hist <- renderPlotly({
      df <- scored_data()
      req(nrow(df) > 0)

      thresh <- FRAUD_CONFIG$score_threshold
      plot_ly(
        x = df$score_fraude,
        type = "histogram",
        marker = list(
          color = PALETTE$primary,
          line = list(color = "white", width = 0.5)
        ),
        nbinsx = 50
      ) %>%
        plotly_default_layout(
          title = "Distribuci\u00f3n del Score de Fraude",
          xlab = "Score de Fraude",
          ylab = "Frecuencia"
        ) %>%
        layout(
          shapes = list(
            list(
              type = "line", x0 = thresh, x1 = thresh,
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = PALETTE$danger, width = 2, dash = "dash")
            )
          ),
          annotations = list(
            list(
              x = thresh + 0.02, y = 0.95, yref = "paper",
              text = paste("Umbral", thresh), showarrow = FALSE,
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
              type = "line", x0 = FRAUD_CONFIG$score_threshold, x1 = FRAUD_CONFIG$score_threshold,
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = PALETTE$danger, width = 1.5, dash = "dash")
            )
          ),
          legend = list(x = 0.01, y = 0.99)
        )
    })
  })
}
