# ==============================================================================
# MODULO: Analisis de Escenarios / Stress Testing
# Modelo colectivo de riesgo con simulacion Monte Carlo
# ==============================================================================

scenarioUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("An\u00e1lisis de Escenarios y Stress Testing"),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(3, 9)),

      # --- Panel de controles ---
      card(
        card_header("Par\u00e1metros de Estr\u00e9s"),
        sliderInput(
          ns("freq_stress"), "Estr\u00e9s de Frecuencia",
          min = 0.5, max = 2.0, value = 1.0, step = 0.05
        ),
        sliderInput(
          ns("sev_stress"), "Estr\u00e9s de Severidad",
          min = 0.5, max = 2.0, value = 1.0, step = 0.05
        ),
        hr(),
        radioButtons(
          ns("n_sim"), "N\u00famero de Simulaciones",
          choices = c("1,000" = 1000, "10,000" = 10000, "50,000" = 50000),
          selected = 10000
        ),
        actionButton(
          ns("run_sim"), "Ejecutar Simulaci\u00f3n",
          class = "btn-primary w-100 mt-2",
          icon = icon("play")
        ),
        hr(),
        htmlOutput(ns("param_summary"))
      ),

      # --- Panel de resultados ---
      tagList(
        # Value boxes
        layout_columns(
          col_widths = breakpoints(sm = 6, md = 3),
          value_box(
            title = "VaR 99.5%", value = textOutput(ns("vb_var995")),
            showcase = icon("shield-halved"), theme = "danger"
          ),
          value_box(
            title = "TVaR 99.5%", value = textOutput(ns("vb_tvar995")),
            showcase = icon("triangle-exclamation"), theme = "danger"
          ),
          value_box(
            title = "P\u00e9rdida Media", value = textOutput(ns("vb_mean")),
            showcase = icon("chart-line"), theme = "primary"
          ),
          value_box(
            title = "Desviaci\u00f3n Est\u00e1ndar", value = textOutput(ns("vb_sd")),
            showcase = icon("arrows-left-right"), theme = "warning"
          )
        ),

        # Graficos
        navset_card_tab(
          title = "Resultados de Simulaci\u00f3n",
          nav_panel(
            "Densidad de P\u00e9rdida Agregada",
            plotlyOutput(ns("plot_density"), height = "450px")
          ),
          nav_panel(
            "Curva de Excedencia",
            plotlyOutput(ns("plot_exceedance"), height = "450px")
          ),
          nav_panel(
            "Tabla de Impacto",
            DTOutput(ns("table_impact"))
          )
        )
      )
    )
  )
}

scenarioServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    # --- Parametros calibrados desde los datos ---
    fitted_params <- reactive({
      tryCatch({
        d <- filtered_data()
        req(nrow(d$siniestros) > 2)

        paid <- d$siniestros %>% filter(estado_siniestro == "Pagado")
        montos <- paid$monto_siniestro
        montos <- montos[!is.na(montos) & montos > 0]
        req(length(montos) > 2)

        # Metodo de momentos para Gamma
        mu <- mean(montos)
        v <- var(montos)
        shape <- mu^2 / v
        scale <- v / mu

        # Lambda: numero esperado anual de siniestros
        n_polizas <- nrow(d$polizas)
        n_claims <- length(montos)
        n_years <- max(1, length(unique(year(d$siniestros$fecha_siniestro))))
        lambda <- n_claims / n_years  # expected annual claim count

        list(
          shape  = shape,
          scale  = scale,
          lambda = lambda,
          mu     = mu,
          v      = v,
          n_claims = n_claims,
          n_polizas = n_polizas
        )
      }, error = function(e) {
        NULL
      })
    })

    # --- Resumen de parametros ---
    output$param_summary <- renderUI({
      params <- fitted_params()
      if (is.null(params)) {
        return(tags$p(class = "text-muted", "Sin datos suficientes para calibrar."))
      }
      tags$div(
        class = "small text-muted",
        tags$strong("Parametros calibrados:"),
        tags$br(),
        sprintf("Lambda = %s", format_num(params$lambda)),
        tags$br(),
        sprintf("Gamma shape = %.2f", params$shape),
        tags$br(),
        sprintf("Gamma scale = %s", format_currency_mxn(params$scale)),
        tags$br(),
        sprintf("Severidad media = %s", format_currency_mxn(params$mu)),
        tags$br(),
        sprintf("Polizas: %s | Siniestros: %s",
                format_num(params$n_polizas),
                format_num(params$n_claims))
      )
    })

    # --- Simulacion (auto-run on load + on button click) ---
    sim_results <- eventReactive(list(input$run_sim, TRUE), ignoreNULL = FALSE, {
      tryCatch({
        params <- fitted_params()
        req(!is.null(params))

        n_sim <- as.integer(input$n_sim)
        freq_stress <- input$freq_stress
        sev_stress <- input$sev_stress

        lambda_base <- params$lambda
        shape <- params$shape
        scale_base <- params$scale

        # --- Simulacion baseline (estres = 1x) ---
        set.seed(42)
        baseline_losses <- vapply(seq_len(n_sim), function(i) {
          n_claims <- rpois(1, lambda_base)
          if (n_claims == 0) return(0)
          sum(rgamma(n_claims, shape = shape, scale = scale_base))
        }, numeric(1))

        # --- Simulacion estresada ---
        lambda_stressed <- lambda_base * freq_stress
        scale_stressed <- scale_base * sev_stress

        set.seed(42)
        stressed_losses <- vapply(seq_len(n_sim), function(i) {
          n_claims <- rpois(1, lambda_stressed)
          if (n_claims == 0) return(0)
          sum(rgamma(n_claims, shape = shape, scale = scale_stressed))
        }, numeric(1))

        # --- Metricas ---
        calc_metrics <- function(losses) {
          losses <- losses[is.finite(losses)]
          sorted <- sort(losses)
          n <- length(sorted)
          req(n > 0)
          var95  <- sorted[ceiling(0.95 * n)]
          var99  <- sorted[ceiling(0.99 * n)]
          var995 <- sorted[ceiling(0.995 * n)]
          tvar95  <- mean(sorted[sorted >= var95])
          tvar99  <- mean(sorted[sorted >= var99])
          tvar995 <- mean(sorted[sorted >= var995])
          list(
            mean_loss = mean(losses),
            sd_loss   = sd(losses),
            var_95    = var95,
            var_99    = var99,
            var_995   = var995,
            tvar_95   = tvar95,
            tvar_99   = tvar99,
            tvar_995  = tvar995
          )
        }

        baseline_metrics <- calc_metrics(baseline_losses)
        stressed_metrics <- calc_metrics(stressed_losses)

        list(
          baseline_losses  = baseline_losses,
          stressed_losses  = stressed_losses,
          baseline_metrics = baseline_metrics,
          stressed_metrics = stressed_metrics,
          freq_stress      = freq_stress,
          sev_stress       = sev_stress,
          n_sim            = n_sim
        )
      }, error = function(e) {
        showNotification(
          paste("Error en simulaci\u00f3n:", e$message),
          type = "error"
        )
        NULL
      })
    })

    # --- Value boxes ---
    output$vb_var995 <- renderText({
      res <- sim_results()
      if (is.null(res)) return("--")
      format_currency_mxn(res$stressed_metrics$var_995)
    })

    output$vb_tvar995 <- renderText({
      res <- sim_results()
      if (is.null(res)) return("--")
      format_currency_mxn(res$stressed_metrics$tvar_995)
    })

    output$vb_mean <- renderText({
      res <- sim_results()
      if (is.null(res)) return("--")
      format_currency_mxn(res$stressed_metrics$mean_loss)
    })

    output$vb_sd <- renderText({
      res <- sim_results()
      if (is.null(res)) return("--")
      format_currency_mxn(res$stressed_metrics$sd_loss)
    })

    # --- Plot: Densidad de perdida agregada ---
    output$plot_density <- renderPlotly({
      res <- sim_results()
      req(!is.null(res))

      bm <- res$baseline_metrics
      sm <- res$stressed_metrics

      # Baseline density trace
      p <- plot_ly() %>%
        add_histogram(
          x = res$baseline_losses,
          histnorm = "probability density",
          name = "Baseline",
          marker = list(color = PALETTE$primary, opacity = 0.5),
          nbinsx = 80
        ) %>%
        add_histogram(
          x = res$stressed_losses,
          histnorm = "probability density",
          name = "Estresado",
          marker = list(color = PALETTE$danger, opacity = 0.5),
          nbinsx = 80
        )

      # Lineas verticales VaR/TVaR sobre estresado
      y_max <- 0  # plotly auto-scales, lines use paper coords
      shapes <- list(
        list(type = "line", x0 = sm$var_95, x1 = sm$var_95,
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = PALETTE$accent, width = 1.5, dash = "dot")),
        list(type = "line", x0 = sm$var_99, x1 = sm$var_99,
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = PALETTE$secondary, width = 1.5, dash = "dot")),
        list(type = "line", x0 = sm$var_995, x1 = sm$var_995,
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = PALETTE$danger, width = 2, dash = "dash"))
      )

      annotations <- list(
        list(x = sm$var_95, y = 1, yref = "paper", text = "VaR 95%",
             showarrow = FALSE, font = list(size = 10, color = PALETTE$accent),
             yanchor = "bottom"),
        list(x = sm$var_99, y = 0.95, yref = "paper", text = "VaR 99%",
             showarrow = FALSE, font = list(size = 10, color = PALETTE$secondary),
             yanchor = "bottom"),
        list(x = sm$var_995, y = 0.90, yref = "paper", text = "VaR 99.5%",
             showarrow = FALSE, font = list(size = 10, color = PALETTE$danger),
             yanchor = "bottom")
      )

      p %>%
        plotly_default_layout(
          title = "Distribuci\u00f3n de P\u00e9rdida Agregada",
          xlab = "P\u00e9rdida Agregada (MXN)",
          ylab = "Densidad"
        ) %>%
        layout(
          barmode = "overlay",
          shapes = shapes,
          annotations = annotations,
          legend = list(x = 0.8, y = 0.95)
        )
    })

    # --- Plot: Curva de excedencia ---
    output$plot_exceedance <- renderPlotly({
      res <- sim_results()
      req(!is.null(res))

      # Baseline exceedance
      bl_sorted <- sort(res$baseline_losses)
      bl_n <- length(bl_sorted)
      bl_exceed <- 1 - (seq_len(bl_n) / bl_n)

      # Stressed exceedance
      st_sorted <- sort(res$stressed_losses)
      st_n <- length(st_sorted)
      st_exceed <- 1 - (seq_len(st_n) / st_n)

      plot_ly() %>%
        add_trace(
          x = bl_sorted, y = bl_exceed,
          type = "scatter", mode = "lines",
          name = "Baseline",
          line = list(color = PALETTE$primary, width = 2)
        ) %>%
        add_trace(
          x = st_sorted, y = st_exceed,
          type = "scatter", mode = "lines",
          name = "Estresado",
          line = list(color = PALETTE$danger, width = 2)
        ) %>%
        plotly_default_layout(
          title = "Curva de Probabilidad de Excedencia (1 - CDF)",
          xlab = "P\u00e9rdida Agregada (MXN)",
          ylab = "P(P\u00e9rdida > x)"
        ) %>%
        layout(
          yaxis = list(type = "log", tickformat = ".2%",
                       title = "P(P\u00e9rdida > x)",
                       gridcolor = "#E9ECEF", zerolinecolor = "#E9ECEF"),
          legend = list(x = 0.7, y = 0.95)
        )
    })

    # --- Tabla de impacto ---
    output$table_impact <- renderDT({
      res <- sim_results()
      req(!is.null(res))

      bm <- res$baseline_metrics
      sm <- res$stressed_metrics

      pct_change <- function(base, stressed) {
        if (is.na(base) || base == 0) return(NA_real_)
        (stressed - base) / abs(base)
      }

      impact_df <- tibble(
        Metrica = c(
          "P\u00e9rdida Media",
          "Desviaci\u00f3n Est\u00e1ndar",
          "VaR 95%",
          "VaR 99%",
          "VaR 99.5%",
          "TVaR 95%",
          "TVaR 99%",
          "TVaR 99.5%"
        ),
        Baseline = c(
          bm$mean_loss, bm$sd_loss,
          bm$var_95, bm$var_99, bm$var_995,
          bm$tvar_95, bm$tvar_99, bm$tvar_995
        ),
        Estresado = c(
          sm$mean_loss, sm$sd_loss,
          sm$var_95, sm$var_99, sm$var_995,
          sm$tvar_95, sm$tvar_99, sm$tvar_995
        )
      ) %>%
        mutate(
          `Cambio %` = pct_change(Baseline, Estresado)
        )

      datatable(
        impact_df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "t",
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = 1:3)
          )
        )
      ) %>%
        formatCurrency(c("Baseline", "Estresado"), currency = "$", digits = 0) %>%
        formatPercentage("Cambio %", digits = 1)
    })
  })
}
