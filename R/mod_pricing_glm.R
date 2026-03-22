# ==============================================================================
# MODULO: Motor de Pricing GLM
# GLM de frecuencia (Poisson) y severidad (Gamma) con cotizador interactivo
# ==============================================================================

# --- UI -----------------------------------------------------------------------
pricingGlmUI <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("pricing_tabs"),
    title = "Motor de Pricing GLM",
    full_screen = TRUE,

    # ------------------------------------------------------------------
    # Tab 1: Resultados del Modelo
    # ------------------------------------------------------------------
    nav_panel(
      title = "Resultados del Modelo",
      icon = icon("chart-line"),
      layout_columns(
        col_widths = c(12),
        uiOutput(ns("model_status_banner"))
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 4),
        value_box(
          title = "Observaciones (Frecuencia)",
          value = textOutput(ns("n_obs_freq")),
          showcase = icon("database"),
          theme = "primary"
        ),
        value_box(
          title = "Observaciones (Severidad)",
          value = textOutput(ns("n_obs_sev")),
          showcase = icon("database"),
          theme = "secondary"
        ),
        value_box(
          title = "Prima Pura Promedio",
          value = textOutput(ns("avg_pure_premium")),
          showcase = icon("dollar-sign"),
          theme = "success"
        )
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        card(
          card_header("Coeficientes - Modelo de Frecuencia (Poisson)"),
          DTOutput(ns("tbl_freq_coefs"))
        ),
        card(
          card_header("Coeficientes - Modelo de Severidad (Gamma)"),
          DTOutput(ns("tbl_sev_coefs"))
        )
      ),
      card(
        card_header("Comparacion de Modelos"),
        DTOutput(ns("tbl_model_comparison"))
      )
    ),

    # ------------------------------------------------------------------
    # Tab 2: Tabla de Relatividades
    # ------------------------------------------------------------------
    nav_panel(
      title = "Tabla de Relatividades",
      icon = icon("table"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        card(
          card_header("Relatividades - Frecuencia"),
          DTOutput(ns("tbl_rel_freq"))
        ),
        card(
          card_header("Relatividades - Severidad"),
          DTOutput(ns("tbl_rel_sev"))
        )
      )
    ),

    # ------------------------------------------------------------------
    # Tab 3: Cotizador Interactivo
    # ------------------------------------------------------------------
    nav_panel(
      title = "Cotizador Interactivo",
      icon = icon("calculator"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = c(4, 8)),

        # Panel de inputs
        card(
          card_header("Perfil del Asegurado"),
          sliderInput(
            ns("cot_edad"), "Edad del Conductor",
            min = 18, max = 75, value = 35, step = 1
          ),
          radioButtons(
            ns("cot_genero"), "Genero",
            choices = c("M", "F"), selected = "M", inline = TRUE
          ),
          selectInput(
            ns("cot_tipo_vehiculo"), "Tipo de Vehiculo",
            choices = NULL
          ),
          selectInput(
            ns("cot_zona_riesgo"), "Zona de Riesgo",
            choices = c("Zona Alta", "Zona Media", "Zona Baja"),
            selected = "Zona Media"
          ),
          selectInput(
            ns("cot_canal_venta"), "Canal de Venta",
            choices = NULL
          ),
          selectInput(
            ns("cot_segmento_score"), "Segmento Score Crediticio",
            choices = c("Bajo (<550)", "Medio (550-649)", "Alto (650+)"),
            selected = "Medio (550-649)"
          ),
          hr(),
          actionButton(
            ns("btn_cotizar"), "Cotizar",
            class = "btn-primary w-100", icon = icon("calculator")
          )
        ),

        # Panel de resultados
        tagList(
          layout_columns(
            col_widths = breakpoints(sm = 6, md = 3),
            value_box(
              title = "Frecuencia Esperada",
              value = textOutput(ns("cot_freq")),
              showcase = icon("percent"),
              theme = "primary"
            ),
            value_box(
              title = "Severidad Esperada",
              value = textOutput(ns("cot_sev")),
              showcase = icon("money-bill"),
              theme = "secondary"
            ),
            value_box(
              title = "Prima Pura",
              value = textOutput(ns("cot_pure")),
              showcase = icon("calculator"),
              theme = "success"
            ),
            value_box(
              title = "Prima Comercial (40%)",
              value = textOutput(ns("cot_commercial")),
              showcase = icon("tags"),
              theme = "warning"
            )
          ),
          card(
            card_header("Descomposicion de la Prima - Waterfall"),
            plotly::plotlyOutput(ns("plot_waterfall"), height = "480px")
          ),
          card(
            card_header("Comparacion con Portafolio"),
            DTOutput(ns("tbl_cot_comparison"))
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # Tab 4: Diagnosticos
    # ------------------------------------------------------------------
    nav_panel(
      title = "Diagnosticos",
      icon = icon("stethoscope"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        card(
          card_header("Residuos Deviance vs Ajustados - Frecuencia"),
          plotly::plotlyOutput(ns("plot_resid_freq"), height = "380px")
        ),
        card(
          card_header("Residuos Deviance vs Ajustados - Severidad"),
          plotly::plotlyOutput(ns("plot_resid_sev"), height = "380px")
        )
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        card(
          card_header("QQ Plot - Frecuencia"),
          plotly::plotlyOutput(ns("plot_qq_freq"), height = "380px")
        ),
        card(
          card_header("QQ Plot - Severidad"),
          plotly::plotlyOutput(ns("plot_qq_sev"), height = "380px")
        )
      ),
      card(
        card_header("Observado vs Predicho por Factor"),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(4, 8)),
          selectInput(
            ns("diag_factor"), "Factor de Analisis",
            choices = c(
              "rango_edad", "genero", "tipo_vehiculo",
              "zona_riesgo", "canal_venta", "segmento_score"
            ),
            selected = "rango_edad"
          ),
          plotly::plotlyOutput(ns("plot_obs_vs_pred"), height = "380px")
        )
      )
    )
  )
}

# --- SERVER -------------------------------------------------------------------
pricingGlmServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ====================================================================
    # DATA PREP: build modeling datasets
    # ====================================================================
    modeling_data <- reactive({
      req(filtered_data())
      d <- filtered_data()
      polizas <- d$polizas
      siniestros <- d$siniestros

      # -- Frequency dataset: one row per policy with claim count ----------
      claim_counts <- siniestros %>%
        count(poliza_id, name = "n_claims")

      freq_df <- polizas %>%
        left_join(claim_counts, by = "poliza_id") %>%
        mutate(
          n_claims   = replace_na(n_claims, 0L),
          exposicion = pmax(exposicion, 0.01)
        ) %>%
        filter(
          !is.na(rango_edad),
          !is.na(genero),
          !is.na(tipo_vehiculo),
          !is.na(zona_riesgo),
          !is.na(canal_venta),
          !is.na(segmento_score),
          segmento_score != "Sin Score"
        ) %>%
        mutate(across(
          c(rango_edad, genero, tipo_vehiculo, zona_riesgo,
            canal_venta, segmento_score),
          as.factor
        ))

      # -- Severity dataset: one row per claim with positive amount --------
      sev_df <- siniestros %>%
        filter(monto_siniestro > 0) %>%
        left_join(
          polizas %>%
            select(poliza_id, tipo_vehiculo, rango_edad, zona_riesgo),
          by = "poliza_id"
        ) %>%
        filter(
          !is.na(tipo_siniestro),
          !is.na(tipo_vehiculo),
          !is.na(rango_edad),
          !is.na(zona_riesgo)
        ) %>%
        mutate(across(
          c(tipo_siniestro, tipo_vehiculo, rango_edad, zona_riesgo),
          as.factor
        ))

      list(freq = freq_df, sev = sev_df)
    })

    # ====================================================================
    # FIT MODELS
    # ====================================================================
    freq_model <- reactive({
      req(modeling_data())
      df <- modeling_data()$freq

      validate(
        need(nrow(df) >= 50,
             "Se necesitan al menos 50 polizas para ajustar el modelo de frecuencia.")
      )

      tryCatch(
        suppressWarnings(
          glm(
            n_claims ~ rango_edad + genero + tipo_vehiculo +
              zona_riesgo + canal_venta + segmento_score,
            family  = poisson(link = "log"),
            offset  = log(exposicion),
            data    = df
          )
        ),
        error = function(e) {
          message("GLM frecuencia error: ", e$message)
          NULL
        }
      )
    })

    sev_model <- reactive({
      req(modeling_data())
      df <- modeling_data()$sev

      validate(
        need(nrow(df) >= 30,
             "Se necesitan al menos 30 siniestros para ajustar el modelo de severidad.")
      )

      tryCatch(
        suppressWarnings(
          glm(
            monto_siniestro ~ tipo_siniestro + tipo_vehiculo +
              rango_edad + zona_riesgo,
            family = Gamma(link = "log"),
            data   = df
          )
        ),
        error = function(e) {
          message("GLM severidad error: ", e$message)
          NULL
        }
      )
    })

    # ====================================================================
    # HELPERS: tidy tables, relativities
    # ====================================================================
    tidy_freq <- reactive({
      req(freq_model())
      # Use conf.int=FALSE to avoid slow profiling on large datasets
      # Compute CI manually from std.error instead
      t <- broom::tidy(freq_model(), exponentiate = TRUE, conf.int = FALSE)
      t$conf.low  <- exp(log(t$estimate) - 1.96 * t$std.error)
      t$conf.high <- exp(log(t$estimate) + 1.96 * t$std.error)
      t
    })

    tidy_sev <- reactive({
      req(sev_model())
      t <- broom::tidy(sev_model(), exponentiate = TRUE, conf.int = FALSE)
      t$conf.low  <- exp(log(t$estimate) - 1.96 * t$std.error)
      t$conf.high <- exp(log(t$estimate) + 1.96 * t$std.error)
      t
    })

    # Build relativities table from a tidy model output
    build_relativities <- function(tidy_df, model_label) {
      tidy_df %>%
        mutate(
          Factor = gsub("^(rango_edad|genero|tipo_vehiculo|zona_riesgo|canal_venta|segmento_score|tipo_siniestro)", "\\1: ", term),
          Factor = case_when(
            term == "(Intercept)" ~ "Intercepto",
            grepl("^rango_edad", term)      ~ "Rango Edad",
            grepl("^genero", term)           ~ "Genero",
            grepl("^tipo_vehiculo", term)    ~ "Tipo Vehiculo",
            grepl("^zona_riesgo", term)      ~ "Zona Riesgo",
            grepl("^canal_venta", term)      ~ "Canal Venta",
            grepl("^segmento_score", term)   ~ "Segmento Score",
            grepl("^tipo_siniestro", term)   ~ "Tipo Siniestro",
            TRUE ~ term
          ),
          Nivel = gsub(
            "^(rango_edad|genero|tipo_vehiculo|zona_riesgo|canal_venta|segmento_score|tipo_siniestro)",
            "", term
          ),
          Nivel = ifelse(Nivel == "", "(Intercepto)", Nivel),
          Relatividad = round(estimate, 4),
          `IC 95%` = paste0(
            "[", round(conf.low, 4), " , ", round(conf.high, 4), "]"
          ),
          Base = ifelse(term == "(Intercept)", "Si", "No")
        ) %>%
        select(Factor, Nivel, Relatividad, `IC 95%`, Base)
    }

    # ====================================================================
    # Tab 1 -- Resultados del Modelo
    # ====================================================================
    output$model_status_banner <- renderUI({
      fm <- freq_model()
      sm <- sev_model()
      if (is.null(fm) || is.null(sm)) {
        tags$div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          " No se pudieron ajustar uno o ambos modelos. Revise que los datos filtrados tengan suficiente variabilidad."
        )
      } else {
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          " Modelos ajustados exitosamente."
        )
      }
    })

    output$n_obs_freq <- renderText({
      req(modeling_data())
      format(nrow(modeling_data()$freq), big.mark = ",")
    })

    output$n_obs_sev <- renderText({
      req(modeling_data())
      format(nrow(modeling_data()$sev), big.mark = ",")
    })

    output$avg_pure_premium <- renderText({
      req(freq_model(), sev_model())
      df_freq <- modeling_data()$freq
      # Predicted annual claim rate per policy (response / exposure)
      pred_freq_rate <- predict(freq_model(), type = "response") / df_freq$exposicion
      pred_sev  <- mean(predict(sev_model(), type = "response"))
      # Pure premium = aggregate frequency rate * average severity
      avg_freq_rate <- sum(predict(freq_model(), type = "response")) / sum(df_freq$exposicion)
      avg_pp <- avg_freq_rate * pred_sev
      format_currency_mxn(avg_pp)
    })

    output$tbl_freq_coefs <- renderDT({
      req(tidy_freq())
      df <- tidy_freq() %>%
        mutate(
          estimate  = round(estimate, 4),
          std.error = round(std.error, 4),
          statistic = round(statistic, 2),
          p.value   = signif(p.value, 3),
          conf.low  = round(conf.low, 4),
          conf.high = round(conf.high, 4)
        ) %>%
        rename(
          Termino       = term,
          `Exp(Coef)`   = estimate,
          `Error Std`   = std.error,
          `Estadistico` = statistic,
          `Valor p`     = p.value,
          `IC Inf`      = conf.low,
          `IC Sup`      = conf.high
        )
      datatable(
        df,
        rownames = FALSE,
        options  = list(
          pageLength = 15, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      ) %>%
        formatStyle(
          "Valor p",
          backgroundColor = styleInterval(c(0.01, 0.05, 0.10),
                                          c("#d4edda", "#d1ecf1", "#fff3cd", "#f8d7da"))
        )
    })

    output$tbl_sev_coefs <- renderDT({
      req(tidy_sev())
      df <- tidy_sev() %>%
        mutate(
          estimate  = round(estimate, 4),
          std.error = round(std.error, 4),
          statistic = round(statistic, 2),
          p.value   = signif(p.value, 3),
          conf.low  = round(conf.low, 4),
          conf.high = round(conf.high, 4)
        ) %>%
        rename(
          Termino       = term,
          `Exp(Coef)`   = estimate,
          `Error Std`   = std.error,
          `Estadistico` = statistic,
          `Valor p`     = p.value,
          `IC Inf`      = conf.low,
          `IC Sup`      = conf.high
        )
      datatable(
        df,
        rownames = FALSE,
        options  = list(
          pageLength = 15, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      ) %>%
        formatStyle(
          "Valor p",
          backgroundColor = styleInterval(c(0.01, 0.05, 0.10),
                                          c("#d4edda", "#d1ecf1", "#fff3cd", "#f8d7da"))
        )
    })

    output$tbl_model_comparison <- renderDT({
      req(freq_model(), sev_model())
      fm <- freq_model()
      sm <- sev_model()
      comp <- tibble(
        Modelo        = c("Frecuencia (Poisson)", "Severidad (Gamma)"),
        AIC           = round(c(AIC(fm), AIC(sm)), 2),
        BIC           = round(c(BIC(fm), BIC(sm)), 2),
        Devianza      = round(c(deviance(fm), deviance(sm)), 2),
        `Devianza Nula` = round(c(fm$null.deviance, sm$null.deviance), 2),
        `GL Residuales` = c(fm$df.residual, sm$df.residual),
        `Pseudo R2`   = round(
          c(1 - deviance(fm) / fm$null.deviance,
            1 - deviance(sm) / sm$null.deviance), 4
        )
      )
      datatable(
        comp, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE,
                       columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    })

    # ====================================================================
    # Tab 2 -- Tabla de Relatividades
    # ====================================================================
    output$tbl_rel_freq <- renderDT({
      req(tidy_freq())
      df <- build_relativities(tidy_freq(), "Frecuencia")
      datatable(
        df, rownames = FALSE,
        options = list(
          pageLength = 20, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      ) %>%
        formatStyle(
          "Base",
          backgroundColor = styleEqual("Si", "#d4edda")
        )
    })

    output$tbl_rel_sev <- renderDT({
      req(tidy_sev())
      df <- build_relativities(tidy_sev(), "Severidad")
      datatable(
        df, rownames = FALSE,
        options = list(
          pageLength = 20, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      ) %>%
        formatStyle(
          "Base",
          backgroundColor = styleEqual("Si", "#d4edda")
        )
    })

    # ====================================================================
    # Tab 3 -- Cotizador Interactivo
    # ====================================================================

    # Populate choices from data
    observe({
      req(filtered_data())
      d <- filtered_data()$polizas
      updateSelectInput(session, "cot_tipo_vehiculo",
                        choices = sort(unique(d$tipo_vehiculo)))
      updateSelectInput(session, "cot_canal_venta",
                        choices = sort(unique(d$canal_venta)))
    })

    # Map age to rango_edad factor level (same breaks as enrich_polizas)
    age_to_rango <- function(edad) {
      cut(
        edad,
        breaks = c(0, 25, 35, 45, 55, 100),
        labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
        right = TRUE
      )
    }

    # Reactive prediction triggered by button
    prediction <- eventReactive(input$btn_cotizar, {
      req(freq_model(), sev_model())

      rango <- as.character(age_to_rango(input$cot_edad))

      # Build new-data rows matching model factor levels
      newdata_freq <- tibble(
        rango_edad      = factor(rango, levels = levels(modeling_data()$freq$rango_edad)),
        genero          = factor(input$cot_genero, levels = levels(modeling_data()$freq$genero)),
        tipo_vehiculo   = factor(input$cot_tipo_vehiculo, levels = levels(modeling_data()$freq$tipo_vehiculo)),
        zona_riesgo     = factor(input$cot_zona_riesgo, levels = levels(modeling_data()$freq$zona_riesgo)),
        canal_venta     = factor(input$cot_canal_venta, levels = levels(modeling_data()$freq$canal_venta)),
        segmento_score  = factor(input$cot_segmento_score, levels = levels(modeling_data()$freq$segmento_score)),
        exposicion      = 1
      )

      newdata_sev <- tibble(
        tipo_siniestro = factor(
          levels(modeling_data()$sev$tipo_siniestro)[1],
          levels = levels(modeling_data()$sev$tipo_siniestro)
        ),
        tipo_vehiculo = factor(input$cot_tipo_vehiculo, levels = levels(modeling_data()$sev$tipo_vehiculo)),
        rango_edad    = factor(rango, levels = levels(modeling_data()$sev$rango_edad)),
        zona_riesgo   = factor(input$cot_zona_riesgo, levels = levels(modeling_data()$sev$zona_riesgo))
      )

      # Predictions
      pred_freq <- tryCatch(
        predict(freq_model(), newdata = newdata_freq, type = "response"),
        error = function(e) NA_real_
      )
      pred_sev <- tryCatch(
        predict(sev_model(), newdata = newdata_sev, type = "response"),
        error = function(e) NA_real_
      )

      pure_premium       <- pred_freq * pred_sev
      commercial_premium <- pure_premium * 1.40

      # --- Waterfall decomposition (frequency side) -----------------------
      # Extract base rate and multiplicative factors from the frequency model
      fm <- freq_model()
      coefs_freq <- coef(fm)
      intercept_freq <- coefs_freq["(Intercept)"]

      # For each factor, find the matching coefficient
      factor_map <- list(
        `Rango Edad`      = paste0("rango_edad", rango),
        `Genero`          = paste0("genero", input$cot_genero),
        `Tipo Vehiculo`   = paste0("tipo_vehiculo", input$cot_tipo_vehiculo),
        `Zona Riesgo`     = paste0("zona_riesgo", input$cot_zona_riesgo),
        `Canal Venta`     = paste0("canal_venta", input$cot_canal_venta),
        `Segmento Score`  = paste0("segmento_score", input$cot_segmento_score)
      )

      # Severity side coefficients
      sm <- sev_model()
      coefs_sev <- coef(sm)
      intercept_sev <- coefs_sev["(Intercept)"]

      sev_factor_map <- list(
        `Tipo Vehiculo (Sev)` = paste0("tipo_vehiculo", input$cot_tipo_vehiculo),
        `Rango Edad (Sev)`    = paste0("rango_edad", rango),
        `Zona Riesgo (Sev)`   = paste0("zona_riesgo", input$cot_zona_riesgo)
      )

      # Base rate = exp(intercept_freq) * exp(intercept_sev)
      base_rate <- exp(intercept_freq) * exp(intercept_sev)

      # Build waterfall steps
      steps <- list()
      running <- base_rate

      # Frequency factors
      for (nm in names(factor_map)) {
        coef_name <- factor_map[[nm]]
        if (coef_name %in% names(coefs_freq)) {
          multiplier <- exp(coefs_freq[coef_name])
        } else {
          multiplier <- 1.0  # base level
        }
        increment <- running * (multiplier - 1)
        steps[[nm]] <- list(multiplier = multiplier, increment = increment)
        running <- running * multiplier
      }

      # Severity factors
      for (nm in names(sev_factor_map)) {
        coef_name <- sev_factor_map[[nm]]
        if (coef_name %in% names(coefs_sev)) {
          multiplier <- exp(coefs_sev[coef_name])
        } else {
          multiplier <- 1.0
        }
        increment <- running * (multiplier - 1)
        steps[[nm]] <- list(multiplier = multiplier, increment = increment)
        running <- running * multiplier
      }

      # Loading
      loading_increment <- running * 0.40
      steps[["Carga Comercial (40%)"]] <- list(multiplier = 1.40, increment = loading_increment)

      # Portfolio averages for comparison
      port_freq <- mean(modeling_data()$freq$n_claims / modeling_data()$freq$exposicion)
      port_sev  <- mean(modeling_data()$sev$monto_siniestro)
      port_pp   <- port_freq * port_sev

      list(
        freq             = pred_freq,
        sev              = pred_sev,
        pure_premium     = pure_premium,
        commercial       = commercial_premium,
        base_rate        = base_rate,
        steps            = steps,
        final_commercial = running * 1.40,
        port_freq        = port_freq,
        port_sev         = port_sev,
        port_pp          = port_pp
      )
    })

    output$cot_freq <- renderText({
      req(prediction())
      format_pct(prediction()$freq)
    })

    output$cot_sev <- renderText({
      req(prediction())
      format_currency_mxn(prediction()$sev)
    })

    output$cot_pure <- renderText({
      req(prediction())
      format_currency_mxn(prediction()$pure_premium)
    })

    output$cot_commercial <- renderText({
      req(prediction())
      format_currency_mxn(prediction()$commercial)
    })

    # --- Waterfall chart ---------------------------------------------------
    output$plot_waterfall <- plotly::renderPlotly({
      req(prediction())
      pred <- prediction()

      # Build the waterfall dataframe
      labels     <- c("Tasa Base")
      values     <- c(pred$base_rate)
      measures   <- c("absolute")

      for (nm in names(pred$steps)) {
        labels   <- c(labels, nm)
        values   <- c(values, pred$steps[[nm]]$increment)
        measures <- c(measures, "relative")
      }

      labels   <- c(labels, "Prima Comercial")
      values   <- c(values, pred$final_commercial)
      measures <- c(measures, "total")

      # Color mapping
      colors <- sapply(seq_along(measures), function(i) {
        if (measures[i] == "absolute") return(PALETTE$primary)
        if (measures[i] == "total") return(PALETTE$success)
        if (values[i] >= 0) return(PALETTE$danger)
        return(PALETTE$accent)
      })

      # Plotly waterfall
      p <- plotly::plot_ly(
        type = "waterfall",
        x = ~labels,
        y = ~values,
        measure = ~measures,
        text = sapply(values, function(v) format_currency_mxn(v)),
        textposition = "outside",
        connector = list(line = list(color = PALETTE$muted, width = 1, dash = "dot")),
        increasing = list(marker = list(color = PALETTE$danger)),
        decreasing = list(marker = list(color = PALETTE$accent)),
        totals     = list(marker = list(color = PALETTE$success))
      )

      # Override base bar color
      p <- p %>%
        plotly::layout(
          waterfallgap = 0.3
        )

      p %>% plotly_default_layout(
        title = "Descomposicion Multiplicativa de la Prima",
        xlab  = "",
        ylab  = "Prima (MXN)"
      ) %>%
        plotly::layout(
          xaxis = list(
            tickangle = -30,
            tickfont  = list(size = 11)
          ),
          showlegend = FALSE
        )
    })

    # --- Comparison table ---------------------------------------------------
    output$tbl_cot_comparison <- renderDT({
      req(prediction())
      pred <- prediction()
      comp <- tibble(
        Metrica = c("Frecuencia", "Severidad", "Prima Pura"),
        `Perfil Cotizado` = c(
          format_pct(pred$freq),
          format_currency_mxn(pred$sev),
          format_currency_mxn(pred$pure_premium)
        ),
        `Promedio Portafolio` = c(
          format_pct(pred$port_freq),
          format_currency_mxn(pred$port_sev),
          format_currency_mxn(pred$port_pp)
        ),
        `Diferencia (%)` = c(
          format_pct((pred$freq - pred$port_freq) / pred$port_freq),
          format_pct((pred$sev - pred$port_sev) / pred$port_sev),
          format_pct((pred$pure_premium - pred$port_pp) / pred$port_pp)
        )
      )
      datatable(
        comp, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE,
                       columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    })

    # ====================================================================
    # Tab 4 -- Diagnosticos
    # ====================================================================

    # Deviance residuals vs fitted (Frequency)
    output$plot_resid_freq <- plotly::renderPlotly({
      req(freq_model())
      fm <- freq_model()
      df <- tibble(
        fitted    = fitted(fm),
        residuals = residuals(fm, type = "deviance")
      )

      p <- plotly::plot_ly(
        df, x = ~fitted, y = ~residuals,
        type = "scatter", mode = "markers",
        marker = list(
          color = PALETTE$primary, size = 4, opacity = 0.5,
          line = list(width = 0)
        ),
        hoverinfo = "text",
        text = ~paste("Ajustado:", round(fitted, 4),
                       "<br>Residuo:", round(residuals, 4))
      ) %>%
        plotly::layout(
          shapes = list(
            list(type = "line", x0 = min(df$fitted), x1 = max(df$fitted),
                 y0 = 0, y1 = 0,
                 line = list(color = PALETTE$danger, width = 2, dash = "dash"))
          )
        )

      p %>% plotly_default_layout(
        title = NULL,
        xlab  = "Valores Ajustados",
        ylab  = "Residuos Deviance"
      )
    })

    # Deviance residuals vs fitted (Severity)
    output$plot_resid_sev <- plotly::renderPlotly({
      req(sev_model())
      sm <- sev_model()
      df <- tibble(
        fitted    = fitted(sm),
        residuals = residuals(sm, type = "deviance")
      )

      p <- plotly::plot_ly(
        df, x = ~fitted, y = ~residuals,
        type = "scatter", mode = "markers",
        marker = list(
          color = PALETTE$secondary, size = 4, opacity = 0.5,
          line = list(width = 0)
        ),
        hoverinfo = "text",
        text = ~paste("Ajustado:", round(fitted, 2),
                       "<br>Residuo:", round(residuals, 4))
      ) %>%
        plotly::layout(
          shapes = list(
            list(type = "line", x0 = min(df$fitted), x1 = max(df$fitted),
                 y0 = 0, y1 = 0,
                 line = list(color = PALETTE$danger, width = 2, dash = "dash"))
          )
        )

      p %>% plotly_default_layout(
        title = NULL,
        xlab  = "Valores Ajustados",
        ylab  = "Residuos Deviance"
      )
    })

    # QQ Plot - Frequency
    output$plot_qq_freq <- plotly::renderPlotly({
      req(freq_model())
      resids <- residuals(freq_model(), type = "deviance")
      n <- length(resids)
      theoretical <- qnorm(ppoints(n))
      ordered_res <- sort(resids)

      df <- tibble(theoretical = theoretical, sample = ordered_res)

      p <- plotly::plot_ly(
        df, x = ~theoretical, y = ~sample,
        type = "scatter", mode = "markers",
        marker = list(color = PALETTE$primary, size = 4, opacity = 0.6),
        hoverinfo = "text",
        text = ~paste("Teorico:", round(theoretical, 3),
                       "<br>Muestra:", round(sample, 3))
      ) %>%
        plotly::layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(theoretical), x1 = max(theoretical),
              y0 = min(theoretical), y1 = max(theoretical),
              line = list(color = PALETTE$danger, width = 2, dash = "dash")
            )
          )
        )

      p %>% plotly_default_layout(
        title = NULL,
        xlab  = "Cuantiles Teoricos",
        ylab  = "Cuantiles Muestrales"
      )
    })

    # QQ Plot - Severity
    output$plot_qq_sev <- plotly::renderPlotly({
      req(sev_model())
      resids <- residuals(sev_model(), type = "deviance")
      n <- length(resids)
      theoretical <- qnorm(ppoints(n))
      ordered_res <- sort(resids)

      df <- tibble(theoretical = theoretical, sample = ordered_res)

      p <- plotly::plot_ly(
        df, x = ~theoretical, y = ~sample,
        type = "scatter", mode = "markers",
        marker = list(color = PALETTE$secondary, size = 4, opacity = 0.6),
        hoverinfo = "text",
        text = ~paste("Teorico:", round(theoretical, 3),
                       "<br>Muestra:", round(sample, 3))
      ) %>%
        plotly::layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(theoretical), x1 = max(theoretical),
              y0 = min(theoretical), y1 = max(theoretical),
              line = list(color = PALETTE$danger, width = 2, dash = "dash")
            )
          )
        )

      p %>% plotly_default_layout(
        title = NULL,
        xlab  = "Cuantiles Teoricos",
        ylab  = "Cuantiles Muestrales"
      )
    })

    # Observed vs Predicted by factor level
    output$plot_obs_vs_pred <- plotly::renderPlotly({
      req(freq_model(), modeling_data(), input$diag_factor)

      factor_var <- input$diag_factor
      fm <- freq_model()
      df <- modeling_data()$freq

      # Observed frequency by factor
      observed <- df %>%
        group_by(level = as.character(.data[[factor_var]])) %>%
        summarise(
          obs_freq  = sum(n_claims) / sum(exposicion),
          n_polizas = n(),
          .groups   = "drop"
        )

      # Predicted frequency by factor
      df$pred_freq <- predict(fm, type = "response") / df$exposicion
      predicted <- df %>%
        group_by(level = as.character(.data[[factor_var]])) %>%
        summarise(
          pred_freq = mean(pred_freq),
          .groups   = "drop"
        )

      comp <- observed %>%
        left_join(predicted, by = "level") %>%
        arrange(level)

      p <- plotly::plot_ly(comp, x = ~level) %>%
        plotly::add_bars(
          y = ~obs_freq, name = "Observada",
          marker = list(color = PALETTE$primary, opacity = 0.7)
        ) %>%
        plotly::add_bars(
          y = ~pred_freq, name = "Predicha",
          marker = list(color = PALETTE$accent, opacity = 0.7)
        ) %>%
        plotly::layout(
          barmode = "group",
          legend  = list(orientation = "h", x = 0.3, y = 1.12)
        )

      p %>% plotly_default_layout(
        title = paste("Observado vs Predicho -", factor_var),
        xlab  = factor_var,
        ylab  = "Frecuencia"
      )
    })

  })
}
