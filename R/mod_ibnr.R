# ==============================================================================
# MODULO: Reservas IBNR (Incurred But Not Reported)
# Implementacion manual de Chain Ladder y Bornhuetter-Ferguson
# ==============================================================================

# --- Helper: build triangle matrix from long data ---
build_triangle <- function(pagos) {
  agg <- pagos %>%
    group_by(anio_ocurrencia, anio_desarrollo) %>%
    summarise(monto = sum(monto_pago, na.rm = TRUE), .groups = "drop")

  # Incremental triangle
  incr <- agg %>%
    pivot_wider(names_from = anio_desarrollo, values_from = monto, values_fill = 0) %>%
    arrange(anio_ocurrencia)

  origin <- incr$anio_ocurrencia
  mat <- as.matrix(incr[, -1])
  rownames(mat) <- origin
  # Mask future cells as NA (lower-right of triangle)
  n_row <- nrow(mat)
  n_col <- ncol(mat)
  for (i in seq_len(n_row)) {
    available_devs <- (max(origin) - origin[i])
    if (available_devs < (n_col - 1)) {
      mat[i, (available_devs + 2):n_col] <- NA
    }
  }
  mat
}

incr_to_cum <- function(tri_incr) {
  tri_cum <- tri_incr
  for (i in seq_len(nrow(tri_cum))) {
    for (j in 2:ncol(tri_cum)) {
      if (!is.na(tri_cum[i, j]) && !is.na(tri_cum[i, j - 1])) {
        tri_cum[i, j] <- tri_cum[i, j - 1] + tri_incr[i, j]
      }
    }
  }
  tri_cum
}

cum_to_incr <- function(tri_cum) {
  tri_incr <- tri_cum
  for (i in seq_len(nrow(tri_incr))) {
    for (j in ncol(tri_incr):2) {
      if (!is.na(tri_incr[i, j]) && !is.na(tri_incr[i, j - 1])) {
        tri_incr[i, j] <- tri_cum[i, j] - tri_cum[i, j - 1]
      }
    }
  }
  tri_incr
}

get_latest_diagonal <- function(tri_cum) {
  sapply(seq_len(nrow(tri_cum)), function(i) {
    vals <- tri_cum[i, ]
    non_na <- which(!is.na(vals))
    if (length(non_na) == 0) return(NA_real_)
    vals[max(non_na)]
  })
}

# --- Chain Ladder computation ---
chain_ladder <- function(tri_cum) {
  n_dev <- ncol(tri_cum)
  n_orig <- nrow(tri_cum)

  # Link ratios (volume-weighted)
  ldf <- numeric(n_dev - 1)
  for (j in seq_len(n_dev - 1)) {
    from <- tri_cum[, j]
    to <- tri_cum[, j + 1]
    valid <- !is.na(from) & !is.na(to) & from > 0
    ldf[j] <- if (sum(valid) > 0) sum(to[valid]) / sum(from[valid]) else 1.0
  }

  # CDF to ultimate (product of LDFs from position to end)
  cdf <- numeric(n_dev)
  cdf[n_dev] <- 1.0
  for (j in (n_dev - 1):1) {
    cdf[j] <- ldf[j] * cdf[j + 1]
  }

  # Dev position for each origin year
  latest <- get_latest_diagonal(tri_cum)
  dev_pos <- sapply(seq_len(n_orig), function(i) {
    vals <- tri_cum[i, ]
    max(which(!is.na(vals)))
  })

  # Ultimate and IBNR
  ultimate <- latest * cdf[dev_pos]
  ibnr <- ultimate - latest

  # Mack standard error (simplified)
  sigma_sq <- numeric(n_dev - 1)
  for (j in seq_len(n_dev - 1)) {
    from <- tri_cum[, j]
    to <- tri_cum[, j + 1]
    valid <- !is.na(from) & !is.na(to) & from > 0
    if (sum(valid) > 1) {
      resid <- (to[valid] / from[valid] - ldf[j])^2 * from[valid]
      sigma_sq[j] <- sum(resid) / (sum(valid) - 1)
    } else {
      sigma_sq[j] <- 0
    }
  }

  # Process variance for each origin year (Mack 1993)
  se <- numeric(n_orig)
  for (i in seq_len(n_orig)) {
    pos <- dev_pos[i]
    if (pos >= n_dev) {
      se[i] <- 0
      next
    }
    var_sum <- 0
    for (j in pos:(n_dev - 1)) {
      c_ij <- if (j == pos) latest[i] else latest[i] * prod(ldf[pos:min(j - 1, n_dev - 1)])
      if (c_ij > 0 && sigma_sq[j] > 0) {
        col_sum <- sum(tri_cum[!is.na(tri_cum[, j]), j])
        var_sum <- var_sum + c_ij^2 * sigma_sq[j] / ldf[j]^2 * (1 / c_ij + 1 / col_sum)
      }
    }
    se[i] <- sqrt(max(var_sum, 0))
  }

  list(
    ldf = ldf,
    cdf = cdf,
    latest = latest,
    ultimate = ultimate,
    ibnr = ibnr,
    se = se,
    dev_pos = dev_pos,
    origin = as.integer(rownames(tri_cum))
  )
}

# --- UI -----------------------------------------------------------------------
ibnrUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("ibnr_tabs"),
    title = "Reservas IBNR",
    full_screen = TRUE,

    nav_panel(
      title = "Tri\u00e1ngulo de Desarrollo", icon = icon("table"),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span("Tri\u00e1ngulo de Pagos"),
          radioButtons(ns("tri_tipo"), NULL,
                       choices = c("Acumulado" = "cumulative", "Incremental" = "incremental"),
                       selected = "cumulative", inline = TRUE)
        ),
        DTOutput(ns("dt_triangulo"))
      )
    ),

    nav_panel(
      title = "Factores de Desarrollo", icon = icon("arrows-left-right"),
      card(card_header("Link Ratios (Factores Edad-a-Edad)"), DTOutput(ns("dt_link_ratios"))),
      card(card_header("Factores de Desarrollo Acumulados"), DTOutput(ns("dt_cdf")))
    ),

    nav_panel(
      title = "Estimaci\u00f3n IBNR", icon = icon("calculator"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 4),
        value_box(title = "IBNR Total (Chain Ladder)", value = textOutput(ns("vb_ibnr_cl")),
                  showcase = icon("link"), theme = "primary"),
        value_box(title = "IBNR Total (Bornhuetter-Ferguson)", value = textOutput(ns("vb_ibnr_bf")),
                  showcase = icon("scale-balanced"), theme = "secondary"),
        value_box(title = "Diferencia CL vs BF", value = textOutput(ns("vb_diff")),
                  showcase = icon("arrows-left-right"), theme = "warning")
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Comparativo por Año de Ocurrencia",
          sliderInput(ns("elr"), "Expected Loss Ratio (BF)",
                      min = IBNR_CONFIG$elr_min, max = IBNR_CONFIG$elr_max,
                      value = IBNR_CONFIG$default_elr, step = 0.01)
        ),
        DTOutput(ns("dt_comparativo"))
      )
    ),

    nav_panel(
      title = "Diagn\u00f3sticos", icon = icon("chart-line"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        card(card_header("IBNR y Errores Est\u00e1ndar por A\u00f1o"),
             plotlyOutput(ns("plot_se"), height = "400px")),
        card(card_header("Factores de Desarrollo por Periodo"),
             plotlyOutput(ns("plot_ldf"), height = "400px"))
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
ibnrServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    tri_data <- reactive({
      d <- filtered_data()
      validate(need(nrow(d$pagos) > 0, "No hay datos de pagos de desarrollo."))
      incr <- build_triangle(d$pagos)
      cum <- incr_to_cum(incr)
      validate(need(nrow(cum) >= 2 && ncol(cum) >= 2,
                    "El triangulo requiere al menos 2 periodos."))
      list(cumulative = cum, incremental = incr)
    })

    cl_result <- reactive({
      chain_ladder(tri_data()$cumulative)
    })

    # --- Tab 1: Triangle display ---
    output$dt_triangulo <- renderDT({
      tipo <- input$tri_tipo %||% "cumulative"
      tri <- if (tipo == "cumulative") tri_data()$cumulative else tri_data()$incremental
      df <- as.data.frame(tri)
      df <- cbind(`Año Ocurrencia` = rownames(tri), df)
      colnames(df) <- c("Año Ocurrencia", paste0("Dev ", colnames(tri)))

      vals <- as.numeric(unlist(df[, -1]))
      vals <- vals[!is.na(vals) & vals > 0]
      brks <- if (length(vals) >= 2) unique(quantile(vals, seq(0, 1, length.out = 7))) else NULL
      clrs <- if (!is.null(brks) && length(brks) > 1) {
        colorRampPalette(c("#F8F9FA", "#B3D7EA", PALETTE$primary, "#1A5276"))(length(brks) - 1)
      } else NULL

      dt <- datatable(df, rownames = FALSE,
                      options = list(dom = "t", pageLength = nrow(df), scrollX = TRUE))
      num_cols <- setdiff(names(df), "Año Ocurrencia")
      dt <- formatCurrency(dt, num_cols, "$", digits = 0)
      if (!is.null(brks) && length(brks) > 2) {
        dt <- formatStyle(dt, num_cols,
                          backgroundColor = styleInterval(brks[-c(1, length(brks))], clrs))
      }
      dt
    })

    # --- Tab 2: Link ratios ---
    output$dt_link_ratios <- renderDT({
      cl <- cl_result()
      tri <- tri_data()$cumulative
      n_dev <- ncol(tri)

      # Individual ratios
      ratios <- matrix(NA, nrow(tri), n_dev - 1)
      for (j in seq_len(n_dev - 1)) {
        for (i in seq_len(nrow(tri))) {
          if (!is.na(tri[i, j]) && !is.na(tri[i, j + 1]) && tri[i, j] > 0) {
            ratios[i, j] <- tri[i, j + 1] / tri[i, j]
          }
        }
      }
      dev_labels <- paste0(colnames(tri)[-n_dev], " -> ", colnames(tri)[-1])
      df <- as.data.frame(ratios)
      names(df) <- dev_labels
      df <- cbind(`Año` = rownames(tri), df)
      avg_row <- c("Ponderado", sprintf("%.4f", cl$ldf))
      df <- rbind(df, avg_row)

      datatable(df, rownames = FALSE,
                options = list(dom = "t", pageLength = nrow(df), scrollX = TRUE)) %>%
        formatRound(dev_labels, 4)
    })

    output$dt_cdf <- renderDT({
      cl <- cl_result()
      dev_labels <- paste0("Dev ", seq_along(cl$cdf) - 1)
      df <- data.frame(
        Periodo = dev_labels,
        CDF = sprintf("%.4f", cl$cdf),
        `% Desarrollo` = sprintf("%.1f%%", 100 / cl$cdf),
        check.names = FALSE
      )
      datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = nrow(df)))
    })

    # --- Tab 3: IBNR Estimation ---
    bf_result <- reactive({
      cl <- cl_result()
      elr <- input$elr %||% IBNR_CONFIG$default_elr
      d <- filtered_data()
      # Match premium to accident year via exposure-weighted allocation
      premium <- sapply(cl$origin, function(yr) {
        pols_yr <- d$polizas %>% filter(anio_suscripcion == yr)
        sum(pols_yr$prima_neta, na.rm = TRUE)
      })
      pct_unreported <- 1 - 1 / cl$cdf[cl$dev_pos]
      expected_ult <- premium * elr
      bf_ibnr <- expected_ult * pct_unreported
      bf_ult <- cl$latest + bf_ibnr
      list(premium = premium, bf_ultimate = bf_ult, bf_ibnr = bf_ibnr,
           pct_unreported = pct_unreported)
    })

    output$dt_comparativo <- renderDT({
      cl <- cl_result()
      bf <- bf_result()
      df <- data.frame(
        `Año` = cl$origin,
        `Pagado` = cl$latest,
        `CL Ultimate` = cl$ultimate,
        `CL IBNR` = cl$ibnr,
        `CL S.E.` = cl$se,
        `BF Ultimate` = bf$bf_ultimate,
        `BF IBNR` = bf$bf_ibnr,
        check.names = FALSE
      )
      totals <- data.frame(
        `Año` = "TOTAL",
        `Pagado` = sum(cl$latest),
        `CL Ultimate` = sum(cl$ultimate),
        `CL IBNR` = sum(cl$ibnr),
        `CL S.E.` = sqrt(sum(cl$se^2)),
        `BF Ultimate` = sum(bf$bf_ultimate),
        `BF IBNR` = sum(bf$bf_ibnr),
        check.names = FALSE
      )
      df <- rbind(df, totals)
      money_cols <- c("Pagado", "CL Ultimate", "CL IBNR", "CL S.E.", "BF Ultimate", "BF IBNR")
      datatable(df, rownames = FALSE,
                options = list(dom = "t", pageLength = nrow(df), scrollX = TRUE)) %>%
        formatCurrency(money_cols, "$", digits = 0)
    })

    output$vb_ibnr_cl <- renderText(format_currency_mxn(sum(cl_result()$ibnr)))
    output$vb_ibnr_bf <- renderText(format_currency_mxn(sum(bf_result()$bf_ibnr)))
    output$vb_diff <- renderText({
      diff_val <- sum(cl_result()$ibnr) - sum(bf_result()$bf_ibnr)
      paste0(ifelse(diff_val >= 0, "+", ""), format_currency_mxn(diff_val))
    })

    # --- Tab 4: Diagnostics ---
    output$plot_se <- renderPlotly({
      cl <- cl_result()
      df <- data.frame(anio = cl$origin, ibnr = cl$ibnr, se = cl$se)
      df$lower <- df$ibnr - 1.96 * df$se
      df$upper <- df$ibnr + 1.96 * df$se

      plot_ly(df, x = ~anio) %>%
        add_ribbons(ymin = ~lower, ymax = ~upper,
                    fillcolor = "rgba(46,134,171,0.2)",
                    line = list(color = "transparent"), name = "IC 95%") %>%
        add_trace(y = ~ibnr, type = "scatter", mode = "lines+markers",
                  line = list(color = PALETTE$primary, width = 3),
                  marker = list(color = PALETTE$primary, size = 8), name = "IBNR") %>%
        plotly_default_layout(xlab = "Año Ocurrencia", ylab = "IBNR (MXN)")
    })

    output$plot_ldf <- renderPlotly({
      cl <- cl_result()
      dev_labels <- seq_along(cl$ldf) - 1
      df <- data.frame(dev = dev_labels, ldf = cl$ldf)

      plot_ly(df, x = ~dev, y = ~ldf, type = "bar",
              marker = list(color = PALETTE$accent),
              text = ~sprintf("%.4f", ldf), textposition = "outside") %>%
        plotly_default_layout(xlab = "Per\u00edodo de Desarrollo", ylab = "Link Ratio") %>%
        layout(yaxis = list(range = c(0.9, max(cl$ldf) * 1.1)))
    })
  })
}
