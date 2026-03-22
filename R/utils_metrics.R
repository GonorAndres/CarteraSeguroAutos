# ==============================================================================
# FUNCIONES DE METRICAS ACTUARIALES
# Funciones puras para calcular KPIs de siniestralidad
# ==============================================================================

#' Calcular loss ratio por variable(s) de agrupacion
#' @param polizas tibble de polizas
#' @param siniestros tibble de siniestros
#' @param ... variables de agrupacion (unquoted)
#' @return tibble con columnas: grupo, n_polizas, prima_total, n_siniestros, siniestros_total, loss_ratio
calc_loss_ratio <- function(polizas, siniestros, ...) {
  group_vars <- enquos(...)
  has_groups <- length(group_vars) > 0

  if (has_groups) {
    group_names <- purrr::map_chr(group_vars, rlang::quo_name)

    prima_agg <- polizas %>%
      group_by(!!!group_vars) %>%
      summarise(
        n_polizas = n(),
        prima_total = sum(prima_neta, na.rm = TRUE),
        .groups = "drop"
      )

    sin_agg <- siniestros %>%
      left_join(
        polizas %>% select(poliza_id, !!!group_vars),
        by = "poliza_id"
      ) %>%
      group_by(!!!group_vars) %>%
      summarise(
        n_siniestros = n(),
        siniestros_total = sum(monto_pagado, na.rm = TRUE),
        .groups = "drop"
      )

    result <- prima_agg %>%
      left_join(sin_agg, by = group_names) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0L),
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = ifelse(prima_total > 0, siniestros_total / prima_total, NA_real_)
      )
  } else {
    result <- tibble(
      n_polizas = nrow(polizas),
      prima_total = sum(polizas$prima_neta, na.rm = TRUE),
      n_siniestros = nrow(siniestros),
      siniestros_total = sum(siniestros$monto_pagado, na.rm = TRUE)
    ) %>%
      mutate(
        loss_ratio = ifelse(prima_total > 0, siniestros_total / prima_total, NA_real_)
      )
  }

  result
}

#' Calcular frecuencia por variable(s) de agrupacion
#' @param polizas tibble de polizas
#' @param siniestros tibble de siniestros
#' @param ... variables de agrupacion (unquoted)
#' @return tibble con columnas: grupo, n_polizas, n_siniestros, frecuencia
calc_frequency <- function(polizas, siniestros, ...) {
  group_vars <- enquos(...)
  has_groups <- length(group_vars) > 0

  if (has_groups) {
    group_names <- purrr::map_chr(group_vars, rlang::quo_name)

    pol_agg <- polizas %>%
      group_by(!!!group_vars) %>%
      summarise(
        n_polizas = n(),
        exposicion_total = sum(exposicion, na.rm = TRUE),
        .groups = "drop"
      )

    sin_agg <- siniestros %>%
      left_join(
        polizas %>% select(poliza_id, !!!group_vars),
        by = "poliza_id"
      ) %>%
      group_by(!!!group_vars) %>%
      summarise(n_siniestros = n(), .groups = "drop")

    result <- pol_agg %>%
      left_join(sin_agg, by = group_names) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0L),
        frecuencia = ifelse(exposicion_total > 0, n_siniestros / exposicion_total, NA_real_)
      )
  } else {
    exposicion_total <- sum(polizas$exposicion, na.rm = TRUE)
    result <- tibble(
      n_polizas = nrow(polizas),
      n_siniestros = nrow(siniestros),
      exposicion_total = exposicion_total,
      frecuencia = ifelse(exposicion_total > 0, nrow(siniestros) / exposicion_total, NA_real_)
    )
  }

  result
}

#' Calcular severidad por variable(s) de agrupacion
#' @param siniestros tibble de siniestros
#' @param ... variables de agrupacion (unquoted)
#' @return tibble con columnas: grupo, n, mean, median, sd, min, max
calc_severity <- function(siniestros, ...) {
  group_vars <- enquos(...)
  has_groups <- length(group_vars) > 0

  base <- if (has_groups) {
    siniestros %>% group_by(!!!group_vars)
  } else {
    siniestros
  }

  base %>%
    summarise(
      n = n(),
      severidad_media = mean(monto_siniestro, na.rm = TRUE),
      severidad_mediana = median(monto_siniestro, na.rm = TRUE),
      severidad_sd = sd(monto_siniestro, na.rm = TRUE),
      severidad_min = min(monto_siniestro, na.rm = TRUE),
      severidad_max = max(monto_siniestro, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calcular KPIs principales del portafolio
#' @param polizas tibble de polizas
#' @param siniestros tibble de siniestros
#' @return named list con todos los KPIs
calc_kpis <- function(polizas, siniestros) {
  prima_total <- sum(polizas$prima_neta, na.rm = TRUE)
  siniestros_total <- sum(siniestros$monto_pagado, na.rm = TRUE)

  list(
    n_polizas = nrow(polizas),
    n_siniestros = nrow(siniestros),
    prima_total = prima_total,
    siniestros_total = siniestros_total,
    loss_ratio = ifelse(prima_total > 0, siniestros_total / prima_total, NA_real_),
    frecuencia = {
      exp_total <- sum(polizas$exposicion, na.rm = TRUE)
      ifelse(exp_total > 0, nrow(siniestros) / exp_total, NA_real_)
    },
    severidad_media = mean(siniestros$monto_siniestro, na.rm = TRUE),
    severidad_mediana = median(siniestros$monto_siniestro, na.rm = TRUE),
    suma_asegurada_total = sum(polizas$suma_asegurada, na.rm = TRUE)
  )
}

#' Calcular KPIs comparativos entre dos periodos
#' @param kpis_actual named list de KPIs periodo actual
#' @param kpis_anterior named list de KPIs periodo anterior
#' @return named list con cambios porcentuales
calc_kpis_delta <- function(kpis_actual, kpis_anterior) {
  metrics <- c("n_polizas", "n_siniestros", "prima_total", "siniestros_total",
               "loss_ratio", "frecuencia", "severidad_media")

  deltas <- setNames(
    lapply(metrics, function(m) {
      prev <- kpis_anterior[[m]]
      curr <- kpis_actual[[m]]
      if (is.null(prev) || is.null(curr) || prev == 0) return(NA_real_)
      (curr - prev) / abs(prev)
    }),
    paste0("delta_", metrics)
  )

  deltas
}
