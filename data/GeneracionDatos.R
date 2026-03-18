# ==============================================================================
# GENERACION DE DATOS SINTETICOS - SEGUROS AUTO MEXICO (MULTI-AÑO)
# Proyecto: Dashboard Siniestralidad
# Autor: Andres Gonzalez Ortega
# Descripcion: Genera 5 años de polizas y siniestros con patrones de desarrollo,
#              renovaciones/cancelaciones, e inflacion de severidad.
#              Calibrado con parametros del mercado mexicano (CONDUSEF/AMIS).
# ==============================================================================

library(tidyverse)
library(lubridate)

set.seed(1976)

# ==============================================================================
# PARAMETROS DE MERCADO
# ==============================================================================

params_mercado <- list(
  n_new_per_year = 12000,
  lambda_freq    = 0.085,
  avg_severity   = 24000,
  target_loss_ratio = 0.75,
  missing_rate   = 0.06,
  retention_rate = 0.82,
  severity_inflation = 0.04,
  anios = 2020:2024
)

cat("=== Generacion de Datos Multi-Año ===\n")
cat("Años:", paste(params_mercado$anios, collapse = ", "), "\n")
cat("Polizas nuevas/año:", params_mercado$n_new_per_year, "\n\n")

# ==============================================================================
# CATALOGOS
# ==============================================================================

estados_mexico <- tibble(
  estado = c("Estado de Mexico", "Jalisco", "Ciudad de Mexico", "Nuevo Leon",
             "Sonora", "Chihuahua", "Michoacan", "Guanajuato", "Puebla",
             "Veracruz", "Baja California", "Tamaulipas", "Coahuila"),
  cp_base = c(50000L, 44000L, 1000L, 64000L, 83000L, 31000L, 58000L, 36000L,
              72000L, 91000L, 21000L, 87000L, 25000L),
  concentracion = c(0.12, 0.08, 0.06, 0.05, 0.04, 0.04, 0.04, 0.04,
                    0.04, 0.04, 0.04, 0.04, 0.37)
)

vehiculos_catalogo <- tibble(
  marca = c("Nissan", "Nissan", "Nissan", "Volkswagen", "Volkswagen", "Volkswagen",
            "Chevrolet", "Chevrolet", "Toyota", "Toyota", "Ford", "Ford",
            "Honda", "Honda", "Hyundai", "Mazda", "Kia", "Seat"),
  modelo = c("Versa", "March", "Sentra", "Jetta", "Vento", "Polo",
             "Aveo", "Equinox", "Corolla", "Yaris", "Fiesta", "EcoSport",
             "Civic", "City", "Accent", "Mazda3", "Rio", "Ibiza"),
  tipo = c("Sedan", "Hatchback", "Sedan", "Sedan", "Sedan", "Hatchback",
           "Sedan", "SUV", "Sedan", "Hatchback", "Hatchback", "SUV",
           "Sedan", "Sedan", "Sedan", "Sedan", "Sedan", "Hatchback"),
  valor_mediano = c(360000, 288000, 405000, 468000, 378000, 315000,
                    306000, 612000, 432000, 315000, 288000, 522000,
                    468000, 342000, 324000, 414000, 306000, 351000),
  participacion_mercado = c(0.08, 0.06, 0.04, 0.10, 0.05, 0.03,
                            0.06, 0.06, 0.08, 0.04, 0.04, 0.04,
                            0.05, 0.03, 0.04, 0.05, 0.04, 0.04)
)

# ==============================================================================
# FACTORES DE RIESGO (unificados, sin duplicacion)
# ==============================================================================

factor_edad <- function(edad) {
  case_when(
    edad < 25 ~ 1.35,
    edad < 35 ~ 1.10,
    edad < 50 ~ 1.00,
    TRUE      ~ 1.20
  )
}

factor_vehiculo <- function(tipo) {
  case_when(
    tipo == "SUV"       ~ 1.15,
    tipo == "Sedan"     ~ 1.05,
    TRUE                ~ 1.00
  )
}

factor_zona <- function(estado) {
  case_when(
    estado %in% c("Ciudad de Mexico", "Estado de Mexico") ~ 1.30,
    estado %in% c("Jalisco", "Nuevo Leon")                ~ 1.10,
    TRUE                                                   ~ 0.95
  )
}

factor_estacional <- function(mes) {
  case_when(
    mes %in% 6:10    ~ 1.30,
    mes %in% c(1, 2) ~ 0.80,
    TRUE              ~ 1.00
  )
}

# ==============================================================================
# FUNCIONES DE GENERACION
# ==============================================================================

generar_conductores <- function(n) {
  edad <- pmax(pmin(round(rnorm(n, mean = 35, sd = 8)), 75), 18)
  experiencia <- round(pmin(pmax(0, edad - 18 + rnorm(n, 0, 2)), edad - 18))
  score <- pmax(pmin(round(rnorm(n, mean = 650, sd = 60)), 754), 413)

  tibble(
    edad_conductor   = edad,
    experiencia_anos = experiencia,
    score_crediticio = score,
    genero = sample(c("M", "F"), n, prob = c(0.52, 0.48), replace = TRUE)
  )
}

generar_vehiculos_ubicacion <- function(n, anio_ref) {
  estados_sel <- sample(estados_mexico$estado, n,
                        prob = estados_mexico$concentracion, replace = TRUE)

  cps <- vapply(estados_sel, function(est) {
    cp_base <- estados_mexico$cp_base[estados_mexico$estado == est]
    sprintf("%05d", cp_base + sample(0:9999, 1))
  }, character(1))

  vehiculos_idx <- sample(seq_len(nrow(vehiculos_catalogo)), n,
                          prob = vehiculos_catalogo$participacion_mercado,
                          replace = TRUE)

  anos_vehiculo <- anio_ref - rpois(n, lambda = 8)
  anos_vehiculo <- pmax(anos_vehiculo, anio_ref - 20)

  antiguedad <- anio_ref - anos_vehiculo

  valor_comercial <- vehiculos_catalogo$valor_mediano[vehiculos_idx] * case_when(
    antiguedad == 0 ~ 1.0,
    antiguedad == 1 ~ case_when(
      vehiculos_catalogo$marca[vehiculos_idx] %in% c("Toyota", "Honda") ~ 0.85,
      vehiculos_catalogo$marca[vehiculos_idx] %in% c("Volkswagen", "Nissan") ~ 0.82,
      TRUE ~ 0.78
    ),
    antiguedad <= 5 ~ case_when(
      vehiculos_catalogo$marca[vehiculos_idx] %in% c("Toyota", "Honda") ~ 0.85 * 0.92^(antiguedad - 1),
      vehiculos_catalogo$marca[vehiculos_idx] %in% c("Volkswagen", "Nissan") ~ 0.82 * 0.90^(antiguedad - 1),
      TRUE ~ 0.78 * 0.88^(antiguedad - 1)
    ),
    TRUE ~ case_when(
      vehiculos_catalogo$marca[vehiculos_idx] %in% c("Toyota", "Honda") ~ 0.85 * 0.92^4 * 0.96^(antiguedad - 5),
      vehiculos_catalogo$marca[vehiculos_idx] %in% c("Volkswagen", "Nissan") ~ 0.82 * 0.90^4 * 0.95^(antiguedad - 5),
      TRUE ~ 0.78 * 0.88^4 * 0.94^(antiguedad - 5)
    )
  )

  tibble(
    estado          = estados_sel,
    codigo_postal   = cps,
    marca_vehiculo  = vehiculos_catalogo$marca[vehiculos_idx],
    modelo_vehiculo = vehiculos_catalogo$modelo[vehiculos_idx],
    tipo_vehiculo   = vehiculos_catalogo$tipo[vehiculos_idx],
    ano_vehiculo    = anos_vehiculo,
    valor_comercial = valor_comercial,
    canal_venta = sample(c("Agente", "Directo", "Banco", "Digital"), n,
                         prob = c(0.45, 0.25, 0.20, 0.10), replace = TRUE)
  )
}

calcular_prima <- function(freq_esperada, sev_esperada, suma_aseg) {
  prima_pura <- freq_esperada * sev_esperada
  recargos <- -0.05
  factor_suma <- log(pmax(suma_aseg, 1) / 160000) * 0.02 + 1
  prima_pura * (1 + recargos) * factor_suma
}

generar_polizas_nuevas <- function(n, anio, id_offset = 0L) {
  conductores <- generar_conductores(n)
  vehiculos   <- generar_vehiculos_ubicacion(n, anio)

  fecha_inicio <- as.Date(paste0(anio, "-01-01")) + days(sample(0:364, n, replace = TRUE))
  fecha_vencimiento <- fecha_inicio + years(1)

  corte <- as.Date(paste0(max(params_mercado$anios), "-12-31"))
  exposicion_dias <- as.numeric(pmin(corte - fecha_inicio, 365))
  exposicion <- pmax(exposicion_dias / 365, 0)

  polizas <- tibble(
    poliza_id = (id_offset + 1L):(id_offset + n),
    anio_suscripcion   = anio,
    numero_renovacion  = 0L,
    poliza_original_id = NA_integer_,
    poliza_status      = "Vigente"
  ) %>%
    bind_cols(conductores) %>%
    bind_cols(vehiculos) %>%
    mutate(
      fecha_inicio      = fecha_inicio,
      fecha_vencimiento = fecha_vencimiento,
      exposicion        = exposicion,
      suma_asegurada    = round(valor_comercial * runif(n, 0.7, 1.1)),
      freq_esperada = params_mercado$lambda_freq *
        factor_edad(edad_conductor) *
        factor_vehiculo(tipo_vehiculo) *
        factor_zona(estado) *
        exposicion,
      sev_esperada = params_mercado$avg_severity *
        (1 + params_mercado$severity_inflation)^(anio - 2020) *
        (1 + (suma_asegurada - 160000) / 1e6) *
        factor_vehiculo(tipo_vehiculo),
      prima_neta = calcular_prima(freq_esperada, sev_esperada, suma_asegurada)
    ) %>%
    select(-valor_comercial)

  polizas
}

renovar_polizas <- function(prev_polizas, prev_siniestros, anio, id_offset) {
  vigentes <- prev_polizas %>%
    filter(poliza_status == "Vigente")

  tuvo_siniestro <- prev_siniestros %>%
    distinct(poliza_id) %>%
    mutate(tuvo_siniestro = TRUE)

  candidatos <- vigentes %>%
    left_join(tuvo_siniestro, by = "poliza_id") %>%
    mutate(
      tuvo_siniestro = replace_na(tuvo_siniestro, FALSE),
      prob_renovacion = params_mercado$retention_rate *
        ifelse(tuvo_siniestro, 0.90, 1.0) *
        ifelse(edad_conductor < 25, 0.95, 1.0)
    )

  renueva <- runif(nrow(candidatos)) < candidatos$prob_renovacion
  renovadas <- candidatos[renueva, ]
  no_renovadas_ids <- candidatos$poliza_id[!renueva]

  if (nrow(renovadas) == 0) {
    return(list(polizas = tibble(), no_renovadas_ids = no_renovadas_ids))
  }

  n <- nrow(renovadas)
  fecha_inicio <- renovadas$fecha_vencimiento
  fecha_vencimiento <- fecha_inicio + years(1)
  corte <- as.Date(paste0(max(params_mercado$anios), "-12-31"))
  exposicion <- pmax(as.numeric(pmin(corte - fecha_inicio, 365)) / 365, 0)

  nuevas_renovadas <- tibble(
    poliza_id          = (id_offset + 1L):(id_offset + n),
    anio_suscripcion   = anio,
    numero_renovacion  = renovadas$numero_renovacion + 1L,
    poliza_original_id = coalesce(renovadas$poliza_original_id, renovadas$poliza_id),
    poliza_status      = "Vigente",
    edad_conductor     = renovadas$edad_conductor + 1L,
    experiencia_anos   = pmin(replace_na(renovadas$experiencia_anos, 0L) + 1L, pmax(renovadas$edad_conductor - 17L, 0L)),
    score_crediticio   = renovadas$score_crediticio,
    genero             = renovadas$genero,
    estado             = renovadas$estado,
    codigo_postal      = renovadas$codigo_postal,
    marca_vehiculo     = renovadas$marca_vehiculo,
    modelo_vehiculo    = renovadas$modelo_vehiculo,
    tipo_vehiculo      = renovadas$tipo_vehiculo,
    ano_vehiculo       = renovadas$ano_vehiculo,
    canal_venta        = renovadas$canal_venta,
    fecha_inicio       = fecha_inicio,
    fecha_vencimiento  = fecha_vencimiento,
    exposicion         = exposicion,
    suma_asegurada     = round(renovadas$suma_asegurada * 0.95)
  ) %>%
    mutate(
      freq_esperada = params_mercado$lambda_freq *
        factor_edad(edad_conductor) *
        factor_vehiculo(tipo_vehiculo) *
        factor_zona(estado) *
        exposicion,
      sev_esperada = params_mercado$avg_severity *
        (1 + params_mercado$severity_inflation)^(anio - 2020) *
        (1 + (suma_asegurada - 160000) / 1e6) *
        factor_vehiculo(tipo_vehiculo),
      prima_neta = calcular_prima(freq_esperada, sev_esperada, suma_asegurada)
    )

  list(polizas = nuevas_renovadas, no_renovadas_ids = no_renovadas_ids)
}

# ==============================================================================
# GENERACION DE SINIESTROS CON DESARROLLO
# ==============================================================================

# Factores de desarrollo acumulado (auto: short-tail)
DEV_FACTORS <- c(0.60, 0.85, 0.95, 0.99, 1.00)

generar_siniestros_anio <- function(polizas_anio, anio, anio_max, id_offset = 0L) {
  n_pol <- nrow(polizas_anio)
  freq_lambda <- pmax(replace_na(polizas_anio$freq_esperada, 0), 0)
  n_siniestros <- rpois(n_pol, freq_lambda)

  pol_ids <- rep(polizas_anio$poliza_id, n_siniestros)
  if (length(pol_ids) == 0) return(tibble())

  pol_info <- polizas_anio[rep(seq_len(n_pol), n_siniestros), ]
  n_total <- nrow(pol_info)

  dias_vigencia <- pmax(as.numeric(pol_info$exposicion * 365), 1)
  dia_siniestro <- vapply(dias_vigencia, function(d) sample.int(as.integer(d), 1), integer(1))

  fecha_siniestro <- pol_info$fecha_inicio + days(dia_siniestro)
  mes_siniestro <- month(fecha_siniestro)

  tipo_siniestro <- sample(
    c("Colision", "Robo Total", "Robo Parcial", "Danos", "Incendio"),
    n_total, replace = TRUE, prob = c(0.65, 0.04, 0.10, 0.20, 0.01)
  )

  monto_base <- rgamma(n_total, shape = 2, scale = 8000)

  inflacion <- (1 + params_mercado$severity_inflation)^(anio - 2020)
  estacional <- factor_estacional(mes_siniestro)

  monto_siniestro <- case_when(
    tipo_siniestro == "Robo Total"   ~ pol_info$suma_asegurada * runif(n_total, 0.85, 1.0),
    tipo_siniestro == "Colision"     ~ monto_base * runif(n_total, 0.7, 1.6),
    tipo_siniestro == "Incendio"     ~ pol_info$suma_asegurada * runif(n_total, 0.70, 0.95),
    TRUE                             ~ monto_base
  ) * inflacion * rnorm(n_total, estacional, 0.1)

  monto_siniestro <- pmax(monto_siniestro, 500)

  deducible <- case_when(
    tipo_siniestro == "Robo Total" ~ monto_siniestro * 0.15,
    TRUE                           ~ pmax(monto_siniestro * 0.15, 1800)
  )

  monto_neto <- pmax(0, monto_siniestro - deducible)

  fecha_reporte <- fecha_siniestro + days(sample(0:15, n_total, replace = TRUE))

  # Estado del siniestro (depende del desarrollo disponible)
  max_dev <- anio_max - anio
  if (max_dev >= 2) {
    estado_siniestro <- sample(c("Pagado", "Pagado", "Pagado", "Pagado", "Pagado",
                                  "Pagado", "Pagado", "Pagado", "Rechazado", "Rechazado"),
                                n_total, replace = TRUE)
  } else if (max_dev == 1) {
    estado_siniestro <- sample(c("Pagado", "En proceso", "Rechazado"),
                                n_total, replace = TRUE, prob = c(0.78, 0.19, 0.03))
  } else {
    estado_siniestro <- sample(c("Pagado", "En proceso", "Rechazado"),
                                n_total, replace = TRUE, prob = c(0.55, 0.42, 0.03))
  }

  monto_pagado_final <- case_when(
    estado_siniestro == "Pagado" ~ monto_neto,
    TRUE                         ~ 0
  )

  # Monto reserva para siniestros abiertos
  monto_reserva <- case_when(
    estado_siniestro == "En proceso" ~ monto_neto * runif(n_total, 0.8, 1.2),
    TRUE                             ~ 0
  )

  siniestros_base <- tibble(
    siniestro_id     = (id_offset + 1L):(id_offset + n_total),
    poliza_id        = pol_ids,
    anio_ocurrencia  = anio,
    fecha_siniestro  = fecha_siniestro,
    fecha_reporte    = fecha_reporte,
    tipo_siniestro   = tipo_siniestro,
    monto_siniestro  = round(monto_siniestro, 2),
    deducible        = round(deducible, 2),
    monto_neto       = round(monto_neto, 2),
    estado_siniestro = estado_siniestro,
    monto_reserva    = round(monto_reserva, 2),
    mes_siniestro    = mes_siniestro
  )

  # Generar pagos por desarrollo
  pagos <- generar_desarrollo(siniestros_base, anio, anio_max)

  list(siniestros = siniestros_base, pagos = pagos)
}

generar_desarrollo <- function(siniestros, anio_ocurrencia, anio_max) {
  pagados <- siniestros %>%
    filter(estado_siniestro == "Pagado", monto_neto > 0)

  if (nrow(pagados) == 0) return(tibble())

  max_dev <- anio_max - anio_ocurrencia
  if (max_dev < 0) return(tibble())

  dev_years <- 0:min(max_dev, length(DEV_FACTORS) - 1)

  pagos_list <- lapply(seq_len(nrow(pagados)), function(i) {
    sin <- pagados[i, ]
    monto_total <- sin$monto_neto

    # Factores acumulados con ruido
    cumul <- DEV_FACTORS[dev_years + 1] * rnorm(length(dev_years), 1, 0.03)
    cumul <- pmin(pmax(cumul, 0.01), 1.0)
    cumul <- sort(cumul)
    if (length(cumul) > 0) cumul[length(cumul)] <- min(cumul[length(cumul)], 1.0)

    incremental <- c(cumul[1], diff(cumul))
    incremental <- pmax(incremental, 0)
    incremental <- incremental / sum(incremental)

    tibble(
      siniestro_id    = sin$siniestro_id,
      anio_ocurrencia = anio_ocurrencia,
      anio_desarrollo = dev_years,
      anio_calendario = anio_ocurrencia + dev_years,
      numero_pago     = seq_along(dev_years),
      monto_pago      = round(monto_total * incremental, 2),
      monto_acumulado = round(monto_total * cumul, 2)
    )
  })

  bind_rows(pagos_list)
}

# ==============================================================================
# GENERACION MULTI-AÑO
# ==============================================================================

cat("Generando datos multi-año...\n")

all_polizas    <- list()
all_siniestros <- list()
all_pagos      <- list()

poliza_id_counter    <- 0L
siniestro_id_counter <- 0L
prev_year_polizas    <- NULL
prev_year_siniestros <- NULL

for (anio in params_mercado$anios) {
  cat(sprintf("\n--- Año %d ---\n", anio))

  # Polizas nuevas
  nuevas <- generar_polizas_nuevas(
    params_mercado$n_new_per_year, anio, poliza_id_counter
  )
  poliza_id_counter <- poliza_id_counter + nrow(nuevas)
  cat(sprintf("  Nuevas: %d\n", nrow(nuevas)))

  # Renovaciones
  renovadas <- tibble()
  if (!is.null(prev_year_polizas)) {
    ren_result <- renovar_polizas(
      prev_year_polizas, prev_year_siniestros, anio, poliza_id_counter
    )
    renovadas <- ren_result$polizas
    poliza_id_counter <- poliza_id_counter + nrow(renovadas)

    # Actualizar status de no renovadas en año anterior
    if (length(ren_result$no_renovadas_ids) > 0) {
      idx <- which(all_polizas[[length(all_polizas)]]$poliza_id %in% ren_result$no_renovadas_ids)
      all_polizas[[length(all_polizas)]]$poliza_status[idx] <- "No Renovada"
    }

    cat(sprintf("  Renovadas: %d\n", nrow(renovadas)))
    cat(sprintf("  No renovadas: %d\n", length(ren_result$no_renovadas_ids)))
  }

  polizas_anio <- bind_rows(nuevas, renovadas)

  # Generar siniestros
  sin_result <- generar_siniestros_anio(
    polizas_anio, anio, max(params_mercado$anios), siniestro_id_counter
  )
  siniestro_id_counter <- siniestro_id_counter + nrow(sin_result$siniestros)

  cat(sprintf("  Siniestros: %d\n", nrow(sin_result$siniestros)))
  cat(sprintf("  Pagos de desarrollo: %d\n", nrow(sin_result$pagos)))

  all_polizas[[as.character(anio)]]    <- polizas_anio
  all_siniestros[[as.character(anio)]] <- sin_result$siniestros
  all_pagos[[as.character(anio)]]      <- sin_result$pagos

  prev_year_polizas    <- polizas_anio
  prev_year_siniestros <- sin_result$siniestros
}

# ==============================================================================
# CONSOLIDAR DATASETS
# ==============================================================================

dataset_polizas <- bind_rows(all_polizas)
dataset_siniestros <- bind_rows(all_siniestros)
dataset_pagos <- bind_rows(all_pagos)

# Agregar monto_pagado a siniestros (total pagado hasta la fecha)
pagos_totales <- dataset_pagos %>%
  group_by(siniestro_id) %>%
  summarise(monto_pagado = sum(monto_pago, na.rm = TRUE), .groups = "drop")

dataset_siniestros <- dataset_siniestros %>%
  left_join(pagos_totales, by = "siniestro_id") %>%
  mutate(monto_pagado = replace_na(monto_pagado, 0))

# ==============================================================================
# INTRODUCIR MISSING VALUES
# ==============================================================================

introducir_missing <- function(polizas_df, rate = 0.06) {
  polizas_df %>%
    mutate(
      score_crediticio = ifelse(runif(n()) < rate, NA_real_, score_crediticio),
      experiencia_anos = ifelse(edad_conductor < 25 & runif(n()) < 0.08,
                                NA_integer_, experiencia_anos)
    )
}

dataset_polizas <- introducir_missing(dataset_polizas, params_mercado$missing_rate)

# ==============================================================================
# VALIDACION KPIs
# ==============================================================================

validar_kpis <- function(polizas_df, siniestros_df) {
  prima_total <- sum(polizas_df$prima_neta, na.rm = TRUE)
  pagado_total <- sum(siniestros_df$monto_pagado, na.rm = TRUE)
  loss_ratio <- pagado_total / prima_total
  freq <- nrow(siniestros_df) / nrow(polizas_df)
  sev <- mean(siniestros_df$monto_siniestro[siniestros_df$estado_siniestro == "Pagado"],
              na.rm = TRUE)

  list(
    loss_ratio = loss_ratio,
    frecuencia = freq,
    severidad  = sev,
    n_polizas  = nrow(polizas_df),
    n_siniestros = nrow(siniestros_df),
    prima_total  = prima_total,
    pagado_total = pagado_total,
    target_achieved = loss_ratio >= 0.60 & loss_ratio <= 0.90
  )
}

kpis <- validar_kpis(dataset_polizas, dataset_siniestros)

cat("\n=== VALIDACION KPIs ===\n")
cat(sprintf("  Polizas totales:  %s\n", format(kpis$n_polizas, big.mark = ",")))
cat(sprintf("  Siniestros:       %s\n", format(kpis$n_siniestros, big.mark = ",")))
cat(sprintf("  Prima total:      $%s\n", format(round(kpis$prima_total), big.mark = ",")))
cat(sprintf("  Pagado total:     $%s\n", format(round(kpis$pagado_total), big.mark = ",")))
cat(sprintf("  Loss Ratio:       %.2f%% (target: 75%%)\n", kpis$loss_ratio * 100))
cat(sprintf("  Frecuencia:       %.2f%% (target: 8.5%%)\n", kpis$frecuencia * 100))
cat(sprintf("  Severidad media:  $%s (target: $24,000)\n", format(round(kpis$severidad), big.mark = ",")))
cat(sprintf("  Target alcanzado: %s\n", kpis$target_achieved))

# KPIs por año
cat("\n=== KPIs POR AÑO ===\n")
for (anio in params_mercado$anios) {
  p_anio <- dataset_polizas %>% filter(anio_suscripcion == anio)
  s_anio <- dataset_siniestros %>% filter(anio_ocurrencia == anio)
  k <- validar_kpis(p_anio, s_anio)
  cat(sprintf("  %d: Polizas=%s, Siniestros=%s, LR=%.1f%%, Freq=%.1f%%\n",
              anio, format(k$n_polizas, big.mark = ","),
              format(k$n_siniestros, big.mark = ","),
              k$loss_ratio * 100, k$frecuencia * 100))
}

# ==============================================================================
# PREPARAR DATOS FINALES
# ==============================================================================

dataset_polizas_final <- dataset_polizas %>%
  select(poliza_id, anio_suscripcion, numero_renovacion, poliza_original_id,
         poliza_status, edad_conductor, experiencia_anos, score_crediticio,
         genero, estado, codigo_postal, marca_vehiculo, modelo_vehiculo,
         tipo_vehiculo, ano_vehiculo, canal_venta, fecha_inicio,
         fecha_vencimiento, exposicion, suma_asegurada, prima_neta)

dataset_siniestros_final <- dataset_siniestros %>%
  select(siniestro_id, poliza_id, anio_ocurrencia, fecha_siniestro,
         fecha_reporte, tipo_siniestro, monto_siniestro, deducible,
         monto_neto, monto_pagado, estado_siniestro, monto_reserva,
         mes_siniestro)

dataset_pagos_final <- dataset_pagos %>%
  select(siniestro_id, anio_ocurrencia, anio_desarrollo, anio_calendario,
         numero_pago, monto_pago, monto_acumulado)

# ==============================================================================
# EXPORTAR
# ==============================================================================

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write_csv(dataset_polizas_final, "data/processed/polizas_sinteticas.csv")
write_csv(dataset_siniestros_final, "data/processed/siniestros_sinteticos.csv")
write_csv(dataset_pagos_final, "data/processed/pagos_desarrollo.csv")

resumen <- list(
  fecha_generacion = as.character(today()),
  version = "2.0-multianio",
  parametros = params_mercado,
  kpis = kpis,
  estructura = list(
    n_polizas = nrow(dataset_polizas_final),
    n_siniestros = nrow(dataset_siniestros_final),
    n_pagos = nrow(dataset_pagos_final),
    anios = params_mercado$anios,
    estados = sort(unique(dataset_polizas_final$estado)),
    tipos_vehiculo = sort(unique(dataset_polizas_final$tipo_vehiculo))
  )
)

jsonlite::write_json(resumen, "data/processed/resumen_generacion.json",
                     pretty = TRUE, auto_unbox = TRUE)

cat("\n=== Archivos generados ===\n")
cat("  data/processed/polizas_sinteticas.csv\n")
cat("  data/processed/siniestros_sinteticos.csv\n")
cat("  data/processed/pagos_desarrollo.csv\n")
cat("  data/processed/resumen_generacion.json\n")
cat("\nGeneracion completada exitosamente.\n")
