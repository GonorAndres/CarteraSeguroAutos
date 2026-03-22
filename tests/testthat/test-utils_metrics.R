library(testthat)
library(tidyverse)

# Source utils sin cargar Shiny
source(here::here("R/utils_metrics.R"), local = TRUE)

# --- Test data ---
polizas_test <- tibble(
  poliza_id  = 1:10,
  prima_neta = c(5000, 6000, 4500, 7000, 5500, 6500, 4000, 8000, 5000, 6000),
  estado     = c("CDMX", "CDMX", "JAL", "JAL", "NL", "NL", "CDMX", "JAL", "NL", "CDMX"),
  canal_venta = c("Agente", "Directo", "Agente", "Banco", "Digital", "Agente",
                   "Directo", "Banco", "Agente", "Digital"),
  edad_conductor = c(22, 35, 45, 28, 60, 33, 40, 25, 55, 30),
  tipo_vehiculo = c("Sedan", "SUV", "Hatchback", "Sedan", "SUV",
                     "Sedan", "Hatchback", "SUV", "Sedan", "Hatchback"),
  suma_asegurada = c(200000, 350000, 150000, 400000, 300000,
                      250000, 120000, 500000, 280000, 180000),
  exposicion = rep(1.0, 10)
)

siniestros_test <- tibble(
  siniestro_id    = 1:4,
  poliza_id       = c(1, 3, 5, 8),
  monto_siniestro = c(15000, 8000, 25000, 40000),
  monto_pagado    = c(12000, 6500, 20000, 35000),
  tipo_siniestro  = c("Colision", "Danos", "Robo Total", "Colision"),
  estado_siniestro = c("Pagado", "Pagado", "Pagado", "Pagado")
)

# --- calc_loss_ratio ---
test_that("calc_loss_ratio global devuelve valores correctos", {
  result <- calc_loss_ratio(polizas_test, siniestros_test)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_polizas, 10)
  expect_equal(result$n_siniestros, 4)
  expect_equal(result$prima_total, sum(polizas_test$prima_neta))
  expect_equal(result$siniestros_total, sum(siniestros_test$monto_pagado))
  expect_equal(result$loss_ratio, sum(siniestros_test$monto_pagado) / sum(polizas_test$prima_neta))
})

test_that("calc_loss_ratio por grupo funciona correctamente", {
  result <- calc_loss_ratio(polizas_test, siniestros_test, estado)
  expect_true("estado" %in% names(result))
  expect_equal(nrow(result), 3)  # CDMX, JAL, NL
  expect_true(all(result$loss_ratio >= 0 | is.na(result$loss_ratio)))
})

test_that("calc_loss_ratio maneja grupos sin siniestros correctamente", {
  result <- calc_loss_ratio(polizas_test, siniestros_test, canal_venta)
  # Banco tiene siniestro (poliza 8), Digital tiene siniestro (poliza 5)
  # Verificar que todos los canales aparecen y tienen loss_ratio >= 0
  expect_true(all(result$n_siniestros >= 0))
  expect_true(all(result$loss_ratio >= 0 | is.na(result$loss_ratio)))
  expect_true(all(c("Agente", "Directo", "Banco", "Digital") %in% result$canal_venta))
})

# --- calc_frequency ---
test_that("calc_frequency global es correcta", {
  result <- calc_frequency(polizas_test, siniestros_test)
  expect_equal(result$frecuencia, 4 / 10)
})

test_that("calc_frequency por grupo funciona", {
  result <- calc_frequency(polizas_test, siniestros_test, estado)
  expect_equal(nrow(result), 3)
  expect_true(all(result$frecuencia >= 0))
})

# --- calc_severity ---
test_that("calc_severity global calcula estadisticos correctamente", {
  result <- calc_severity(siniestros_test)
  expect_equal(result$n, 4)
  expect_equal(result$severidad_media, mean(siniestros_test$monto_siniestro))
  expect_equal(result$severidad_mediana, median(siniestros_test$monto_siniestro))
})

test_that("calc_severity por tipo funciona", {
  result <- calc_severity(siniestros_test, tipo_siniestro)
  expect_true("tipo_siniestro" %in% names(result))
  expect_equal(nrow(result), 3)  # Colision, Danos, Robo Total
})

# --- calc_kpis ---
test_that("calc_kpis devuelve todos los campos necesarios", {
  kpis <- calc_kpis(polizas_test, siniestros_test)
  campos <- c("n_polizas", "n_siniestros", "prima_total", "siniestros_total",
              "loss_ratio", "frecuencia", "severidad_media", "severidad_mediana",
              "suma_asegurada_total")
  expect_true(all(campos %in% names(kpis)))
})

test_that("calc_kpis valores son coherentes", {
  kpis <- calc_kpis(polizas_test, siniestros_test)
  expect_equal(kpis$loss_ratio, kpis$siniestros_total / kpis$prima_total)
  exp_total <- sum(polizas_test$exposicion)
  expect_equal(kpis$frecuencia, kpis$n_siniestros / exp_total)
  expect_true(kpis$severidad_media > 0)
})

# --- calc_kpis_delta ---
test_that("calc_kpis_delta calcula cambios correctamente", {
  kpis_1 <- list(n_polizas = 100, n_siniestros = 10, prima_total = 500000,
                 siniestros_total = 350000, loss_ratio = 0.70, frecuencia = 0.10,
                 severidad_media = 35000)
  kpis_2 <- list(n_polizas = 110, n_siniestros = 12, prima_total = 550000,
                 siniestros_total = 385000, loss_ratio = 0.70, frecuencia = 0.109,
                 severidad_media = 32083)

  deltas <- calc_kpis_delta(kpis_2, kpis_1)
  expect_equal(deltas$delta_n_polizas, 0.10)
  expect_true(!is.na(deltas$delta_loss_ratio))
})

# --- Edge cases: empty data ---
test_that("calc_loss_ratio handles empty siniestros", {
  result <- calc_loss_ratio(polizas_test, tibble(
    siniestro_id = integer(), poliza_id = integer(),
    monto_pagado = numeric(), monto_siniestro = numeric()
  ))
  expect_equal(result$n_siniestros, 0)
  expect_equal(result$siniestros_total, 0)
  expect_equal(result$loss_ratio, 0)
})

test_that("calc_frequency handles empty siniestros", {
  result <- calc_frequency(polizas_test, tibble(
    siniestro_id = integer(), poliza_id = integer()
  ))
  expect_equal(result$n_siniestros, 0)
  expect_equal(result$frecuencia, 0)
})

test_that("calc_kpis_delta handles zero denominator", {
  kpis_zero <- list(n_polizas = 0, n_siniestros = 0, prima_total = 0,
                    siniestros_total = 0, loss_ratio = 0, frecuencia = 0,
                    severidad_media = 0)
  kpis_curr <- list(n_polizas = 100, n_siniestros = 10, prima_total = 500000,
                    siniestros_total = 350000, loss_ratio = 0.70, frecuencia = 0.10,
                    severidad_media = 35000)
  deltas <- calc_kpis_delta(kpis_curr, kpis_zero)
  expect_true(is.na(deltas$delta_n_polizas))
})

test_that("calc_frequency uses exposure-adjusted denominator", {
  polizas_partial <- tibble(
    poliza_id = 1:4, exposicion = c(1.0, 0.5, 0.25, 1.0),
    prima_neta = rep(5000, 4)
  )
  siniestros_2 <- tibble(
    siniestro_id = 1:2, poliza_id = c(1, 2)
  )
  result <- calc_frequency(polizas_partial, siniestros_2)
  # 2 claims / 2.75 exposure-years = 0.7273
  expect_equal(result$frecuencia, 2 / 2.75, tolerance = 1e-4)
})
