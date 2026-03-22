library(testthat)
library(tidyverse)
library(DBI)
library(RSQLite)

# --- Validate generated data integrity ---
test_that("generated data files exist", {
  expect_true(file.exists(here::here("data/processed/polizas_sinteticas.csv")))
  expect_true(file.exists(here::here("data/processed/siniestros_sinteticos.csv")))
  expect_true(file.exists(here::here("data/processed/pagos_desarrollo.csv")))
  expect_true(file.exists(here::here("data/siniestralidad.db")))
})

test_that("SQLite database has all tables", {
  con <- dbConnect(SQLite(), here::here("data/siniestralidad.db"))
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)
  expect_true("polizas" %in% tables)
  expect_true("siniestros" %in% tables)
  expect_true("pagos_desarrollo" %in% tables)
})

# Load data for remaining tests (inside a tryCatch to avoid crashing the runner)
db_path <- here::here("data/siniestralidad.db")
if (file.exists(db_path)) {
  con <- dbConnect(SQLite(), db_path)
  polizas <- dbGetQuery(con, "SELECT * FROM polizas") %>% as_tibble()
  siniestros <- dbGetQuery(con, "SELECT * FROM siniestros") %>% as_tibble()
  pagos <- dbGetQuery(con, "SELECT * FROM pagos_desarrollo") %>% as_tibble()
  dbDisconnect(con)
} else {
  polizas <- tibble()
  siniestros <- tibble()
  pagos <- tibble()
}

# --- Polizas validation ---
test_that("polizas has expected volume (>50K)", {
  expect_true(nrow(polizas) > 50000)
})

test_that("polizas covers 5 subscription years", {
  years <- sort(unique(polizas$anio_suscripcion))
  expect_equal(years, 2020:2024)
})

test_that("polizas prima_neta is mostly positive", {
  pct_positive <- mean(polizas$prima_neta > 0, na.rm = TRUE)
  expect_true(pct_positive > 0.99, info = paste("% positive:", round(pct_positive * 100, 2)))
})

test_that("polizas has no negative suma_asegurada", {
  expect_true(all(polizas$suma_asegurada > 0, na.rm = TRUE))
})

test_that("polizas edad is in valid range", {
  expect_true(all(polizas$edad_conductor >= 18 & polizas$edad_conductor <= 76, na.rm = TRUE))
})

test_that("polizas has 13 states", {
  expect_equal(length(unique(polizas$estado)), 13)
})

test_that("poliza_id is unique", {
  expect_equal(length(unique(polizas$poliza_id)), nrow(polizas))
})

# --- Siniestros validation ---
test_that("siniestros has expected volume (>5K)", {
  expect_true(nrow(siniestros) > 5000)
})

test_that("all siniestros reference valid polizas", {
  expect_true(all(siniestros$poliza_id %in% polizas$poliza_id))
})

test_that("siniestros has no negative montos", {
  expect_true(all(siniestros$monto_siniestro >= 0, na.rm = TRUE))
  expect_true(all(siniestros$monto_pagado >= 0, na.rm = TRUE))
  expect_true(all(siniestros$deducible >= 0, na.rm = TRUE))
})

test_that("siniestros covers expected claim types", {
  tipos <- unique(siniestros$tipo_siniestro)
  expect_true("Colision" %in% tipos)
  expect_true("Robo Total" %in% tipos)
  expect_true("Danos" %in% tipos)
})

# --- KPI validation ---
test_that("overall loss ratio is in acceptable range (0.50-0.90)", {
  lr <- sum(siniestros$monto_pagado, na.rm = TRUE) / sum(polizas$prima_neta, na.rm = TRUE)
  expect_true(lr >= 0.50 && lr <= 0.90,
              info = paste("Loss ratio:", round(lr, 4)))
})

test_that("overall frequency is in acceptable range (0.05-0.15)", {
  freq <- nrow(siniestros) / nrow(polizas)
  expect_true(freq >= 0.05 && freq <= 0.15,
              info = paste("Frequency:", round(freq, 4)))
})

# --- Pagos validation ---
test_that("pagos has development years 0-4", {
  dev_years <- sort(unique(pagos$anio_desarrollo))
  expect_true(0 %in% dev_years)
  expect_true(max(dev_years) <= 4)
})

test_that("all pagos reference valid siniestros", {
  expect_true(all(pagos$siniestro_id %in% siniestros$siniestro_id))
})

test_that("pagos monto_acumulado is non-decreasing within each siniestro", {
  check <- pagos %>%
    arrange(siniestro_id, anio_desarrollo) %>%
    group_by(siniestro_id) %>%
    mutate(is_increasing = monto_acumulado >= lag(monto_acumulado, default = 0)) %>%
    ungroup()
  expect_true(all(check$is_increasing))
})

# --- Data integrity: claim cap ---
test_that("no monto_siniestro exceeds suma_asegurada", {
  sin_with_sa <- siniestros %>%
    inner_join(polizas %>% select(poliza_id, suma_asegurada), by = "poliza_id")
  violations <- sin_with_sa %>% filter(monto_siniestro > suma_asegurada)
  expect_equal(nrow(violations), 0,
               info = paste("Found", nrow(violations), "claims exceeding suma_asegurada"))
})

# --- Data integrity: premium is non-negative ---
test_that("prima_neta is non-negative for all policies", {
  expect_true(all(polizas$prima_neta >= 0, na.rm = TRUE),
              info = "Found policies with negative prima_neta")
  pct_positive <- mean(polizas$prima_neta > 0, na.rm = TRUE)
  expect_true(pct_positive > 0.99,
              info = paste("% positive:", round(pct_positive * 100, 2)))
})

# --- Data integrity: rejection rates are realistic ---
test_that("rejection rate for mature years is under 10%", {
  mature <- siniestros %>% filter(anio_ocurrencia <= 2022)
  if (nrow(mature) > 0) {
    reject_pct <- mean(mature$estado_siniestro == "Rechazado")
    expect_true(reject_pct < 0.10,
                info = paste("Rejection rate:", round(reject_pct * 100, 1), "%"))
  }
})
