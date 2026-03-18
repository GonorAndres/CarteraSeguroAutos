library(testthat)
library(tidyverse)
library(DBI)
library(RSQLite)

source(here::here("R/utils_metrics.R"), local = TRUE)
source(here::here("R/utils_data.R"), local = TRUE)

# --- load_data ---
db_path <- here::here("data/siniestralidad.db")

test_that("load_data returns list with polizas and siniestros", {
  d <- load_data(db_path)
  expect_type(d, "list")
  expect_true("polizas" %in% names(d))
  expect_true("siniestros" %in% names(d))
  expect_s3_class(d$polizas, "tbl_df")
  expect_s3_class(d$siniestros, "tbl_df")
  expect_true(nrow(d$polizas) > 0)
  expect_true(nrow(d$siniestros) > 0)
})

test_that("load_data parses dates correctly", {
  d <- load_data(db_path)
  expect_s3_class(d$polizas$fecha_inicio, "Date")
  expect_s3_class(d$polizas$fecha_vencimiento, "Date")
  expect_s3_class(d$siniestros$fecha_siniestro, "Date")
})

# --- enrich_polizas ---
test_that("enrich_polizas adds segmentation columns", {
  d <- load_data(db_path)
  enriched <- enrich_polizas(d$polizas)
  expected_cols <- c("rango_edad", "segmento_edad", "segmento_score", "zona_riesgo")
  for (col in expected_cols) {
    expect_true(col %in% names(enriched), info = paste("Missing column:", col))
  }
})

test_that("enrich_polizas rango_edad has correct levels", {
  d <- load_data(db_path)
  enriched <- enrich_polizas(d$polizas)
  levels_expected <- c("18-25", "26-35", "36-45", "46-55", "56+")
  actual_levels <- levels(enriched$rango_edad)
  expect_equal(actual_levels, levels_expected)
})

test_that("enrich_polizas zona_riesgo maps states correctly", {
  polizas_mini <- tibble(
    poliza_id = 1:3,
    edad_conductor = c(30, 40, 50),
    score_crediticio = c(700, 500, NA),
    estado = c("Ciudad de Mexico", "Jalisco", "Sonora")
  )
  enriched <- enrich_polizas(polizas_mini)
  expect_equal(enriched$zona_riesgo, c("Zona Alta", "Zona Media", "Zona Baja"))
})

# --- filter_data ---
test_that("filter_data filters by estado", {
  d <- load_data(db_path)
  d$polizas <- enrich_polizas(d$polizas)
  filters <- list(estado = "Jalisco")
  result <- filter_data(d, filters)
  expect_true(all(result$polizas$estado == "Jalisco"))
  expect_true(nrow(result$polizas) < nrow(d$polizas))
})

test_that("filter_data filters by edad_range", {
  d <- load_data(db_path)
  d$polizas <- enrich_polizas(d$polizas)
  filters <- list(edad_range = c(25, 35))
  result <- filter_data(d, filters)
  expect_true(all(result$polizas$edad_conductor >= 25))
  expect_true(all(result$polizas$edad_conductor <= 35))
})

test_that("filter_data siniestros only include filtered polizas", {
  d <- load_data(db_path)
  d$polizas <- enrich_polizas(d$polizas)
  filters <- list(estado = "Jalisco")
  result <- filter_data(d, filters)
  sin_poliza_ids <- unique(result$siniestros$poliza_id)
  pol_ids <- result$polizas$poliza_id
  expect_true(all(sin_poliza_ids %in% pol_ids))
})

test_that("filter_data with no filters returns all data", {
  d <- load_data(db_path)
  d$polizas <- enrich_polizas(d$polizas)
  result <- filter_data(d, list())
  expect_equal(nrow(result$polizas), nrow(d$polizas))
})

# --- get_filter_choices ---
test_that("get_filter_choices returns all expected keys", {
  d <- load_data(db_path)
  d$polizas <- enrich_polizas(d$polizas)
  choices <- get_filter_choices(d)
  expect_true(all(c("estados", "tipos_vehiculo", "canales", "anios",
                     "edad_min", "edad_max", "fecha_min", "fecha_max") %in% names(choices)))
  expect_true(length(choices$estados) > 0)
  expect_true(choices$edad_min >= 18)
})
