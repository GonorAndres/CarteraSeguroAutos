# ==============================================================================
# ADVANCED ACTUARIAL TESTS: utils_metrics.R
# Domain-driven invariant tests that go beyond the existing basic tests.
# These validate mathematical properties, edge cases, and actuarial constraints
# that a general programmer would miss.
# ==============================================================================

library(testthat)
library(tidyverse)

source(here::here("R/utils_metrics.R"), local = TRUE)

# ==============================================================================
# TEST DATA: carefully constructed to test specific actuarial properties
# ==============================================================================

# Portfolio where every policy has exactly one claim paying exactly the premium.
# Loss ratio MUST be exactly 1.0 by construction.
polizas_breakeven <- tibble(
  poliza_id  = 1:5,
  prima_neta = c(10000, 20000, 30000, 40000, 50000),
  exposicion = rep(1.0, 5)
)
siniestros_breakeven <- tibble(
  siniestro_id    = 1:5,
  poliza_id       = 1:5,
  monto_pagado    = c(10000, 20000, 30000, 40000, 50000),
  monto_siniestro = c(10000, 20000, 30000, 40000, 50000)
)

# Portfolio with varying exposures -- the core test for exposure-adjusted frequency.
polizas_partial_exp <- tibble(
  poliza_id  = 1:6,
  prima_neta = rep(12000, 6),
  exposicion = c(1.0, 0.5, 0.25, 0.75, 1.0, 0.0)
  # Total exposure = 3.5 (the zero-exposure policy contributes nothing)
)
siniestros_partial_exp <- tibble(
  siniestro_id    = 1:3,
  poliza_id       = c(1, 2, 5),
  monto_pagado    = c(5000, 3000, 8000),
  monto_siniestro = c(5000, 3000, 8000)
)

# Single-policy portfolio (minimum viable portfolio)
polizas_single <- tibble(
  poliza_id = 1L, prima_neta = 25000, exposicion = 0.5
)
siniestros_single <- tibble(
  siniestro_id = 1L, poliza_id = 1L,
  monto_pagado = 12000, monto_siniestro = 15000
)

# ==============================================================================
# LOSS RATIO: Mathematical Properties
# ==============================================================================

test_that("loss ratio equals exactly 1.0 when paid claims equal premium (breakeven portfolio)", {
  # Actuarial identity: if sum(paid) == sum(premium), LR == 1.0 exactly.
  result <- calc_loss_ratio(polizas_breakeven, siniestros_breakeven)
  expect_equal(result$loss_ratio, 1.0, tolerance = 0)
})

test_that("loss ratio is additive: total LR = sum(paid) / sum(premium) across groups", {
  # The portfolio-level LR should equal sum of group paid / sum of group premium,
  # NOT the average of group-level LRs (which would be a mean-of-ratios error).
  polizas_grouped <- tibble(
    poliza_id  = 1:4,
    prima_neta = c(100000, 100000, 10000, 10000),
    exposicion = rep(1, 4),
    grupo = c("A", "A", "B", "B")
  )
  siniestros_grouped <- tibble(
    siniestro_id    = 1:2,
    poliza_id       = c(1, 3),
    monto_pagado    = c(50000, 9000),
    monto_siniestro = c(50000, 9000)
  )

  # Total LR = (50000 + 9000) / (100000 + 100000 + 10000 + 10000) = 59000 / 220000
  result_global <- calc_loss_ratio(polizas_grouped, siniestros_grouped)
  expected_lr <- 59000 / 220000
  expect_equal(result_global$loss_ratio, expected_lr, tolerance = 1e-10)

  # Group A LR = 50000/200000 = 0.25, Group B LR = 9000/20000 = 0.45
  # Simple average would be 0.35, but correct total LR is ~0.268
  # This is NOT the same as mean of group LRs (which would be wrong).
  expect_true(abs(result_global$loss_ratio - mean(c(0.25, 0.45))) > 0.01,
              info = "LR should NOT be the simple average of group LRs")
})

test_that("loss ratio returns NA when premium is zero", {
  polizas_zero_prem <- tibble(
    poliza_id = 1L, prima_neta = 0, exposicion = 1.0
  )
  sin_zero <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_pagado = 5000, monto_siniestro = 5000
  )
  result <- calc_loss_ratio(polizas_zero_prem, sin_zero)
  # Division by zero premium must yield NA, not Inf
  expect_true(is.na(result$loss_ratio))
})

test_that("loss ratio uses monto_pagado not monto_siniestro (paid vs incurred)", {
  # Actuarially, this module calculates a PAID loss ratio.
  # monto_pagado < monto_siniestro when there is a deductible.
  polizas_test <- tibble(
    poliza_id = 1L, prima_neta = 100000, exposicion = 1.0
  )
  sin_test <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_pagado = 60000, monto_siniestro = 80000
  )
  result <- calc_loss_ratio(polizas_test, sin_test)
  # Should use monto_pagado (60000), not monto_siniestro (80000)
  expect_equal(result$loss_ratio, 0.6, tolerance = 1e-10)
  expect_true(result$loss_ratio != 0.8)
})

test_that("loss ratio is non-negative when both premium and paid are non-negative", {
  # Under standard auto insurance, both premium and paid claims are >= 0.
  # This guarantees LR >= 0.
  polizas_t <- tibble(
    poliza_id = 1:3, prima_neta = c(1000, 2000, 3000), exposicion = rep(1, 3)
  )
  sin_t <- tibble(
    siniestro_id = 1:2, poliza_id = c(1, 3),
    monto_pagado = c(500, 1500), monto_siniestro = c(500, 1500)
  )
  result <- calc_loss_ratio(polizas_t, sin_t)
  expect_true(result$loss_ratio >= 0)
})

# ==============================================================================
# FREQUENCY: Exposure-Adjusted Denominator
# ==============================================================================

test_that("frequency uses exposure sum, not policy count, as denominator", {
  # 3 claims across 3.5 exposure-years = 0.857... claims per exposure-year
  # If using policy count (6), would get 0.5 -- a materially different answer.
  result <- calc_frequency(polizas_partial_exp, siniestros_partial_exp)

  # Hand calculation: 3 claims / (1.0 + 0.5 + 0.25 + 0.75 + 1.0 + 0.0) = 3/3.5
  expected_freq <- 3 / 3.5
  expect_equal(result$frecuencia, expected_freq, tolerance = 1e-10)

  # Verify it is NOT using policy count
  wrong_freq <- 3 / 6  # = 0.5
  expect_true(abs(result$frecuencia - wrong_freq) > 0.1)
})

test_that("frequency can exceed 1.0 (multiple claims per policy-year)", {
  # A single policy with 3 claims in one year: frequency = 3.0
  pol <- tibble(poliza_id = 1L, prima_neta = 10000, exposicion = 1.0)
  sin <- tibble(
    siniestro_id = 1:3, poliza_id = rep(1L, 3),
    monto_pagado = c(1000, 2000, 3000), monto_siniestro = c(1000, 2000, 3000)
  )
  result <- calc_frequency(pol, sin)
  expect_equal(result$frecuencia, 3.0)
})

test_that("frequency returns NA when total exposure is zero, not Inf", {
  # All policies have zero exposure (e.g., cancelled on day 1)
  pol_zero <- tibble(
    poliza_id = 1:2, prima_neta = c(5000, 5000), exposicion = c(0, 0)
  )
  sin_zero <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_pagado = 1000, monto_siniestro = 1000
  )
  result <- calc_frequency(pol_zero, sin_zero)
  # Must be NA, not Inf or NaN
  expect_true(is.na(result$frecuencia))
  expect_false(is.infinite(result$frecuencia %||% 0))
})

test_that("frequency with half-year exposure gives double the raw rate", {
  # 1 claim on a policy with 0.5 years exposure = frequency 2.0 (annualized)
  pol <- tibble(poliza_id = 1L, prima_neta = 10000, exposicion = 0.5)
  sin <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_pagado = 5000, monto_siniestro = 5000
  )
  result <- calc_frequency(pol, sin)
  expect_equal(result$frecuencia, 2.0)
})

test_that("frequency is zero when there are no claims", {
  pol <- tibble(poliza_id = 1:3, prima_neta = rep(10000, 3), exposicion = rep(1, 3))
  sin <- tibble(siniestro_id = integer(), poliza_id = integer())
  result <- calc_frequency(pol, sin)
  expect_equal(result$frecuencia, 0)
})

test_that("grouped frequency sums exposure within each group, not across groups", {
  pol <- tibble(
    poliza_id = 1:4,
    prima_neta = rep(10000, 4),
    exposicion = c(1.0, 0.5, 1.0, 0.5),
    grupo = c("X", "X", "Y", "Y")
  )
  sin <- tibble(
    siniestro_id = 1:2, poliza_id = c(1, 3),
    monto_pagado = c(5000, 5000), monto_siniestro = c(5000, 5000)
  )
  result <- calc_frequency(pol, sin, grupo)

  # Group X: 1 claim / 1.5 exposure = 0.667
  # Group Y: 1 claim / 1.5 exposure = 0.667
  x_row <- result %>% filter(grupo == "X")
  y_row <- result %>% filter(grupo == "Y")
  expect_equal(x_row$frecuencia, 1 / 1.5, tolerance = 1e-10)
  expect_equal(y_row$frecuencia, 1 / 1.5, tolerance = 1e-10)
})

# ==============================================================================
# SEVERITY: Statistical Properties
# ==============================================================================

test_that("severity statistics are consistent with manual calculation", {
  sin <- tibble(
    siniestro_id = 1:5, poliza_id = 1:5,
    monto_siniestro = c(10000, 20000, 30000, 40000, 50000),
    monto_pagado = c(8000, 16000, 24000, 32000, 40000)
  )
  result <- calc_severity(sin)
  # Mean = (10+20+30+40+50)/5 * 1000 = 30000
  expect_equal(result$severidad_media, 30000)
  # Median = 30000
  expect_equal(result$severidad_mediana, 30000)
  # SD = sd(c(10000,20000,30000,40000,50000))
  expect_equal(result$severidad_sd, sd(c(10000, 20000, 30000, 40000, 50000)),
               tolerance = 1e-6)
  # Min and max
  expect_equal(result$severidad_min, 10000)
  expect_equal(result$severidad_max, 50000)
})

test_that("severity uses monto_siniestro (incurred) not monto_pagado", {
  sin <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_siniestro = 50000, monto_pagado = 30000
  )
  result <- calc_severity(sin)
  expect_equal(result$severidad_media, 50000)
  expect_true(result$severidad_media != 30000)
})

test_that("severity with single observation has NA standard deviation", {
  sin <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_siniestro = 25000, monto_pagado = 20000
  )
  result <- calc_severity(sin)
  expect_true(is.na(result$severidad_sd))
  expect_equal(result$severidad_media, 25000)
})

# ==============================================================================
# KPIs: Internal Consistency
# ==============================================================================

test_that("KPI loss ratio equals siniestros_total / prima_total (identity check)", {
  pol <- tibble(
    poliza_id = 1:3, prima_neta = c(10000, 20000, 30000),
    exposicion = c(1.0, 0.8, 0.6),
    suma_asegurada = c(100000, 200000, 300000)
  )
  sin <- tibble(
    siniestro_id = 1:2, poliza_id = c(1, 3),
    monto_pagado = c(5000, 15000), monto_siniestro = c(7000, 18000)
  )
  kpis <- calc_kpis(pol, sin)

  # Identity: LR = paid / premium
  expect_equal(kpis$loss_ratio, kpis$siniestros_total / kpis$prima_total,
               tolerance = 1e-15)

  # Frequency uses exposure
  expect_equal(kpis$frecuencia, kpis$n_siniestros / sum(pol$exposicion),
               tolerance = 1e-15)

  # Pure premium identity: freq * sev approx= LR * avg_premium
  # (only approximate due to exposure weighting)
  pure_premium <- kpis$frecuencia * kpis$severidad_media
  lr_times_avg_prem <- kpis$loss_ratio * (kpis$prima_total / sum(pol$exposicion))
  # These are NOT necessarily equal because freq uses exposure and LR uses premium
  # But both should be positive
  expect_true(pure_premium > 0)
  expect_true(lr_times_avg_prem > 0)
})

test_that("KPI frequency uses exposure, not policy count", {
  pol <- tibble(
    poliza_id = 1:4, prima_neta = rep(10000, 4),
    exposicion = c(0.5, 0.5, 0.5, 0.5),  # total = 2.0
    suma_asegurada = rep(100000, 4)
  )
  sin <- tibble(
    siniestro_id = 1:2, poliza_id = c(1, 2),
    monto_pagado = c(5000, 5000), monto_siniestro = c(5000, 5000)
  )
  kpis <- calc_kpis(pol, sin)
  # 2 claims / 2.0 exposure = 1.0
  expect_equal(kpis$frecuencia, 1.0)
  # Not 2/4 = 0.5 (policy count based)
  expect_true(kpis$frecuencia != 0.5)
})

# ==============================================================================
# KPI DELTA: Percentage Change Properties
# ==============================================================================

test_that("kpi delta is zero when current equals previous", {
  kpis_same <- list(
    n_polizas = 100, n_siniestros = 10, prima_total = 500000,
    siniestros_total = 350000, loss_ratio = 0.70,
    frecuencia = 0.10, severidad_media = 35000
  )
  deltas <- calc_kpis_delta(kpis_same, kpis_same)
  for (nm in names(deltas)) {
    expect_equal(deltas[[nm]], 0, tolerance = 1e-15,
                 info = paste("Delta should be 0 for", nm))
  }
})

test_that("kpi delta is exactly 1.0 when value doubles", {
  kpis_prev <- list(
    n_polizas = 100, n_siniestros = 10, prima_total = 500000,
    siniestros_total = 350000, loss_ratio = 0.70,
    frecuencia = 0.10, severidad_media = 35000
  )
  kpis_curr <- list(
    n_polizas = 200, n_siniestros = 20, prima_total = 1000000,
    siniestros_total = 700000, loss_ratio = 1.40,
    frecuencia = 0.20, severidad_media = 70000
  )
  deltas <- calc_kpis_delta(kpis_curr, kpis_prev)
  for (nm in names(deltas)) {
    expect_equal(deltas[[nm]], 1.0, tolerance = 1e-10,
                 info = paste("Delta should be 1.0 (doubled) for", nm))
  }
})

test_that("kpi delta is -0.5 when value halves", {
  kpis_prev <- list(
    n_polizas = 200, n_siniestros = 20, prima_total = 1000000,
    siniestros_total = 700000, loss_ratio = 0.70,
    frecuencia = 0.20, severidad_media = 70000
  )
  kpis_curr <- list(
    n_polizas = 100, n_siniestros = 10, prima_total = 500000,
    siniestros_total = 350000, loss_ratio = 0.35,
    frecuencia = 0.10, severidad_media = 35000
  )
  deltas <- calc_kpis_delta(kpis_curr, kpis_prev)
  for (nm in names(deltas)) {
    expect_equal(deltas[[nm]], -0.5, tolerance = 1e-10,
                 info = paste("Delta should be -0.5 (halved) for", nm))
  }
})

test_that("kpi delta returns NA for all metrics when previous is all zeros", {
  kpis_zero <- list(
    n_polizas = 0, n_siniestros = 0, prima_total = 0,
    siniestros_total = 0, loss_ratio = 0, frecuencia = 0,
    severidad_media = 0
  )
  kpis_curr <- list(
    n_polizas = 100, n_siniestros = 10, prima_total = 500000,
    siniestros_total = 350000, loss_ratio = 0.70,
    frecuencia = 0.10, severidad_media = 35000
  )
  deltas <- calc_kpis_delta(kpis_curr, kpis_zero)
  for (nm in names(deltas)) {
    expect_true(is.na(deltas[[nm]]),
                info = paste("Delta should be NA when prev=0 for", nm))
  }
})

test_that("kpi delta handles missing metrics gracefully", {
  kpis_prev <- list(n_polizas = 100)
  kpis_curr <- list(n_polizas = 110)
  deltas <- calc_kpis_delta(kpis_curr, kpis_prev)
  # Only n_polizas should compute; others should be NA (NULL values)
  expect_equal(deltas$delta_n_polizas, 0.10, tolerance = 1e-10)
  expect_true(is.na(deltas$delta_loss_ratio))
})

# ==============================================================================
# STRESS TEST: Large and Small Numbers
# ==============================================================================

test_that("loss ratio handles very large premium and claim amounts", {
  pol <- tibble(poliza_id = 1L, prima_neta = 1e12, exposicion = 1.0)
  sin <- tibble(
    siniestro_id = 1L, poliza_id = 1L,
    monto_pagado = 5e11, monto_siniestro = 5e11
  )
  result <- calc_loss_ratio(pol, sin)
  expect_equal(result$loss_ratio, 0.5, tolerance = 1e-10)
})

test_that("frequency handles very small exposures without overflow", {
  pol <- tibble(poliza_id = 1L, prima_neta = 100, exposicion = 1e-6)
  sin <- tibble(siniestro_id = 1L, poliza_id = 1L)
  result <- calc_frequency(pol, sin)
  # 1 claim / 1e-6 exposure = 1e6
  expect_equal(result$frecuencia, 1e6, tolerance = 1)
  expect_true(is.finite(result$frecuencia))
})
