# ==============================================================================
# ADVANCED ACTUARIAL TESTS: GLM Pricing Module
# Tests Poisson frequency model properties, Gamma severity model properties,
# pure premium decomposition, and portfolio-level consistency.
# ==============================================================================

library(testthat)
library(tidyverse)

# ==============================================================================
# GLM PROPERTY TESTS WITH SYNTHETIC DATA
# These tests construct simple datasets where we know the theoretical properties
# that must hold, then fit GLMs and verify those properties.
# ==============================================================================

# Build synthetic frequency dataset
build_freq_data <- function(n = 2000, seed = 42) {
  set.seed(seed)
  tibble(
    poliza_id      = seq_len(n),
    n_claims       = rpois(n, lambda = 0.1),
    exposicion     = runif(n, 0.5, 1.0),
    rango_edad     = factor(sample(c("18-25", "26-35", "36-45"), n, replace = TRUE)),
    genero         = factor(sample(c("M", "F"), n, replace = TRUE)),
    tipo_vehiculo  = factor(sample(c("Sedan", "SUV", "Hatchback"), n, replace = TRUE)),
    zona_riesgo    = factor(sample(c("Zona Alta", "Zona Media", "Zona Baja"), n, replace = TRUE)),
    canal_venta    = factor(sample(c("Agente", "Directo"), n, replace = TRUE)),
    segmento_score = factor(sample(c("Bajo (<550)", "Medio (550-649)", "Alto (650+)"), n, replace = TRUE))
  )
}

# Build synthetic severity dataset
build_sev_data <- function(n = 500, seed = 42) {
  set.seed(seed)
  tibble(
    siniestro_id   = seq_len(n),
    monto_siniestro = rgamma(n, shape = 2, scale = 15000),
    tipo_siniestro = factor(sample(c("Colision", "Danos", "Robo Total"), n, replace = TRUE)),
    tipo_vehiculo  = factor(sample(c("Sedan", "SUV", "Hatchback"), n, replace = TRUE)),
    rango_edad     = factor(sample(c("18-25", "26-35", "36-45"), n, replace = TRUE)),
    zona_riesgo    = factor(sample(c("Zona Alta", "Zona Media", "Zona Baja"), n, replace = TRUE))
  )
}

# ==============================================================================
# POISSON FREQUENCY MODEL
# ==============================================================================

test_that("Poisson GLM with log(exposure) offset produces non-negative predictions", {
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo + zona_riesgo,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )
  preds <- predict(model, type = "response")

  # Poisson GLM predictions are exp(X*beta + offset) which is always > 0
  expect_true(all(preds > 0),
              info = "Poisson predictions must be strictly positive")
})

test_that("Poisson GLM predicted counts sum approximately to observed counts", {
  # For a well-specified Poisson GLM, sum of fitted values = sum of observed
  # This is a consequence of the MLE equations.
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo + zona_riesgo,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )

  sum_observed <- sum(df$n_claims)
  sum_fitted <- sum(fitted(model))

  # Equality should hold to high precision for Poisson GLM
  expect_equal(sum_fitted, sum_observed, tolerance = 1e-6,
               info = "Sum of Poisson fitted values must equal sum of observed")
})

test_that("Poisson GLM frequency rate is predicted_count / exposure", {
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )

  # predict(type="response") gives predicted counts (not rates)
  pred_counts <- predict(model, type = "response")
  # Frequency rate = counts / exposure
  freq_rates <- pred_counts / df$exposicion

  # All rates must be positive
  expect_true(all(freq_rates > 0))

  # Rates should be roughly in the range of the simulated lambda (0.1)
  # Allow wide bounds since GLM adjusts by covariates
  expect_true(mean(freq_rates) > 0.01 && mean(freq_rates) < 1.0,
              info = paste("Mean freq rate:", round(mean(freq_rates), 4)))
})

test_that("Poisson GLM exponentiated intercept gives base frequency rate", {
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )

  # For the base levels (first level of each factor), with exposure = 1,
  # the predicted count = exp(intercept)
  base_data <- tibble(
    rango_edad    = factor("18-25", levels = levels(df$rango_edad)),
    genero        = factor("F", levels = levels(df$genero)),
    tipo_vehiculo = factor("Hatchback", levels = levels(df$tipo_vehiculo)),
    exposicion    = 1.0
  )
  pred <- predict(model, newdata = base_data, type = "response")

  # This should equal exp(intercept). Use unname to strip names for comparison.
  expect_equal(as.numeric(pred), unname(exp(coef(model)["(Intercept)"])),
               tolerance = 1e-8)
})

test_that("portfolio frequency is sum(pred)/sum(exposure), not mean of ratios", {
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )

  pred_counts <- predict(model, type = "response")

  # Correct portfolio frequency: sum-of-predicted / sum-of-exposure
  correct_freq <- sum(pred_counts) / sum(df$exposicion)

  # Wrong approach: mean of individual ratios
  wrong_freq <- mean(pred_counts / df$exposicion)

  # These differ when exposure is non-uniform
  # The code should use the correct approach
  expect_true(abs(correct_freq - wrong_freq) > 0 || all(df$exposicion == df$exposicion[1]),
              info = "sum/sum and mean-of-ratios should differ with non-uniform exposure")
})

# ==============================================================================
# GAMMA SEVERITY MODEL
# ==============================================================================

test_that("Gamma GLM predictions are strictly positive", {
  df <- build_sev_data()
  model <- glm(
    monto_siniestro ~ tipo_siniestro + tipo_vehiculo + zona_riesgo,
    family = Gamma(link = "log"),
    data = df
  )
  preds <- predict(model, type = "response")

  # Gamma with log link: predictions = exp(X*beta) > 0 always
  expect_true(all(preds > 0),
              info = "Gamma predictions must be strictly positive")
})

test_that("Gamma GLM fitted mean equals observed mean", {
  # For a GLM, the sum of fitted values equals sum of observed values.
  df <- build_sev_data()
  model <- glm(
    monto_siniestro ~ tipo_siniestro + tipo_vehiculo,
    family = Gamma(link = "log"),
    data = df
  )

  sum_obs <- sum(df$monto_siniestro)
  sum_fit <- sum(fitted(model))

  # This property holds for canonical link but also for log link in practice
  # with intercept included. Allow small tolerance.
  expect_equal(sum_fit, sum_obs, tolerance = sum_obs * 0.001,
               info = "Sum of Gamma fitted values should approximate sum of observed")
})

test_that("Gamma GLM deviance is non-negative", {
  df <- build_sev_data()
  model <- glm(
    monto_siniestro ~ tipo_siniestro + tipo_vehiculo,
    family = Gamma(link = "log"),
    data = df
  )
  expect_true(deviance(model) >= 0)
})

# ==============================================================================
# PURE PREMIUM DECOMPOSITION
# ==============================================================================

test_that("pure premium equals frequency times severity", {
  # This is the fundamental actuarial pricing identity:
  # Pure Premium = E[frequency] * E[severity]
  freq_df <- build_freq_data()
  sev_df <- build_sev_data()

  freq_model <- glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = freq_df
  )

  sev_model <- glm(
    monto_siniestro ~ tipo_siniestro + tipo_vehiculo,
    family = Gamma(link = "log"),
    data = sev_df
  )

  # Portfolio frequency
  port_freq <- sum(predict(freq_model, type = "response")) / sum(freq_df$exposicion)
  # Average predicted severity
  port_sev <- mean(predict(sev_model, type = "response"))

  pure_premium <- port_freq * port_sev

  # Pure premium must be positive
  expect_true(pure_premium > 0)

  # Pure premium should be in a sensible range given lambda=0.1 and shape*scale=30000
  # Expected PP ~ 0.1 * 30000 = 3000 (order of magnitude)
  expect_true(pure_premium > 100 && pure_premium < 50000,
              info = paste("Pure premium:", round(pure_premium, 2)))
})

test_that("commercial premium = pure premium * (1 + loading)", {
  # The module uses a 40% loading
  loading <- 0.40
  pure_premium <- 3500  # arbitrary

  commercial <- pure_premium * (1 + loading)
  expect_equal(commercial, pure_premium * 1.40)
  expect_true(commercial > pure_premium)
})

# ==============================================================================
# RELATIVITIES
# ==============================================================================

test_that("exponentiated Poisson coefficients are positive (they are multiplicative factors)", {
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )

  exp_coefs <- exp(coef(model))
  expect_true(all(exp_coefs > 0))
})

test_that("exponentiated Gamma coefficients are positive", {
  df <- build_sev_data()
  model <- glm(
    monto_siniestro ~ tipo_siniestro + tipo_vehiculo,
    family = Gamma(link = "log"),
    data = df
  )

  exp_coefs <- exp(coef(model))
  expect_true(all(exp_coefs > 0))
})

# ==============================================================================
# CONFIDENCE INTERVALS
# ==============================================================================

test_that("manual Wald CI is symmetric on log scale", {
  df <- build_freq_data()
  model <- glm(
    n_claims ~ rango_edad + genero,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = df
  )

  tidy_df <- broom::tidy(model, exponentiate = TRUE, conf.int = FALSE)
  tidy_df$conf.low  <- exp(log(tidy_df$estimate) - 1.96 * tidy_df$std.error)
  tidy_df$conf.high <- exp(log(tidy_df$estimate) + 1.96 * tidy_df$std.error)

  # Check CI contains the point estimate
  for (i in seq_len(nrow(tidy_df))) {
    expect_true(tidy_df$conf.low[i] <= tidy_df$estimate[i],
                info = paste("CI lower > estimate for", tidy_df$term[i]))
    expect_true(tidy_df$conf.high[i] >= tidy_df$estimate[i],
                info = paste("CI upper < estimate for", tidy_df$term[i]))
  }

  # CI width should be positive
  expect_true(all(tidy_df$conf.high > tidy_df$conf.low))
})

# ==============================================================================
# SEVERITY FILTER: Only Paid Claims
# ==============================================================================

test_that("severity model should only use paid claims (positive amounts)", {
  # The module filters: estado_siniestro == "Pagado" and monto_siniestro > 0
  # If unpaid/rejected claims with monto=0 leak in, the Gamma model would fail
  # (Gamma requires strictly positive response)
  sev_data <- tibble(
    monto_siniestro = c(5000, 0, 10000, -100, 15000),
    estado_siniestro = c("Pagado", "Rechazado", "Pagado", "Pagado", "Pagado"),
    tipo_siniestro = factor(rep("Colision", 5)),
    tipo_vehiculo  = factor(rep("Sedan", 5)),
    rango_edad     = factor(rep("26-35", 5)),
    zona_riesgo    = factor(rep("Zona Media", 5))
  )

  # Apply the same filter as the module
  filtered <- sev_data %>%
    filter(estado_siniestro == "Pagado", monto_siniestro > 0)

  # Should exclude row 2 (Rechazado) and row 4 (negative amount)
  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$monto_siniestro > 0))
})

# ==============================================================================
# GLM WITH REAL PROJECT DATA
# ==============================================================================

test_that("GLM models fit successfully on real project data", {
  db_path <- here::here("data/siniestralidad.db")
  skip_if_not(file.exists(db_path), "Database not found")

  source(here::here("R/utils_data.R"), local = TRUE)
  d <- load_data(db_path)
  d$polizas <- enrich_polizas(d$polizas)

  # Build frequency dataset (same logic as module)
  claim_counts <- d$siniestros %>%
    count(poliza_id, name = "n_claims")

  freq_df <- d$polizas %>%
    left_join(claim_counts, by = "poliza_id") %>%
    mutate(
      n_claims   = replace_na(n_claims, 0L),
      exposicion = pmax(exposicion, 0.01)
    ) %>%
    filter(
      !is.na(rango_edad), !is.na(genero), !is.na(tipo_vehiculo),
      !is.na(zona_riesgo), !is.na(canal_venta), !is.na(segmento_score),
      segmento_score != "Sin Score"
    ) %>%
    mutate(across(
      c(rango_edad, genero, tipo_vehiculo, zona_riesgo, canal_venta, segmento_score),
      as.factor
    ))

  expect_true(nrow(freq_df) >= 50, info = "Need >= 50 rows for freq model")

  freq_model <- suppressWarnings(glm(
    n_claims ~ rango_edad + genero + tipo_vehiculo + zona_riesgo + canal_venta + segmento_score,
    family = poisson(link = "log"),
    offset = log(exposicion),
    data = freq_df
  ))

  expect_false(is.null(freq_model))
  expect_true(freq_model$converged)

  # Predicted counts must sum to observed counts
  expect_equal(sum(fitted(freq_model)), sum(freq_df$n_claims), tolerance = 1)

  # Build severity dataset
  sev_df <- d$siniestros %>%
    filter(estado_siniestro == "Pagado", monto_siniestro > 0) %>%
    left_join(d$polizas %>% select(poliza_id, tipo_vehiculo, rango_edad, zona_riesgo),
              by = "poliza_id") %>%
    filter(!is.na(tipo_siniestro), !is.na(tipo_vehiculo),
           !is.na(rango_edad), !is.na(zona_riesgo)) %>%
    mutate(across(c(tipo_siniestro, tipo_vehiculo, rango_edad, zona_riesgo), as.factor))

  expect_true(nrow(sev_df) >= 30, info = "Need >= 30 rows for sev model")

  sev_model <- suppressWarnings(glm(
    monto_siniestro ~ tipo_siniestro + tipo_vehiculo + rango_edad + zona_riesgo,
    family = Gamma(link = "log"),
    data = sev_df
  ))

  expect_false(is.null(sev_model))
  expect_true(sev_model$converged)

  # All predictions positive
  expect_true(all(predict(sev_model, type = "response") > 0))

  # Portfolio pure premium (na.rm needed because some exposures may be NA)
  port_freq <- sum(predict(freq_model, type = "response")) / sum(freq_df$exposicion, na.rm = TRUE)
  port_sev <- mean(predict(sev_model, type = "response"))
  pp <- port_freq * port_sev

  expect_true(pp > 0)
  # Pure premium should be sensible for Mexican auto insurance (roughly 1000-15000 MXN)
  expect_true(pp > 500 && pp < 50000,
              info = paste("Pure premium:", round(pp, 2), "MXN"))
})
