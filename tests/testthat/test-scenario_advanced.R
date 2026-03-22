# ==============================================================================
# ADVANCED ACTUARIAL TESTS: Scenario / Monte Carlo Stress Testing
# Tests the collective risk model (compound Poisson-Gamma), VaR/TVaR ordering,
# stress multiplier properties, and Gamma MOM parameter estimation.
# ==============================================================================

library(testthat)
library(tidyverse)

# We cannot source the Shiny module directly (it depends on shiny/plotly).
# Instead, we extract and test the pure computational functions inline,
# replicating the exact logic from mod_scenario.R.

# ==============================================================================
# EXTRACTED FUNCTIONS (exact copy from mod_scenario.R)
# ==============================================================================

# Gamma MOM estimation
gamma_mom <- function(montos) {
  montos <- montos[!is.na(montos) & montos > 0]
  mu <- mean(montos)
  v <- var(montos)
  shape <- mu^2 / v
  scale <- v / mu
  list(shape = shape, scale = scale, mu = mu, v = v)
}

# calc_metrics (exact copy from the module)
calc_metrics <- function(losses) {
  sorted <- sort(losses)
  n <- length(sorted)
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

# Simulate aggregate losses (compound Poisson-Gamma)
simulate_aggregate <- function(n_sim, lambda, shape, scale, seed = 42) {
  set.seed(seed)
  vapply(seq_len(n_sim), function(i) {
    n_claims <- rpois(1, lambda)
    if (n_claims == 0) return(0)
    sum(rgamma(n_claims, shape = shape, scale = scale))
  }, numeric(1))
}

# ==============================================================================
# GAMMA MOM ESTIMATION
# ==============================================================================

test_that("Gamma MOM recovers known parameters approximately", {
  # Generate Gamma(shape=2, scale=10000) data. MOM should recover these.
  set.seed(123)
  true_shape <- 2
  true_scale <- 10000
  x <- rgamma(50000, shape = true_shape, scale = true_scale)

  params <- gamma_mom(x)

  # With 50K samples, MOM should be within 5% of true values
  expect_equal(params$shape, true_shape, tolerance = 0.05)
  expect_equal(params$scale, true_scale, tolerance = 500)
})

test_that("Gamma MOM shape and scale are positive for positive data", {
  x <- c(100, 200, 300, 400, 500)
  params <- gamma_mom(x)
  expect_true(params$shape > 0)
  expect_true(params$scale > 0)
})

test_that("Gamma MOM identity: shape * scale = mean", {
  # By definition of MOM: shape = mu^2/v, scale = v/mu
  # shape * scale = (mu^2/v) * (v/mu) = mu
  x <- c(1000, 2000, 3000, 4000, 5000, 6000)
  params <- gamma_mom(x)
  expect_equal(params$shape * params$scale, params$mu, tolerance = 1e-10)
})

test_that("Gamma MOM filters NA and non-positive values", {
  x <- c(100, NA, 200, 0, -50, 300, NA)
  params <- gamma_mom(x)
  # Should use only c(100, 200, 300)
  expect_equal(params$mu, 200)
})

test_that("Gamma MOM with identical values produces extreme shape", {
  # All values identical => variance = 0 => shape = Inf, scale = 0
  # This is a degenerate case the code should handle
  x <- rep(5000, 10)
  params <- gamma_mom(x)
  # var(x) = 0 => shape = mu^2/0 = Inf
  expect_true(is.infinite(params$shape) || params$shape > 1e10)
})

# ==============================================================================
# VaR / TVaR ORDERING PROPERTIES
# ==============================================================================

test_that("VaR is non-decreasing in confidence level", {
  # Fundamental property: VaR(alpha1) <= VaR(alpha2) when alpha1 < alpha2
  set.seed(42)
  losses <- simulate_aggregate(10000, lambda = 50, shape = 2, scale = 5000)
  m <- calc_metrics(losses)

  expect_true(m$var_95 <= m$var_99,
              info = paste("VaR95=", m$var_95, "VaR99=", m$var_99))
  expect_true(m$var_99 <= m$var_995,
              info = paste("VaR99=", m$var_99, "VaR995=", m$var_995))
})

test_that("TVaR >= VaR at the same confidence level", {
  # TVaR is the expected value given exceedance of VaR, so TVaR >= VaR always.
  set.seed(42)
  losses <- simulate_aggregate(10000, lambda = 50, shape = 2, scale = 5000)
  m <- calc_metrics(losses)

  expect_true(m$tvar_95 >= m$var_95,
              info = paste("TVaR95=", m$tvar_95, "VaR95=", m$var_95))
  expect_true(m$tvar_99 >= m$var_99,
              info = paste("TVaR99=", m$tvar_99, "VaR99=", m$var_99))
  expect_true(m$tvar_995 >= m$var_995,
              info = paste("TVaR995=", m$tvar_995, "VaR995=", m$var_995))
})

test_that("TVaR is non-decreasing in confidence level", {
  set.seed(42)
  losses <- simulate_aggregate(10000, lambda = 50, shape = 2, scale = 5000)
  m <- calc_metrics(losses)

  expect_true(m$tvar_95 <= m$tvar_99,
              info = paste("TVaR95=", m$tvar_95, "TVaR99=", m$tvar_99))
  expect_true(m$tvar_99 <= m$tvar_995,
              info = paste("TVaR99=", m$tvar_99, "TVaR995=", m$tvar_995))
})

# ==============================================================================
# COMPOUND POISSON-GAMMA: Theoretical Mean and Variance
# ==============================================================================

test_that("simulated mean converges to theoretical E[S] = lambda * shape * scale", {
  lambda <- 100
  shape <- 3
  scale <- 8000
  n_sim <- 50000

  # Theoretical mean of compound Poisson-Gamma
  theoretical_mean <- lambda * shape * scale  # = 100 * 3 * 8000 = 2,400,000

  losses <- simulate_aggregate(n_sim, lambda, shape, scale, seed = 99)
  sim_mean <- mean(losses)

  # With 50K simulations, relative error should be small (< 3%)
  rel_error <- abs(sim_mean - theoretical_mean) / theoretical_mean
  expect_true(rel_error < 0.03,
              info = paste("Relative error:", round(rel_error * 100, 2), "%",
                           "Sim mean:", round(sim_mean), "Theory:", round(theoretical_mean)))
})

test_that("simulated variance converges to theoretical Var[S]", {
  lambda <- 100
  shape <- 3
  scale <- 8000
  n_sim <- 50000

  # Var[S] = lambda * E[X^2] = lambda * shape * scale^2 * (shape + 1)
  # For Gamma: E[X^2] = Var[X] + (E[X])^2 = shape*scale^2 + (shape*scale)^2
  #          = shape*scale^2*(1 + shape)
  theoretical_var <- lambda * shape * scale^2 * (shape + 1)

  losses <- simulate_aggregate(n_sim, lambda, shape, scale, seed = 99)
  sim_var <- var(losses)

  # Variance estimation is noisier, allow 10% relative error
  rel_error <- abs(sim_var - theoretical_var) / theoretical_var
  expect_true(rel_error < 0.10,
              info = paste("Relative error:", round(rel_error * 100, 2), "%"))
})

# ==============================================================================
# STRESS MULTIPLIERS
# ==============================================================================

test_that("frequency stress multiplier of 2x approximately doubles mean loss", {
  lambda <- 50
  shape <- 2
  scale <- 10000
  n_sim <- 20000

  losses_base <- simulate_aggregate(n_sim, lambda, shape, scale, seed = 42)
  losses_2x <- simulate_aggregate(n_sim, lambda * 2, shape, scale, seed = 42)

  # Mean should approximately double (within 15% of 2x due to different draws)
  ratio <- mean(losses_2x) / mean(losses_base)
  expect_true(ratio > 1.5 && ratio < 2.5,
              info = paste("Mean ratio:", round(ratio, 3)))
})

test_that("severity stress multiplier of 2x approximately doubles mean loss", {
  lambda <- 50
  shape <- 2
  scale <- 10000
  n_sim <- 20000

  losses_base <- simulate_aggregate(n_sim, lambda, shape, scale, seed = 42)
  # Severity stress multiplies scale, keeping shape constant
  losses_2x <- simulate_aggregate(n_sim, lambda, shape, scale * 2, seed = 42)

  ratio <- mean(losses_2x) / mean(losses_base)
  expect_true(ratio > 1.5 && ratio < 2.5,
              info = paste("Mean ratio:", round(ratio, 3)))
})

test_that("stress multiplier of 1.0 gives identical results to baseline with same seed", {
  lambda <- 50
  shape <- 2
  scale <- 10000
  n_sim <- 1000

  losses_1 <- simulate_aggregate(n_sim, lambda * 1.0, shape, scale * 1.0, seed = 42)
  losses_2 <- simulate_aggregate(n_sim, lambda, shape, scale, seed = 42)

  # Same seed, same parameters => identical results
  expect_equal(losses_1, losses_2, tolerance = 0)
})

# ==============================================================================
# SIMULATION: Determinism
# ==============================================================================

test_that("simulation is deterministic with fixed seed", {
  losses_a <- simulate_aggregate(1000, 50, 2, 10000, seed = 123)
  losses_b <- simulate_aggregate(1000, 50, 2, 10000, seed = 123)
  expect_identical(losses_a, losses_b)
})

test_that("different seeds produce different results", {
  losses_a <- simulate_aggregate(1000, 50, 2, 10000, seed = 1)
  losses_b <- simulate_aggregate(1000, 50, 2, 10000, seed = 2)
  expect_false(identical(losses_a, losses_b))
})

# ==============================================================================
# EDGE CASES
# ==============================================================================

test_that("lambda = 0 produces all-zero aggregate losses", {
  losses <- simulate_aggregate(100, lambda = 0, shape = 2, scale = 10000, seed = 42)
  expect_true(all(losses == 0))
})

test_that("aggregate losses are non-negative (Gamma severity is positive)", {
  losses <- simulate_aggregate(5000, lambda = 30, shape = 2, scale = 10000, seed = 42)
  expect_true(all(losses >= 0))
})

test_that("VaR at 95% is below maximum simulated loss", {
  losses <- simulate_aggregate(10000, lambda = 50, shape = 2, scale = 5000, seed = 42)
  m <- calc_metrics(losses)
  expect_true(m$var_95 <= max(losses))
  expect_true(m$var_99 <= max(losses))
  expect_true(m$var_995 <= max(losses))
})

test_that("mean loss is between min and max simulated losses", {
  losses <- simulate_aggregate(10000, lambda = 50, shape = 2, scale = 5000, seed = 42)
  m <- calc_metrics(losses)
  expect_true(m$mean_loss >= min(losses))
  expect_true(m$mean_loss <= max(losses))
})

test_that("standard deviation is non-negative", {
  losses <- simulate_aggregate(10000, lambda = 50, shape = 2, scale = 5000, seed = 42)
  m <- calc_metrics(losses)
  expect_true(m$sd_loss >= 0)
})

# ==============================================================================
# CALC_METRICS: Small Sample Behavior
# ==============================================================================

test_that("calc_metrics works with minimum viable sample (n=1)", {
  m <- calc_metrics(c(50000))
  expect_equal(m$mean_loss, 50000)
  expect_equal(m$var_95, 50000)
  expect_equal(m$var_99, 50000)
  expect_equal(m$var_995, 50000)
  # SD of single observation is NA in R
  expect_true(is.na(m$sd_loss))
})

test_that("calc_metrics with identical losses gives zero standard deviation", {
  m <- calc_metrics(rep(100000, 100))
  expect_equal(m$mean_loss, 100000)
  expect_equal(m$sd_loss, 0)
  expect_equal(m$var_95, 100000)
  expect_equal(m$tvar_95, 100000)
})
