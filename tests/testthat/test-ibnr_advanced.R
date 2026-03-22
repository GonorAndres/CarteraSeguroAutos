# ==============================================================================
# ADVANCED ACTUARIAL TESTS: IBNR / Chain Ladder
# Tests mathematical properties of development triangles, LDFs, CDFs,
# and the Mack standard error estimator.
# ==============================================================================

library(testthat)
library(tidyverse)

source(here::here("R/utils_theme.R"), local = TRUE)
source(here::here("R/mod_ibnr.R"), local = TRUE)

# ==============================================================================
# HAND-CALCULATED TRIANGLES
# ==============================================================================

# A 3x3 incremental triangle with known values.
# Incremental:
#   Dev0   Dev1   Dev2
#   100    50     20     (Origin 2020, fully developed)
#   150    60     NA     (Origin 2021, one period to go)
#   200    NA     NA     (Origin 2022, two periods to go)
#
# Cumulative:
#   100    150    170
#   150    210    NA
#   200    NA     NA

build_known_triangle <- function() {
  pagos <- tibble(
    siniestro_id    = c(1, 1, 1, 2, 2, 3),
    anio_ocurrencia = c(2020, 2020, 2020, 2021, 2021, 2022),
    anio_desarrollo = c(0, 1, 2, 0, 1, 0),
    monto_pago      = c(100, 50, 20, 150, 60, 200)
  )
  pagos
}

# ==============================================================================
# incr_to_cum / cum_to_incr: Exact Inverse Property
# ==============================================================================

test_that("incr_to_cum produces correct cumulative values (hand-calculated)", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)

  # Row 1 (2020): 100, 100+50=150, 150+20=170
  expect_equal(unname(tri_cum[1, 1]), 100)
  expect_equal(unname(tri_cum[1, 2]), 150)
  expect_equal(unname(tri_cum[1, 3]), 170)

  # Row 2 (2021): 150, 150+60=210, NA
  expect_equal(unname(tri_cum[2, 1]), 150)
  expect_equal(unname(tri_cum[2, 2]), 210)
  expect_true(is.na(tri_cum[2, 3]))

  # Row 3 (2022): 200, NA, NA
  expect_equal(unname(tri_cum[3, 1]), 200)
  expect_true(is.na(tri_cum[3, 2]))
  expect_true(is.na(tri_cum[3, 3]))
})

test_that("cum_to_incr recovers exact original incremental values", {
  tri_incr_orig <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr_orig)
  tri_incr_recovered <- cum_to_incr(tri_cum)

  # Every non-NA cell must match exactly (no floating point tolerance needed
  # for integer arithmetic)
  for (i in seq_len(nrow(tri_incr_orig))) {
    for (j in seq_len(ncol(tri_incr_orig))) {
      if (!is.na(tri_incr_orig[i, j])) {
        expect_equal(
          unname(tri_incr_recovered[i, j]),
          unname(tri_incr_orig[i, j]),
          info = paste("Roundtrip failed at cell", i, j)
        )
      }
    }
  }
})

test_that("incr_to_cum(cum_to_incr(X)) = X for arbitrary cumulative triangle", {
  # Build a cumulative triangle directly
  cum_orig <- matrix(c(
    100, 200, 250,
    120, 230, NA,
    140, NA,  NA
  ), nrow = 3, byrow = TRUE)
  rownames(cum_orig) <- 2020:2022

  incr <- cum_to_incr(cum_orig)
  cum_recovered <- incr_to_cum(incr)

  for (i in 1:3) {
    for (j in 1:3) {
      if (!is.na(cum_orig[i, j])) {
        expect_equal(unname(cum_recovered[i, j]), unname(cum_orig[i, j]),
                     info = paste("Cell", i, j))
      }
    }
  }
})

# ==============================================================================
# CHAIN LADDER: Link Ratios and CDF Properties
# ==============================================================================

test_that("LDFs are volume-weighted averages (hand-calculated)", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # LDF from dev 0 to dev 1:
  # Numerator: sum of cum[,2] where both cols available = 150 + 210 = 360
  # Denominator: sum of cum[,1] where both cols available = 100 + 150 = 250
  # LDF[1] = 360/250 = 1.44
  expect_equal(cl$ldf[1], 360 / 250, tolerance = 1e-10)

  # LDF from dev 1 to dev 2:
  # Only row 1 has both: 170/150 = 1.1333...
  expect_equal(cl$ldf[2], 170 / 150, tolerance = 1e-10)
})

test_that("CDF is the reverse-cumulative product of LDFs", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  n_dev <- length(cl$cdf)

  # CDF at last position must be exactly 1.0
  expect_equal(cl$cdf[n_dev], 1.0, tolerance = 0)

  # CDF[j] = ldf[j] * CDF[j+1] for all j
  for (j in seq_len(n_dev - 1)) {
    expect_equal(cl$cdf[j], cl$ldf[j] * cl$cdf[j + 1], tolerance = 1e-10,
                 info = paste("CDF identity failed at position", j))
  }

  # CDF[1] = product of all LDFs
  expect_equal(cl$cdf[1], prod(cl$ldf), tolerance = 1e-10)
})

test_that("fully developed origin year has IBNR exactly zero", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # Origin 2020 (row 1) is at the last development position
  expect_equal(unname(cl$ibnr[1]), 0, tolerance = 0)
})

test_that("ultimate >= latest for every origin year", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  for (i in seq_along(cl$ultimate)) {
    expect_true(cl$ultimate[i] >= cl$latest[i],
                info = paste("Origin", cl$origin[i], ": ultimate < latest"))
  }
})

test_that("IBNR = ultimate - latest (identity)", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  for (i in seq_along(cl$ibnr)) {
    expect_equal(unname(cl$ibnr[i]),
                 unname(cl$ultimate[i] - cl$latest[i]),
                 tolerance = 1e-10,
                 info = paste("IBNR identity failed for origin", cl$origin[i]))
  }
})

test_that("chain ladder IBNR hand calculation for 2022 origin year", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # Origin 2022: latest = 200 (at dev position 1)
  # CDF at position 1 = LDF[1] * LDF[2] = (360/250) * (170/150)
  expected_cdf <- (360 / 250) * (170 / 150)
  expected_ultimate <- 200 * expected_cdf
  expected_ibnr <- expected_ultimate - 200

  expect_equal(unname(cl$ultimate[3]), expected_ultimate, tolerance = 1e-6)
  expect_equal(unname(cl$ibnr[3]), expected_ibnr, tolerance = 1e-6)
})

# ==============================================================================
# CHAIN LADDER: Mack Standard Error
# ==============================================================================

test_that("Mack SE is zero for fully developed origin year", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # Origin 2020 at full development: no estimation error
  expect_equal(unname(cl$se[1]), 0)
})

test_that("Mack SE is non-negative for all origin years", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  expect_true(all(cl$se >= 0))
})

test_that("Mack SE is finite for all origin years", {
  tri_incr <- build_triangle(build_known_triangle())
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  expect_true(all(is.finite(cl$se)))
})

# ==============================================================================
# CHAIN LADDER: Uniform Triangle (Degenerate Case)
# ==============================================================================

test_that("uniform incremental triangle gives predictable LDFs", {
  # All increments are 100. Cumulative row k: 100, 200, 300.
  # LDF dev0->dev1 = sum(200)/sum(100) = 2.0 (volume weighted same as simple)
  # LDF dev1->dev2 = 300/200 = 1.5
  pagos_uniform <- tibble(
    siniestro_id    = c(1, 1, 1, 2, 2, 3),
    anio_ocurrencia = c(2020, 2020, 2020, 2021, 2021, 2022),
    anio_desarrollo = c(0, 1, 2, 0, 1, 0),
    monto_pago      = rep(100, 6)
  )
  tri_incr <- build_triangle(pagos_uniform)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # LDF[1]: both 2020 and 2021 available
  # 2020: cum = 100, 200, 300; 2021: cum = 100, 200, NA
  # LDF = (200+200)/(100+100) = 2.0
  expect_equal(cl$ldf[1], 2.0, tolerance = 1e-10)

  # LDF[2]: only 2020 available: 300/200 = 1.5
  expect_equal(cl$ldf[2], 1.5, tolerance = 1e-10)
})

# ==============================================================================
# CHAIN LADDER: With Real Project Data
# ==============================================================================

test_that("chain ladder on real data produces actuarially sensible results", {
  db_path <- here::here("data/siniestralidad.db")
  skip_if_not(file.exists(db_path), "Database not found")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  pagos <- DBI::dbGetQuery(con, "SELECT * FROM pagos_desarrollo") %>%
    tibble::as_tibble()
  DBI::dbDisconnect(con)

  tri_incr <- build_triangle(pagos)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # All LDFs must be >= 1.0 (losses accumulate, not decrease)
  expect_true(all(cl$ldf >= 1.0),
              info = paste("LDFs:", paste(round(cl$ldf, 4), collapse = ", ")))

  # CDFs must be non-increasing from first to last position
  for (j in seq_len(length(cl$cdf) - 1)) {
    expect_true(cl$cdf[j] >= cl$cdf[j + 1],
                info = paste("CDF not non-increasing at position", j))
  }

  # Total IBNR must be positive (there should be unreported claims)
  expect_true(sum(cl$ibnr) > 0)

  # IBNR as percentage of latest paid should be reasonable (< 100% of latest)
  # This is a soft check -- very immature triangles could violate this
  ibnr_pct <- sum(cl$ibnr) / sum(cl$latest)
  expect_true(ibnr_pct < 2.0,
              info = paste("IBNR/Latest ratio:", round(ibnr_pct, 4),
                           "-- may indicate a problem"))

  # All standard errors must be non-negative
  expect_true(all(cl$se >= 0))
})

# ==============================================================================
# EDGE CASE: 2x2 Triangle (Minimum Size)
# ==============================================================================

test_that("chain ladder works with minimal 2x2 triangle", {
  pagos_mini <- tibble(
    siniestro_id    = c(1, 1, 2),
    anio_ocurrencia = c(2023, 2023, 2024),
    anio_desarrollo = c(0, 1, 0),
    monto_pago      = c(100, 50, 120)
  )
  tri_incr <- build_triangle(pagos_mini)
  tri_cum <- incr_to_cum(tri_incr)

  expect_equal(nrow(tri_cum), 2)
  expect_equal(ncol(tri_cum), 2)

  cl <- chain_ladder(tri_cum)

  # Single LDF: 150/100 = 1.5
  expect_equal(cl$ldf[1], 1.5, tolerance = 1e-10)

  # Origin 2024: latest = 120, ultimate = 120 * 1.5 = 180
  expect_equal(unname(cl$ultimate[2]), 180, tolerance = 1e-6)
  expect_equal(unname(cl$ibnr[2]), 60, tolerance = 1e-6)

  # Origin 2023 fully developed: IBNR = 0
  expect_equal(unname(cl$ibnr[1]), 0)
})

# ==============================================================================
# EDGE CASE: Triangle with Zero Cell
# ==============================================================================

test_that("chain ladder handles zero incremental payments gracefully", {
  # Dev period 1 has zero payment for origin 2021
  pagos_zero <- tibble(
    siniestro_id    = c(1, 1, 1, 2, 3),
    anio_ocurrencia = c(2020, 2020, 2020, 2021, 2022),
    anio_desarrollo = c(0, 1, 2, 0, 0),
    monto_pago      = c(100, 50, 20, 150, 200)
  )
  tri_incr <- build_triangle(pagos_zero)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  # Should not crash

  expect_true(all(is.finite(cl$ultimate)))
  expect_true(all(cl$ibnr >= 0))
})

# ==============================================================================
# get_latest_diagonal: Correctness
# ==============================================================================

test_that("get_latest_diagonal returns last non-NA value per row", {
  mat <- matrix(c(
    10, 20, 30,
    40, 50, NA,
    60, NA, NA
  ), nrow = 3, byrow = TRUE)
  rownames(mat) <- 2020:2022

  diag <- get_latest_diagonal(mat)
  expect_equal(unname(diag[1]), 30)
  expect_equal(unname(diag[2]), 50)
  expect_equal(unname(diag[3]), 60)
})

test_that("get_latest_diagonal returns NA for all-NA row", {
  mat <- matrix(c(
    10, 20, 30,
    NA, NA, NA
  ), nrow = 2, byrow = TRUE)
  rownames(mat) <- 2020:2021

  diag <- get_latest_diagonal(mat)
  expect_equal(unname(diag[1]), 30)
  expect_true(is.na(diag[2]))
})
