library(testthat)
library(tidyverse)

source(here::here("R/utils_theme.R"), local = TRUE)
source(here::here("R/mod_ibnr.R"), local = TRUE)

# --- Test data: known triangle ---
pagos_test <- tibble(
  siniestro_id    = c(1,1,1, 2,2,2, 3,3, 4,4, 5, 6),
  anio_ocurrencia = c(2020,2020,2020, 2020,2020,2020, 2021,2021, 2021,2021, 2022, 2022),
  anio_desarrollo = c(0,1,2, 0,1,2, 0,1, 0,1, 0, 0),
  monto_pago      = c(100,50,20, 200,80,30, 150,60, 180,70, 120, 160)
)

# --- build_triangle ---
test_that("build_triangle produces correct matrix shape", {
  tri <- build_triangle(pagos_test)
  expect_true(is.matrix(tri))
  expect_equal(nrow(tri), 3)  # 3 origin years
  expect_equal(ncol(tri), 3)  # dev 0, 1, 2
})

test_that("build_triangle has NA for future cells", {
  tri <- build_triangle(pagos_test)
  # 2022 (row 3) should have NA for dev 1 and 2
  expect_true(is.na(tri[3, 2]))
  expect_true(is.na(tri[3, 3]))
  # 2021 (row 2) should have NA for dev 2
  expect_true(is.na(tri[2, 3]))
  # 2020 (row 1) should have no NAs
  expect_true(!any(is.na(tri[1, ])))
})

# --- incr_to_cum / cum_to_incr roundtrip ---
test_that("incr_to_cum and cum_to_incr are inverses", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  tri_back <- cum_to_incr(tri_cum)

  # Non-NA values should match
  for (i in seq_len(nrow(tri_incr))) {
    for (j in seq_len(ncol(tri_incr))) {
      if (!is.na(tri_incr[i, j])) {
        expect_equal(tri_back[i, j], tri_incr[i, j],
                     tolerance = 0.01,
                     info = paste("Cell", i, j))
      }
    }
  }
})

test_that("incr_to_cum produces monotonically increasing rows", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  # Row 1 (fully developed): each column >= previous
  for (j in 2:ncol(tri_cum)) {
    if (!is.na(tri_cum[1, j]) && !is.na(tri_cum[1, j - 1])) {
      expect_true(tri_cum[1, j] >= tri_cum[1, j - 1])
    }
  }
})

# --- get_latest_diagonal ---
test_that("get_latest_diagonal extracts correct values", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  diag <- get_latest_diagonal(tri_cum)
  expect_length(diag, nrow(tri_cum))
  # Row 1 latest = last column value (fully developed)
  expect_equal(unname(diag[1]), unname(tri_cum[1, ncol(tri_cum)]))
  # Row 3 latest = first column value (only dev 0)
  expect_equal(unname(diag[3]), unname(tri_cum[3, 1]))
})

# --- chain_ladder ---
test_that("chain_ladder returns valid structure", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  expect_true(is.list(cl))
  expect_true(all(c("ldf", "cdf", "latest", "ultimate", "ibnr", "se", "origin") %in% names(cl)))
})

test_that("chain_ladder LDFs are >= 1", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)
  expect_true(all(cl$ldf >= 1.0))
})

test_that("chain_ladder ultimate >= latest", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)
  expect_true(all(cl$ultimate >= cl$latest))
})

test_that("chain_ladder IBNR is non-negative", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)
  expect_true(all(cl$ibnr >= 0))
})

test_that("chain_ladder fully developed year has IBNR = 0", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)
  # First origin year is fully developed
  expect_equal(unname(cl$ibnr[1]), 0)
})

test_that("chain_ladder SE is non-negative and finite", {
  tri_incr <- build_triangle(pagos_test)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)
  expect_true(all(cl$se >= 0))
  expect_true(all(is.finite(cl$se)))
})

# --- chain_ladder with real data ---
test_that("chain_ladder works with actual project data", {
  source(here::here("R/utils_data.R"), local = TRUE)
  d <- load_data(here::here("data/siniestralidad.db"))
  con <- DBI::dbConnect(RSQLite::SQLite(), here::here("data/siniestralidad.db"))
  pagos <- DBI::dbGetQuery(con, "SELECT * FROM pagos_desarrollo") %>% tibble::as_tibble()
  DBI::dbDisconnect(con)

  tri_incr <- build_triangle(pagos)
  tri_cum <- incr_to_cum(tri_incr)
  cl <- chain_ladder(tri_cum)

  expect_equal(length(cl$origin), 5)  # 5 accident years
  expect_true(sum(cl$ibnr) > 0)       # total IBNR should be positive
  expect_true(all(is.finite(cl$ultimate)))
})
