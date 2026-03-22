library(testthat)

source(here::here("R/utils_theme.R"), local = TRUE)

# --- kpi_color ---
test_that("kpi_color returns correct colors", {
  expect_equal(kpi_color(0.60, 0.70, 0.80), "success")
  expect_equal(kpi_color(0.75, 0.70, 0.80), "warning")
  expect_equal(kpi_color(0.85, 0.70, 0.80), "danger")
  expect_equal(kpi_color(NA, 0.70, 0.80), "secondary")
})

# --- format_currency_mxn ---
test_that("format_currency_mxn formats correctly", {
  expect_equal(format_currency_mxn(12345), "$12,345")
  expect_equal(format_currency_mxn(0), "$0")
  result <- format_currency_mxn(1234567)
  expect_match(result, "1,234,567")
})

# --- format_currency_millions ---
test_that("format_currency_millions shows M suffix", {
  result <- format_currency_millions(5000000)
  expect_match(result, "\\$5M")
})

# --- format_pct ---
test_that("format_pct formats percentages", {
  expect_equal(format_pct(0.7428), "74.28%")
  expect_equal(format_pct(0.1, digits = 1), "10.0%")
})

# --- format_num ---
test_that("format_num adds thousands separator", {
  expect_equal(format_num(140385), "140,385")
  expect_equal(format_num(42), "42")
})

# --- PALETTE ---
test_that("PALETTE contains expected colors", {
  expect_true("primary" %in% names(PALETTE))
  expect_true("danger" %in% names(PALETTE))
  expect_true("success" %in% names(PALETTE))
  expect_match(PALETTE$primary, "^#[0-9A-Fa-f]{6}$")
})

# --- KPI_TARGETS ---
test_that("KPI_TARGETS has expected structure", {
  expect_true("loss_ratio" %in% names(KPI_TARGETS))
  expect_true("bueno" %in% names(KPI_TARGETS$loss_ratio))
  expect_true("alerta" %in% names(KPI_TARGETS$loss_ratio))
})

# --- Edge cases: NA/NaN/Inf ---
test_that("format_currency_mxn handles NA and Inf", {
  expect_equal(format_currency_mxn(NA), "--")
  expect_equal(format_currency_mxn(Inf), "--")
  expect_equal(format_currency_mxn(NaN), "--")
})

test_that("format_currency_millions handles NA and Inf", {
  expect_equal(format_currency_millions(NA), "--")
  expect_equal(format_currency_millions(Inf), "--")
})

test_that("format_pct handles NA and Inf", {
  expect_equal(format_pct(NA), "--")
  expect_equal(format_pct(Inf), "--")
  expect_equal(format_pct(NaN), "--")
})

test_that("format_num handles NA and Inf", {
  expect_equal(format_num(NA), "--")
  expect_equal(format_num(Inf), "--")
})
