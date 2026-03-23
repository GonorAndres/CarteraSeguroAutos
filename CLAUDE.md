# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

R Shiny dashboard for actuarial analysis of a synthetic Mexican auto insurance portfolio (140K policies, 12K claims, 28K development payments across 2020-2024). Calibrated to CONDUSEF/AMIS market parameters. Deployed to Google Cloud Run via Docker.

## Commands

```bash
# Run the app locally (requires renv packages restored)
Rscript -e 'shiny::runApp()'

# Run all tests
Rscript -e 'testthat::test_dir("tests/testthat")'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-utils_metrics.R")'

# Restore dependencies (first time or after renv.lock changes)
Rscript -e 'renv::restore()'

# Regenerate synthetic data (writes CSVs then loads to SQLite)
Rscript data/GeneracionDatos.R && Rscript data/CargaDatos.R

# Docker
docker build -t cartera-autos .
docker run -p 8080:8080 cartera-autos
```

## Architecture

### Data flow

```
data/GeneracionDatos.R -> data/processed/*.csv -> data/CargaDatos.R -> data/siniestralidad.db
                                                                              |
global.R: load_data() + enrich_polizas() -> APP_DATA (list: polizas, siniestros, pagos)
                                                              |
                                    sidebarFiltersServer("filters", APP_DATA) -> filtered (reactive)
                                                              |
                              all modules receive `filtered` reactive as their single data input
```

`global.R` loads everything once at startup. The sidebar filter module (`mod_sidebar_filters.R`) produces a single reactive `filtered` that every tab module consumes. There is no cross-module communication -- all modules are independent peers that read the same filtered reactive.

### Module pattern

Every module follows the same convention: `{name}UI(id)` + `{name}Server(id, filtered_data)`. UI functions return bslib layout components. Server functions take the filtered reactive and compute everything internally.

- **Basic tabs** (7): resumen, loss_ratio, frecuencia, severidad, temporal, geografico, segmentacion -- standard KPI/chart views.
- **Advanced actuarial tabs** (4): pricing_glm, ibnr, scenario, fraud -- each implements its actuarial model from scratch (no external actuarial packages).

### Utilities layer (R/utils_*.R)

- `utils_data.R` -- `load_data()`, `enrich_polizas()`, `filter_data()`, `get_filter_choices()`. All data loading and transformation.
- `utils_metrics.R` -- Pure functions: `calc_loss_ratio()`, `calc_frequency()`, `calc_severity()`, `calc_kpis()`, `calc_kpis_delta()`. Accept polizas/siniestros tibbles and optional grouping vars via `...` (tidy eval with `enquos()`).
- `utils_theme.R` -- `PALETTE`, `PALETTE_CATEGORICAL`, `app_theme` (bslib Bootstrap 5), `KPI_TARGETS`, formatting helpers (`format_currency_mxn`, `format_pct`, `format_num`, `format_currency_millions`), `plotly_default_layout()`, `plotly_bar()`, `plotly_clean()`, `humanize_colnames()`, `COLUMN_LABELS`.
- `utils_export.R` -- `download_csv_handler()`, `download_excel_handler()`, `download_excel_multi_handler()`.

### Actuarial models (implemented from scratch in modules)

- **IBNR** (`mod_ibnr.R`): `build_triangle()`, `incr_to_cum()`, `cum_to_incr()`, `chain_ladder()` with Mack standard errors. Bornhuetter-Ferguson via expected loss ratio slider. These functions are defined at module top-level (not inside server), so tests can source the file directly.
- **Pricing GLM** (`mod_pricing_glm.R`): Two-part model -- Poisson GLM for frequency, Gamma GLM for severity. Pure premium = predicted frequency x predicted severity. Interactive quoter.
- **Scenarios** (`mod_scenario.R`): Collective risk model. Calibrates Gamma severity (method of moments) and Poisson frequency from data. Monte Carlo simulation (1K-50K runs). Computes VaR/TVaR at 95/99/99.5%.
- **Fraud** (`mod_fraud.R`): Mahalanobis distance per claim type (on monto_siniestro, dias_reporte, deducible) + 5 rule-based flags. Composite score = 0.4 * mahal_percentile + 0.6 * (n_flags / 5).

### Data model (SQLite tables)

Three tables in `data/siniestralidad.db`:
- `polizas` -- keyed by `poliza_id`, linked to prior policy via `poliza_original_id` for renewals
- `siniestros` -- keyed by `siniestro_id`, FK to `poliza_id`
- `pagos_desarrollo` -- development payments, FK to `siniestro_id`, indexed by `(anio_ocurrencia, anio_desarrollo)`

## Conventions

- All monetary values in MXN. Use `format_currency_mxn()` for display.
- UI labels are in Spanish; code identifiers and comments mix Spanish and English.
- Every plotly chart must go through `plotly_default_layout()` for consistent styling.
- Tests source utils directly with `source(here::here("R/utils_metrics.R"), local = TRUE)` -- they don't load Shiny.
- The Docker build installs renv packages to the system library and removes `.Rprofile` to avoid renv activation at runtime. Keep this pattern when modifying the Dockerfile.
- R 4.3.3, renv for dependency management, 2-space indentation.

## CI/CD

GitHub Actions (`.github/workflows/ci.yml`): test job runs `testthat::test_dir()` on every push/PR to main. Deploy job (main only) builds Docker, pushes to Artifact Registry, deploys to Cloud Run (2 vCPU, 2 GiB) via Workload Identity Federation.
