---
name: Mahalanobis n<10 guard bug in mod_fraud.R
description: return() inside tryCatch inside mutate does not work as intended; small groups get 0 instead of NA for Mahalanobis distance
type: project
---

The fraud module (R/mod_fraud.R) has a subtle bug in the Mahalanobis computation for claim groups with fewer than 10 observations. The code uses `return(rep(NA_real_, n()))` inside a `tryCatch` block inside `dplyr::mutate`. In this context, `return()` does not return from the tryCatch expression -- it triggers the error handler, which returns `rep(0, n())`.

**Why:** The `return()` function inside `tryCatch({...})` within `mutate()` behaves as a non-local return that triggers the error handler. This means small groups silently get `mahal_dist = 0` instead of `NA`, making them appear at the bottom of the percentile ranking rather than being excluded from the Mahalanobis component of the composite score.

**How to apply:** If this module is refactored, the fix is to use an if/else branch instead of `return()` inside tryCatch. The tests in `test-fraud_advanced.R` document this behavior and will catch any regression or fix.
