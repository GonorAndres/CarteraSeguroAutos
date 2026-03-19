# Future Ideas -- CarteraSeguroAutos

Ideas for future iterations, organized by category. Not a backlog -- just a living list of possibilities to explore.

---

## Actuarial Depth

- **Negative Binomial frequency model**: Add toggle in GLM tab to compare Poisson vs NB. Test for overdispersion and show which fits better.
- **Tweedie GLM**: Single model for pure premium (handles zero claims naturally). Compare against two-part freq x sev.
- **Experience rating / Credibility**: Buhlmann credibility module. Weight individual policyholder experience vs portfolio average. Limited fluctuation credibility (1,082 claims standard).
- **Bonus-Malus system**: Simulate a Mexican-style BM scale with transition matrix visualization. Show how premiums evolve over renewal years based on claim history.
- **A/E analysis**: Actual vs Expected ratios by segment over time. Track model degradation.
- **Burning cost analysis**: Trend-adjusted pure premium by segment. Rate adequacy check per cell.
- **Reinsurance modeling**: Excess of Loss treaty impact (different retention/limit structures). Quota share. Net vs gross loss ratio under each structure.

## Data & Modeling

- **Multi-year data enhancement**: Extend to 10 years for deeper trend analysis. Add catastrophe events (hailstorm in Nuevo Leon, flooding in Tabasco).
- **Real data integration**: Connect to a PostgreSQL/MySQL backend instead of SQLite. Add ETL pipeline for CSV uploads.
- **Survival analysis for retention**: Kaplan-Meier curves by segment. Cox PH model for lapse prediction. Customer lifetime value estimation.
- **Predictive scoring**: XGBoost or random forest for claim probability prediction. Compare against GLM. SHAP values for interpretability.
- **Time series forecasting**: ARIMA or Prophet for monthly claim volume prediction. Seasonal decomposition with trend projection.
- **Portfolio optimization**: Given risk appetite constraints, what's the optimal mix? Efficient frontier visualization (risk vs return by composition).

## Fraud & Risk

- **Enhanced fraud detection**: Isolation Forest for unsupervised anomaly detection. Network analysis for claim rings (policyholders with shared addresses/vehicles). Text analysis on claim descriptions (if added).
- **Early warning system**: Dashboard alerts when KPIs breach thresholds. Email/Slack notification triggers.
- **Catastrophe scenarios**: Pre-built stress tests for specific events (earthquake CDMX, hurricane Cancun). Impact on reserves and capital.
- **Regulatory capital**: LISF/CUSF capital requirement calculations. Best Estimate Liabilities. Risk margin.

## UX & Visualization

- **Leaflet map**: Install system dependencies (libgdal) and enable the choropleth map of Mexican states. Currently falls back to bar charts.
- **Loading spinners**: Add waiter/shinycssloaders around heavy reactive outputs (GLM, DT tables, Monte Carlo).
- **Dark mode toggle**: bslib supports theme switching. Add a navbar toggle for light/dark.
- **PDF report export**: Generate on-demand PDF report from current dashboard state using RMarkdown. Include all visible charts and filters applied.
- **Bookmarkable state**: Enable Shiny URL bookmarking so filter selections can be shared via link.
- **Guided tour**: Add cicerone or introjs walkthrough for first-time users explaining each tab.
- **Mobile optimization**: Further responsive work for phone-sized screens (<576px). Collapsible sidebar by default on mobile.

## Infrastructure & Deployment

- **GCP Cloud Run deployment**: Docker build + `gcloud run deploy`. Configure health checks and scaling.
- **CI/CD pipeline**: Complete GitHub Actions workflow -- lint (lintr), test (testthat), build Docker, deploy to Cloud Run on merge to main.
- **Database connection pooling**: Use `pool` package instead of direct `dbConnect` for production multi-user access.
- **Caching**: Use `memoise` or `shinyCache` for expensive computations (GLM fitting, Monte Carlo). Invalidate on filter change.
- **Logging & monitoring**: Add structured logging with `logger` package. Track tab visits, filter usage, computation times.
- **User authentication**: Add shinymanager or Google OAuth for access control.
- **API layer**: Expose GLM predictions via plumber API endpoint for integration with other systems.

## Content & Documentation

- **Blog post**: Write a detailed blog post for gonorandres.github.io explaining the methodology, architecture, and actuarial reasoning.
- **Technical documentation**: Auto-generate roxygen2 docs for all utility functions.
- **Video walkthrough**: Record a 5-minute demo video showing the dashboard capabilities for the portfolio site.
- **Benchmark comparison**: Add a reference panel comparing portfolio metrics against public CONDUSEF/AMIS industry averages.
- **Multilingual support**: Add English/Spanish toggle for international portfolio audience.

---

*Last updated: 2026-03-19*
