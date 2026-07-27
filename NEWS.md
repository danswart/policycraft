# policycraft 0.1.0

- Makes expectation-chart recalculation use only observations before the
  selected intervention date for the original center line and limits.
- Requires a single `series_id` (when present) and one observation per date,
  preventing unrelated longitudinal series from being combined silently.
- Preserves explicit missing periods as chart gaps and prevents moving ranges
  from bridging across them.
- Adds superintendent term arrows and labels to every longitudinal chart in
  the Shiny application, including date-based and cohort views.
- Adds `plot_expectation_chart()` for standardized expectation charts from the
  R console, including limit and run-signal highlighting.
- Adds `policycraft_chart_theme()` as the single shared theme for console and
  application charts, keeping their typography and colors synchronized.

## New package

- Establishes the Observe → Reason → Insight analytical framework.
- Packages the longitudinal-analysis Shiny application.
- Adds data standardization, expectation-limit, runs, and summary functions.
- Adds tests, Roxygen documentation, a vignette, and GitHub Actions checks.
