# policycraft 0.1.1

- Adds an explicit mapping workflow for calendar dates versus observation
  sequences. Sequence numbers remain ordinal observations and are no longer
  converted to artificial 1970 dates.
- Adds an **Apply mapping to app data** action so proposed mappings are never
  mistaken for committed analytical changes.
- Supports expectation-limit recalculation from either a selected date or a
  selected observation number.
- Expands expectation-chart run rules to include eight observations on one
  side of the center line, six steadily increasing or decreasing observations,
  and fourteen alternating observations. Missing values and recalculation
  boundaries interrupt runs.
- Adds compact, readable **Download R Code** output for every chart. Generated
  scripts contain ordinary data preparation, named calculations, ggplot2
  layers, matching axis graduations, and editable labels rather than serialized
  Shiny objects.
- Labels CL, UCL, and LCL in exported expectation charts using bold 4 mm text
  and removes major and minor grids from expectation-chart output.
- Adds validated canonical longitudinal RDS bundles, an explorer for existing
  canonical observations, registry metadata, observation lineage, filtered-row
  export, and one shared no-reaggregation analytical input.
- Keeps filtering and grouping available for canonical and ordinary uploads,
  while requiring exactly one existing series for expectation-style and
  autocorrelation diagnostics. Specialized derived series belong in explicit
  curation scripts or Quarto documents rather than hidden chart calculations.
- Places grouped-series labels in reserved space to the right of each line's
  final data point, keeping endpoint markers and labels visually distinct.

# policycraft 0.1.0

- Allows line, run, and related grouped charts to use discrete numeric grouping
  columns such as `tested_grade`, and reports grouping categories from the
  currently filtered rows instead of the full uploaded dataset.
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
