# Launch the longitudinal-analysis tool

Starts policycraft's interactive longitudinal-analysis tool. The
application accepts CSV, Excel, and RDS files and provides mapping,
filtering, longitudinal charts, expectation charts, cohort views, and
export controls. Ordinary uploads explicitly distinguish calendar dates
from observation sequences; mappings take effect only after the user
applies them. Expectation limits can be recalculated from a selected
date or observation. Selected points can be omitted from center-line and
limit estimation while remaining visible on both expectation charts.
Run-rule diagnostics include long same-side runs, six-point
increasing/decreasing trends, and fourteen-point alternation. Every
chart can be downloaded as PNG, SVG, PDF, or compact editable ggplot2
code for a Quarto report. RDS inputs may use the validated canonical
longitudinal bundle contract; canonical series retain their registered
grain, metadata, and lineage. Line and bar charts can group series by
character, factor, date, or discrete numeric columns such as tested
grade. Canonical filters inspect existing observations but do not
construct new totals, averages, or ratios.

## Usage

``` r
launch_longitudinal(
  launch_browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", NULL),
  max_upload_mb = 50,
  ...
)

load_longitudinal(
  launch_browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", NULL),
  max_upload_mb = 50,
  ...
)

run_policycraft(...)

run_app(...)
```

## Arguments

- launch_browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). Use
  `TRUE` to open the system browser, `FALSE` to run without opening a
  browser, or supply a custom browser function.

- host:

  Host address on which to serve the application. The default is Shiny's
  local-only default.

- port:

  Optional TCP port. `NULL` asks Shiny to choose an available port.

- max_upload_mb:

  Maximum permitted upload size in megabytes. The default is 50 MB. The
  previous `shiny.maxRequestSize` option is restored when the
  application closes.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly returns the value produced by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). The
function is normally called for its side effect of starting the app.

## Omit points from limit calculations

Open **Exclude observations from expectation-limit calculations** below
the filters and select dates or observation numbers. The untrended chart
estimates its center and limits from included values. The trended chart
fits its model to included values and predicts the line and limits
across all observations. Omitted points appear as crosses and still
participate in signal checks.

Remove an item from the selector to restore it, or use **Clear
exclusions**. Filtering keeps only selections still visible. A new
upload or applied column mapping clears the selections. The uploaded
file is never changed.

Moving ranges and lag-1 estimates use adjacent included pairs; they do
not join across omitted or missing points. For example, omitting the
third value from `c(10, 12, 100, 15, 19)` leaves a center of 14 and
moving ranges of 2 and 4, averaging 3. The point at 100 remains on the
chart.

Untrended limits need at least two included observations and an adjacent
included pair. With recalculation enabled, a later segment containing
fewer than three included observations keeps the original baseline
limits. Trended limits need at least three included observations at two
or more positions.

The optional autocorrelation adjustment uses included adjacent pairs
within each segment. If correlation cannot be estimated, the standard
moving-range calculation is used. When its absolute value is at least
0.999, the existing standard-deviation fallback uses only included
values.

Chart captions list omitted dates or observation numbers. Image
downloads retain the markers and captions; R downloads also retain
exclusion flags. These controls belong to the app.
[`expectation_chart_data()`](https://danswart.github.io/policycraft/reference/expectation_chart_data.md)
and
[`longitudinal_summary()`](https://danswart.github.io/policycraft/reference/longitudinal_summary.md)
do not currently accept point exclusions.

## Deprecated names

`run_policycraft()` and `run_app()` are retained temporarily for code
written before policycraft became a multi-tool package. New code should
use `launch_longitudinal()` (or its `load_longitudinal()` alias).

## Examples

``` r
if (interactive()) {
  launch_longitudinal()
}
```
