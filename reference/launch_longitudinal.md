# Launch the longitudinal-analysis tool

Starts policycraft's interactive longitudinal-analysis tool. The
application accepts CSV, Excel, and RDS files and provides mapping,
filtering, longitudinal charts, expectation charts, cohort views, and
export controls.

## Usage

``` r
launch_longitudinal(
  launch_browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", NULL),
  ...
)

load_longitudinal(
  launch_browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", NULL),
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

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly returns the value produced by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). The
function is normally called for its side effect of starting the app.

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
