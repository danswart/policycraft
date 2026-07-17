# Summarize longitudinal behavior for analytical review

Produces calculation-level facts for an analyst to interpret. The
returned object deliberately uses neutral language: signals warrant
investigation, but do not identify causes or prescribe policy.

## Usage

``` r
longitudinal_summary(data, date, value, run_length = 8L, ...)
```

## Arguments

- data:

  A data frame containing longitudinal observations.

- date:

  Date column.

- value:

  Measure column.

- run_length:

  Minimum run length used by
  [`detect_runs()`](https://danswart.github.io/policycraft/reference/detect_runs.md).

- ...:

  Additional arguments passed to
  [`expectation_chart_data()`](https://danswart.github.io/policycraft/reference/expectation_chart_data.md).

## Value

A one-row tibble containing observation count, date range, center,
limits, average moving range, counts of limit and run signals, and an
`investigate` flag.

## Examples

``` r
x <- data.frame(year = 2015:2024, measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21))
longitudinal_summary(x, year, measure)
#> # A tibble: 1 × 10
#>   observations first_date last_date  center lower_limit upper_limit
#>          <int> <date>     <date>      <dbl>       <dbl>       <dbl>
#> 1           10 2015-01-01 2024-01-01   13.3        7.09        19.5
#> # ℹ 4 more variables: average_moving_range <dbl>, outside_limit_signals <int>,
#> #   run_signals <int>, investigate <lgl>
```
