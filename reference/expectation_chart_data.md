# Calculate expectation-chart values from moving ranges

Calculates an untrended center line and upper and lower expectation
limits from sequential moving ranges. The observations remain in
temporal order. Limits describe the observed system under the selected
baseline; they are not specification limits, confidence intervals,
targets, or proof of causation.

## Usage

``` r
expectation_chart_data(
  data,
  date,
  value,
  multiplier = 3,
  d2 = 1.128,
  lower_bound = -Inf,
  upper_bound = Inf
)
```

## Arguments

- data:

  A data frame containing longitudinal observations.

- date:

  Date column. Supply an unquoted name or character string.

- value:

  Numeric measure column. Supply an unquoted name or character string.

- multiplier:

  Number of estimated sigma units used for each limit.

- d2:

  Bias-correction constant for moving ranges of length two. The
  conventional value is `1.128`.

- lower_bound:

  Optional natural lower bound, such as `0` for counts.

- upper_bound:

  Optional natural upper bound, such as `100` for percentage points.

## Value

A tibble ordered by date with `center`, `upper_limit`, `lower_limit`,
`moving_range`, and `outside_limits` columns. Calculation details are
also stored in the `expectation` attribute.

## Examples

``` r
x <- data.frame(year = 2015:2024, measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21))
expectation_chart_data(x, year, measure)
#> # A tibble: 10 × 9
#>    date       value  year measure center upper_limit lower_limit moving_range
#>    <date>     <dbl> <dbl>   <dbl>  <dbl>       <dbl>       <dbl>        <dbl>
#>  1 2015-01-01    10  2015      10   13.3        19.5        7.09           NA
#>  2 2016-01-01    11  2016      11   13.3        19.5        7.09            1
#>  3 2017-01-01     9  2017       9   13.3        19.5        7.09            2
#>  4 2018-01-01    12  2018      12   13.3        19.5        7.09            3
#>  5 2019-01-01    10  2019      10   13.3        19.5        7.09            2
#>  6 2020-01-01    11  2020      11   13.3        19.5        7.09            1
#>  7 2021-01-01    10  2021      10   13.3        19.5        7.09            1
#>  8 2022-01-01    19  2022      19   13.3        19.5        7.09            9
#>  9 2023-01-01    20  2023      20   13.3        19.5        7.09            1
#> 10 2024-01-01    21  2024      21   13.3        19.5        7.09            1
#> # ℹ 1 more variable: outside_limits <lgl>
```
