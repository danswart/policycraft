# Detect long runs on one side of a center line

Marks observations at and beyond the point where a run reaches the
requested length. Points exactly on the center line interrupt a run.
Missing values also interrupt a run rather than being silently removed
from temporal order.

## Usage

``` r
detect_runs(value, center, length = 8L)
```

## Arguments

- value:

  Numeric observations in temporal order.

- center:

  A scalar center line or a vector with the same length as `value`.

- length:

  Minimum run length. Eight is the package default.

## Value

A logical vector with the same length as `value`.

## Examples

``` r
detect_runs(c(rep(2, 9), rep(0, 8)), center = 1)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE  TRUE
```
