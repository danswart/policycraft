# Convert common measure values to numeric values

Handles numeric vectors, comma-formatted values, percent signs,
missing-value tokens, and cells containing a numeric token plus
explanatory text. Percent values are returned in the scale shown:
`"53.2%"` becomes `53.2`, not `0.532`.

## Usage

``` r
policy_numbers(x)
```

## Arguments

- x:

  A vector of measure values.

## Value

A numeric vector.

## Examples

``` r
policy_numbers(c("1,240", "53.2%", "score: -4.5", "N/A"))
#> [1] 1240.0   53.2   -4.5     NA
```
