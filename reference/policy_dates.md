# Standardize common longitudinal date representations

Converts dates, datetimes, four-digit years, fiscal-year labels such as
`FY20`, school-year strings, Excel serial dates, and common explicit
date formats into `Date` values. Implausible years are returned as
missing.

## Usage

``` r
policy_dates(
  x,
  min_year = 1990L,
  max_year = as.integer(format(Sys.Date(), "%Y")) + 3L
)
```

## Arguments

- x:

  A vector containing dates or date-like values.

- min_year:

  Earliest plausible year.

- max_year:

  Latest plausible year.

## Value

A `Date` vector with the same length as `x`.

## Examples

``` r
policy_dates(c("2019", "FY20", "2021-06-30"))
#> [1] "2019-01-01" "2020-08-31" "2021-06-30"
```
