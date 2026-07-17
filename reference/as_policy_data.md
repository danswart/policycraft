# Prepare a data frame for longitudinal policy analysis

Creates standardized `date` and `value` columns while retaining the
original fields. Explicit column selection is preferred because policy
datasets often contain several plausible dates and measures.

## Usage

``` r
as_policy_data(data, date, value, drop_missing = FALSE)
```

## Arguments

- data:

  A data frame or tibble.

- date:

  Column containing time values. Supply an unquoted name or a character
  string.

- value:

  Column containing the measure. Supply an unquoted name or a character
  string.

- drop_missing:

  If `TRUE`, remove rows with missing standardized dates or values.

## Value

A tibble with `date`, `value`, and `year` as its first columns.

## Examples

``` r
raw <- data.frame(fiscal_year = c("FY20", "FY21"), rate = c("51%", "54%"))
as_policy_data(raw, fiscal_year, rate)
#> # A tibble: 2 × 5
#>   date       value  year fiscal_year rate 
#>   <date>     <dbl> <dbl> <chr>       <chr>
#> 1 2020-08-31    51  2020 FY20        51%  
#> 2 2021-08-31    54  2021 FY21        54%  
```
