# List analytical series in a canonical bundle

Returns one row per `series_id`, with the defining observation
dimensions, valid period, measure definition, unit, derivation
definition, and row count. Fields that are absent from a compatible
registry are returned as `NA`.

## Usage

``` r
list_canonical_series(bundle)
```

## Arguments

- bundle:

  A canonical bundle accepted by
  [`validate_canonical_bundle()`](https://danswart.github.io/policycraft/reference/canonical_bundle.md).

## Value

A data frame with one row per series.
