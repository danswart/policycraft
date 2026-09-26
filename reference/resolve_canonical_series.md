# Resolve exactly one canonical analytical series

The resolver subsets observations exactly once by `series_id`, orders
those unchanged rows by `observation_date`, and attaches matching
registry, lineage, validation, and screening metadata. It never
aggregates values.

## Usage

``` r
resolve_canonical_series(bundle, series_id)
```

## Arguments

- bundle:

  A canonical bundle accepted by
  [`validate_canonical_bundle()`](https://danswart.github.io/policycraft/reference/canonical_bundle.md).

- series_id:

  Exactly one available series identifier.

## Value

A `policycraft_resolved_series` list. Its `observations` member is the
exact resolved analytical input.
