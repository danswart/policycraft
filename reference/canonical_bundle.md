# Canonical longitudinal bundle support

A canonical longitudinal bundle is a named R list with a supported
`schema_version` and six data-frame components: `observations`,
`measures`, `derivations`, `lineage`, `screening_report`, and
`validation_results`. These helpers validate that contract and resolve
one registry-defined analytical series without filtering, grouping, or
aggregating its rows.

## Usage

``` r
is_canonical_bundle(x)

validate_canonical_bundle(x, error = TRUE)

load_canonical_bundle(path)
```

## Arguments

- x:

  An object to inspect or validate.

- error:

  If `TRUE`, stop with all validation problems. If `FALSE`, return a
  logical value with the problems in a `problems` attribute.

- path:

  Path to an RDS file.

## Value

`is_canonical_bundle()` returns one logical value.
`validate_canonical_bundle()` and `load_canonical_bundle()` return the
validated bundle with class `policycraft_canonical_bundle`.

## Details

Version `canonical_longitudinal_bundle/0.1.0` requires the core fields
used by the canonical handoff. Producers may include the additional
fields in the published schema; policycraft preserves them and validates
their foreign keys when applicable.
