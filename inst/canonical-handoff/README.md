# Canonical bundle handoff for policycraft

This directory is an implementation handoff, not a change to the `policycraft` package.

## Bundle contract

Load the RDS and require a named list containing `observations`, `measures`, `derivations`, `lineage`, `screening_report`, and `validation_results`. Reject unsupported `schema_version` values. Canonical analysis must select one `series_id` whose validation status is usable; it must not re-aggregate a validated series.

The observation grain is one value per date/period, entity, geographic level, subject, tested grade, student group, administration, measure, source (for reported values), or derivation (for calculated values). `observation_id` and `series_id` are stable content-derived identifiers.

## Required package behavior later

1. Detect the named-list canonical contract in addition to ordinary flat files.
2. Present measure and derivation definitions and expose source-to-derived lineage.
3. Resolve exactly one validated analytical `series_id` and send the identical rows to Line, Run, Untrended, Trended, and autocorrelation tools.
4. Require exactly one series for an expectation chart.
5. Retain existing exploratory aggregation controls only for noncanonical uploads.
6. Never average rates when a registry specifies a ratio of sums.

## Unresolved semantic decisions

- Whether the legacy Satisfactory and current Approaches standards are substantively comparable enough for a single policy series. The bundle labels the operational bridge but does not claim psychometric equivalence.
- Whether annual counts across multiple administrations represent distinct students. The defensible unit is test records, so the current denominator label is tests taken.
- Research Portal blank cells do not distinguish suppressed, unavailable, and inapplicable values. They remain unresolved instead of being converted to zero.
- Region 20 and Texas counts are not in the available files. Their reported percentages are retained, but no reconstructed aggregate is claimed.

See `../outputs/validation_results.csv`, `../outputs/screening_report.csv`, and the bundle reconciliation tables for the SCUC test-case evidence.
