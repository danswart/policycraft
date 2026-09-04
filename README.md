# policycraft <img src="man/figures/logo.png" align="right" height="139" alt="policycraft hex sticker" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/danswart/policycraft/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/danswart/policycraft/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Observe. Reason. Insight.**

`policycraft` is an R toolkit for policy analysts to progressively
examine longitudinal system data and translate the observations
into insight that may be useful to decision-makers who are responsible
for the system under review. 

It is designed for analysts advising civic boards, public bodies, and
other decision-makers—and for analysts who share its emphasis on
temporal order, system behavior, non-random patterns, and careful
interpretation.

The charts are tools. Insight is the product.


## Analytical stance

Most systems are affected by many forces creating up-and-down variation
in the measures observed. It is the job of the Policy Analyst to
estimate what those 'ups' and 'downs' probably mean, within the context
of earlier outputs.  This simply cannot be done if the data are not
assembled in date/time order for analysis.

Once data are assembled in the order of their occurance the Longitudinal
tool allows the Policy Analyst to filter the data into the desired
groupings. Then, to progressively examine the selected output with a Run
Chart, a Line Chart, a Bar Chart and, finally, with an Expectation Chart
(trended or untrended).  I call it an Expectation Chart because it will
establish what the reasonable expectations of the process output should
be. 

Placing the data on an Expectation Chart (aka
Control Chart, or Process Behavior Chart) allows the analyst to
estimate:

1. Have system outputs in the past been stable (predictable within the
empirically estimated limits provided by the chart). When the output
moves up and down without a single large 'cause' predominating, it will appear to move randomly between the upper and lower expectaton limits. and remain predictable within limits.

An expectation chart  helps an analyst determine whether the observed
pattern remains consistent with the routine system established before
the policy or whether the data contain evidence that warrants
investigation.

A signal does not identify a cause specifically. but can often point to
one based on when the unusual pattern becomes apparent. It does not
prove that a policy worked or failed. It is a reason to investigate the
system, combine the evidence with institutional and policy knowledge,
and communicate uncertainty honestly.

## Installation

```r
# install.packages("pak")
pak::pak("danswart/policycraft")
```

## Launch the longitudinal-analysis tool

```r
library(policycraft)
launch_longitudinal()
```

The application accepts CSV, Excel, and RDS files and provides explicit column
mapping, dynamic filtering, run and line charts, trended and untrended
expectation charts, cohort analysis, autocorrelation diagnostics, limit
recalculation, and reproducible chart exports.

For an ordinary file, choose the ordering and value columns, declare whether
the ordering column contains **Calendar dates** or an **Observation sequence**,
and click **Apply mapping to app data**. Observation numbers remain observation
numbers; the app does not manufacture calendar dates for the visible axis.

Use the chart tabs progressively:

1. Inspect the mapped rows in **Data Table**.
2. Use **Run Chart** and **Line Chart** to examine order, direction, gaps, and
   grouping.
3. Use **Bar Chart** for a selected categorical comparison.
4. Use **Untrended Expectation Chart** or **Trended Expectation Chart** only
   after considering whether the corresponding model is suitable.
5. Use **Cohort Chart**, **Auto-correlation Analysis**, and **Runs Debug** when
   those diagnostics address the analytical question.

The expectation charts flag observations outside the limits and three run-rule
patterns: eight observations on one side of the center line, six observations
steadily increasing or decreasing, and fourteen observations alternating up
and down. These are investigation signals, not causal findings.

Enable recalculation to select either a date or an observation at which a new
center line and limits begin. The pre-boundary observations establish the
original frozen baseline; rules do not bridge the recalculation boundary.

Every chart can be downloaded as PNG, SVG, or PDF. **Download R Code** produces
a compact, runnable script with recognizable `data.frame()`, named
calculations, `ggplot()`, `geom_*()`, scales, CL/UCL/LCL labels, and theme calls.
The script can be sourced or pasted into a Quarto code cell and edited normally.

### Canonical longitudinal bundles

policycraft also accepts a versioned RDS list containing `observations`,
`measures`, `derivations`, `lineage`, `screening_report`, and
`validation_results`. Version `canonical_longitudinal_bundle/0.1.0` is
supported. Validation requires stable observation and series identifiers and
checks required fields, registry references, units, and lineage endpoints;
failures stop analysis.

`list_canonical_series()` describes available `series_id` values using their
registry metadata. In the app, all validated observations remain available for
filtering, inspection, comparison-oriented Line charts, and export from the
data table. The exact filtered rows form one shared analytical input. Run,
expectation, and autocorrelation diagnostics require the filters to leave
exactly one `series_id`. Missing periods remain gaps.

The app displays series dimensions and valid period, measure and derivation
definitions, observation statuses and source identifiers, lineage, validation
results, and unresolved screening issues. Filtering and grouping select only
existing observations: the app does not construct new combined totals, means,
weighted means, or ratios. Create such specialized series explicitly in a
curation script or Quarto document, validate them, and then analyze the curated
result in policycraft. Ordinary flat-file uploads retain their existing mapping,
filtering, and grouping workflow.

External producers should declare the observation grain, create stable unique
IDs, register every measure and derivation, link derived values to source
observations, include screening and validation tables, and set the supported
schema version. The schema and synthetic example under
`inst/canonical-handoff/` provide construction guidance.

For comparisons such as grade 3 versus grade 4 over time, select a discrete
numeric column such as `tested_grade` in **Line/Bar Chart Grouping**. Numeric
grouping values are treated as category labels, producing one line per selected
grade. The grouping summary reports only categories remaining after all current
filters are applied.

### Expectation-chart input requirements

An expectation chart must contain one temporally ordered process series with
no more than one observation per date. Analysis-ready files should include a
stable `series_id`; filter it to exactly one value before opening the
Expectation Chart tab. The app rejects multi-series or duplicate-date inputs
instead of calculating moving ranges across unrelated grades, subjects,
organizations, standards, or student groups.

When **Enable Recalculation** is selected, the observations before the chosen
date or observation establish the original center line and expectation limits. Those frozen
baseline limits are then used to evaluate the later observations. Explicit
missing periods remain gaps and interrupt moving-range calculations; they are
not removed, converted to zero, interpolated, or silently bridged.

## Use the calculation functions directly

```r
library(policycraft)

example_data <- data.frame(
  year = 2015:2024,
  measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21)
)

expectation_chart_data(example_data, year, measure)
longitudinal_summary(example_data, year, measure)

# Create the package's standardized expectation chart in the plot pane
plot_expectation_chart(example_data, year, measure)
```

## Observe → Reason → Insight

1. **Observe** the measurements in temporal order and understand their source.
2. **Reason** about stability, direction, limits, runs, data quality, and
   alternative explanations.
3. **Develop insight** by combining the analytical evidence with policy goals,
   institutional context, values, constraints, trade-offs, and implementation
   knowledge.

Software does not make policy recommendations. Analysts do.

## Documentation

- `vignette("using-longitudinal-app", package = "policycraft")`
- `vignette("longitudinal-analysis", package = "policycraft")`
- `?policycraft`
- `?expectation_chart_data`
- `?plot_expectation_chart`
- `?launch_longitudinal`

## Development status

This package is experimental and is hosted on GitHub rather than submitted to
CRAN. Interfaces may evolve as additional policy-analysis tools are added.

## License

MIT © 2026 Dan Swart
