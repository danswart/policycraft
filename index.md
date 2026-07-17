# policycraft

**Observe. Reason. Insight.**

`policycraft` is an R toolkit for policy analysts who examine
longitudinal system data and translate disciplined observation into
decision-useful insight. It is designed for analysts advising civic
boards, public bodies, and other decision-makers—and for analysts who
share its emphasis on temporal order, system behavior, non-random
patterns, and careful interpretation.

The charts are tools. Insight is the product.

## Analytical stance

Most measured systems are affected by many small forces. Their data can
move up and down without a single force predominating, yet remain
predictable within empirically estimated limits. An expectation chart
helps an analyst ask whether the observed pattern remains consistent
with that routine system or whether the data contain evidence that
warrants investigation.

A signal does not identify a cause. It does not prove that a policy
worked or failed. It is a reason to investigate the system, combine the
evidence with institutional and policy knowledge, and communicate
uncertainty honestly.

## Installation

``` r

# install.packages("pak")
pak::pak("danswart/policycraft")
```

## Launch the longitudinal-analysis tool

``` r

library(policycraft)
launch_longitudinal()
```

The application accepts CSV, Excel, and RDS files and provides column
mapping, dynamic filtering, run and line charts, trended and untrended
expectation charts, cohort analysis, autocorrelation diagnostics, and
chart exports.

## Use the calculation functions directly

``` r

library(policycraft)

example_data <- data.frame(
  year = 2015:2024,
  measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21)
)

expectation_chart_data(example_data, year, measure)
longitudinal_summary(example_data, year, measure)
```

## Observe → Reason → Insight

1.  **Observe** the measurements in temporal order and understand their
    source.
2.  **Reason** about stability, direction, limits, runs, data quality,
    and alternative explanations.
3.  **Develop insight** by combining the analytical evidence with policy
    goals, institutional context, values, constraints, trade-offs, and
    implementation knowledge.

Software does not make policy recommendations. Analysts do.

## Documentation

- [`vignette("longitudinal-analysis", package = "policycraft")`](https://danswart.github.io/policycraft/articles/longitudinal-analysis.md)
- [`?policycraft`](https://danswart.github.io/policycraft/reference/policycraft-package.md)
- [`?expectation_chart_data`](https://danswart.github.io/policycraft/reference/expectation_chart_data.md)
- [`?launch_longitudinal`](https://danswart.github.io/policycraft/reference/launch_longitudinal.md)

## Development status

This package is experimental and is hosted on GitHub rather than
submitted to CRAN. Interfaces may evolve as additional policy-analysis
tools are added.

## License

MIT © 2026 Dan Swart
