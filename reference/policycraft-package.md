# policycraft: Observe, Reason, Insight

`policycraft` is a toolkit for analysts who advise civic boards, public
bodies, and other decision-makers. It keeps observations in temporal
order, supports reasoning about the system that generated them, and
helps analysts develop defensible insight rather than react to every
fluctuation.

## Details

The package does not claim that software produces policy
recommendations. Its charts and calculations are instruments. Policy
insight still requires subject-matter knowledge, institutional context,
values, trade-offs, legal constraints, implementation judgment, and
clear communication.

## Analytical stance

Many systems are influenced by numerous small forces, with no single
force dominating. Their measurements can fluctuate while remaining
predictable within empirically estimated limits. Expectation charts
preserve temporal order and help an analyst look for evidence that one
or more forces may have become unusually influential. A signal is a
reason to investigate the system; it is not proof of a cause and not an
instruction to manipulate the measured outcome.

## Main workflow

- **Observe:** import, validate, order, and visualize longitudinal
  evidence.

- **Reason:** evaluate stability, direction, limits, runs, and
  suitability for trended or untrended expectation charts.

- **Insight:** combine analytical findings with policy context and
  explain implications, uncertainty, and options to decision-makers.

## Important functions

- [`launch_longitudinal()`](https://danswart.github.io/policycraft/reference/launch_longitudinal.md)
  launches the longitudinal-analysis tool.

- [`as_policy_data()`](https://danswart.github.io/policycraft/reference/as_policy_data.md)
  standardizes longitudinal data.

- [`expectation_chart_data()`](https://danswart.github.io/policycraft/reference/expectation_chart_data.md)
  calculates moving-range expectation limits.

- [`detect_runs()`](https://danswart.github.io/policycraft/reference/detect_runs.md)
  identifies long runs on one side of a center line.

- [`longitudinal_summary()`](https://danswart.github.io/policycraft/reference/longitudinal_summary.md)
  produces a compact analytical summary.

## See also

Useful links:

- <https://danswart.github.io/policycraft/>

- <https://github.com/danswart/policycraft>

- Report bugs at <https://github.com/danswart/policycraft/issues>

## Author

**Maintainer**: Dan Swart <danswartcpa@gmail.com>

Authors:

- Dan Swart <danswartcpa@gmail.com>
