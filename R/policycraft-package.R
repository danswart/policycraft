#' policycraft: Observe, Reason, Insight
#'
#' `policycraft` is a toolkit for analysts who advise civic boards, public
#' bodies, and other decision-makers. It keeps observations in temporal order,
#' supports reasoning about the system that generated them, and helps analysts
#' develop defensible insight rather than react to every fluctuation.
#'
#' The package does not claim that software produces policy recommendations.
#' Its charts and calculations are instruments. Policy insight still requires
#' subject-matter knowledge, institutional context, values, trade-offs, legal
#' constraints, implementation judgment, and clear communication.
#'
#' @section Analytical stance:
#' Many systems are influenced by numerous small forces, with no single force
#' dominating. Their measurements can fluctuate while remaining predictable
#' within empirically estimated limits. Expectation charts preserve temporal
#' order and help an analyst look for evidence that one or more forces may have
#' become unusually influential. A signal is a reason to investigate the
#' system; it is not proof of a cause and not an instruction to manipulate the
#' measured outcome.
#'
#' @section Main workflow:
#' * **Observe:** import, validate, order, and visualize longitudinal evidence.
#' * **Reason:** evaluate stability, direction, limits, runs, and suitability
#'   for trended or untrended expectation charts.
#' * **Insight:** combine analytical findings with policy context and explain
#'   implications, uncertainty, and options to decision-makers.
#'
#' @section Important functions:
#' * [launch_longitudinal()] launches the longitudinal-analysis tool.
#' * [as_policy_data()] standardizes longitudinal data.
#' * [expectation_chart_data()] calculates moving-range expectation limits.
#' * [detect_runs()] identifies long runs on one side of a center line.
#' * [longitudinal_summary()] produces a compact analytical summary.
#'
#' @section Interactive application:
#' The Shiny application accepts ordinary CSV, Excel, and data-frame RDS files,
#' plus validated canonical bundles. For ordinary data, the analyst identifies
#' the ordering column, declares whether it contains dates or observation
#' numbers, identifies the value column, and applies the mapping. The app offers
#' filtering, grouping, run and line charts, bar and cohort views, trended and
#' untrended expectation charts, autocorrelation diagnostics, limit
#' recalculation, and image or editable ggplot2-code downloads.
#'
"_PACKAGE"
