#' Calculate expectation-chart values from moving ranges
#'
#' Calculates an untrended center line and upper and lower expectation limits
#' from sequential moving ranges. The observations remain in temporal order.
#' Limits describe the observed system under the selected baseline; they are not
#' specification limits, confidence intervals, targets, or proof of causation.
#'
#' @param data A data frame containing longitudinal observations.
#' @param date Date column. Supply an unquoted name or character string.
#' @param value Numeric measure column. Supply an unquoted name or character
#'   string.
#' @param multiplier Number of estimated sigma units used for each limit.
#' @param d2 Bias-correction constant for moving ranges of length two. The
#'   conventional value is `1.128`.
#' @param lower_bound Optional natural lower bound, such as `0` for counts.
#' @param upper_bound Optional natural upper bound, such as `100` for percentage
#'   points.
#'
#' @return A tibble ordered by date with `center`, `upper_limit`, `lower_limit`,
#'   `moving_range`, and `outside_limits` columns. Calculation details are also
#'   stored in the `expectation` attribute.
#' @export
#'
#' @examples
#' x <- data.frame(year = 2015:2024, measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21))
#' expectation_chart_data(x, year, measure)
expectation_chart_data <- function(
  data,
  date,
  value,
  multiplier = 3,
  d2 = 1.128,
  lower_bound = -Inf,
  upper_bound = Inf
) {
  prepared <- as_policy_data(data, {{ date }}, {{ value }}, drop_missing = TRUE)
  prepared <- dplyr::arrange(prepared, date)
  if (nrow(prepared) < 2) {
    stop("At least two valid, temporally ordered observations are required.", call. = FALSE)
  }
  center <- mean(prepared$value)
  moving_range <- c(NA_real_, abs(diff(prepared$value)))
  average_moving_range <- mean(moving_range, na.rm = TRUE)
  sigma <- average_moving_range / d2
  upper <- min(center + multiplier * sigma, upper_bound)
  lower <- max(center - multiplier * sigma, lower_bound)
  prepared$center <- center
  prepared$upper_limit <- upper
  prepared$lower_limit <- lower
  prepared$moving_range <- moving_range
  prepared$outside_limits <- prepared$value > upper | prepared$value < lower
  attr(prepared, "expectation") <- list(
    center = center,
    average_moving_range = average_moving_range,
    sigma = sigma,
    multiplier = multiplier,
    d2 = d2
  )
  prepared
}

#' Detect long runs on one side of a center line
#'
#' Marks observations at and beyond the point where a run reaches the requested
#' length. Points exactly on the center line interrupt a run. Missing values
#' also interrupt a run rather than being silently removed from temporal order.
#'
#' @param value Numeric observations in temporal order.
#' @param center A scalar center line or a vector with the same length as
#'   `value`.
#' @param length Minimum run length. Eight is the package default.
#'
#' @return A logical vector with the same length as `value`.
#' @export
#' @examples
#' detect_runs(c(rep(2, 9), rep(0, 8)), center = 1)
detect_runs <- function(value, center, length = 8L) {
  value <- policy_numbers(value)
  if (length(center) == 1L) center <- rep(center, base::length(value))
  if (base::length(center) != base::length(value)) {
    stop("center must be a scalar or have the same length as value.", call. = FALSE)
  }
  side <- ifelse(is.na(value) | is.na(center) | value == center, NA, value > center)
  signal <- rep(FALSE, base::length(value))
  start <- 1L
  while (start <= base::length(side)) {
    if (is.na(side[start])) {
      start <- start + 1L
      next
    }
    end <- start
    while (end < base::length(side) && !is.na(side[end + 1L]) && side[end + 1L] == side[start]) {
      end <- end + 1L
    }
    if (end - start + 1L >= length) signal[(start + length - 1L):end] <- TRUE
    start <- end + 1L
  }
  signal
}

#' Summarize longitudinal behavior for analytical review
#'
#' Produces calculation-level facts for an analyst to interpret. The returned
#' object deliberately uses neutral language: signals warrant investigation,
#' but do not identify causes or prescribe policy.
#'
#' @param data A data frame containing longitudinal observations.
#' @param date Date column.
#' @param value Measure column.
#' @param run_length Minimum run length used by [detect_runs()].
#' @param ... Additional arguments passed to [expectation_chart_data()].
#'
#' @return A one-row tibble containing observation count, date range, center,
#'   limits, average moving range, counts of limit and run signals, and an
#'   `investigate` flag.
#' @export
#' @examples
#' x <- data.frame(year = 2015:2024, measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21))
#' longitudinal_summary(x, year, measure)
longitudinal_summary <- function(data, date, value, run_length = 8L, ...) {
  chart <- expectation_chart_data(data, {{ date }}, {{ value }}, ...)
  run_signal <- detect_runs(chart$value, chart$center, length = run_length)
  details <- attr(chart, "expectation")
  tibble::tibble(
    observations = nrow(chart),
    first_date = min(chart$date),
    last_date = max(chart$date),
    center = details$center,
    lower_limit = chart$lower_limit[1],
    upper_limit = chart$upper_limit[1],
    average_moving_range = details$average_moving_range,
    outside_limit_signals = sum(chart$outside_limits, na.rm = TRUE),
    run_signals = sum(run_signal, na.rm = TRUE),
    investigate = any(chart$outside_limits | run_signal, na.rm = TRUE)
  )
}
