#' Plot a standardized expectation chart
#'
#' Creates a console-friendly expectation chart using the same moving-range
#' calculations as [expectation_chart_data()]. Points outside the expectation
#' limits, and points completing a run on one side of the center, are
#' highlighted for investigation. Signals describe unusual patterns in the
#' observed series; they do not establish a cause or prescribe an action.
#'
#' @param data A data frame containing longitudinal observations.
#' @param date Date column. Supply an unquoted name or character string.
#' @param value Numeric measure column. Supply an unquoted name or character
#'   string.
#' @param run_length Minimum run length passed to [detect_runs()]. Set to
#'   `NULL` to omit run signals.
#' @param title,subtitle,caption Plot labels. Use `NULL` to omit a label.
#' @param x_label,y_label Axis labels.
#' @param ... Additional arguments passed to [expectation_chart_data()], such
#'   as `lower_bound`, `upper_bound`, or `multiplier`.
#'
#' @return A `ggplot` object. Its plotting data include `run_signal` and
#'   `signal` columns, making the result straightforward to inspect or extend
#'   with additional ggplot2 layers.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' x <- data.frame(
#'   year = 2015:2024,
#'   measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21)
#' )
#' plot_expectation_chart(x, year, measure)
plot_expectation_chart <- function(
  data,
  date,
  value,
  run_length = 8L,
  title = "Expectation Chart",
  subtitle = "Moving-range expectation limits",
  caption = "Signals warrant investigation; they do not identify causes.",
  x_label = "Time Period",
  y_label = "Value",
  ...
) {
  chart <- expectation_chart_data(data, {{ date }}, {{ value }}, ...)

  if (!is.null(run_length)) {
    if (length(run_length) != 1L || is.na(run_length) || run_length < 2 || run_length %% 1 != 0) {
      stop("run_length must be NULL or a single integer of at least 2.", call. = FALSE)
    }
    chart$run_signal <- detect_runs(chart$value, chart$center, length = run_length)
  } else {
    chart$run_signal <- FALSE
  }
  chart$signal <- chart$outside_limits | chart$run_signal

  ggplot2::ggplot(chart, ggplot2::aes(x = .data$date, y = .data$value)) +
    ggplot2::geom_line(group = 1, colour = "#666666", linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = .data$center), colour = "#2E86AB", linewidth = 0.9) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$upper_limit),
      colour = "#C73E1D", linewidth = 0.8, linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$lower_limit),
      colour = "#C73E1D", linewidth = 0.8, linetype = "dashed"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$signal), size = 2.8, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#2E86AB", `TRUE` = "#C73E1D")) +
    ggplot2::scale_x_date(
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.03, 0.03))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = title, subtitle = subtitle, caption = caption,
      x = x_label, y = y_label
    ) +
    policycraft_chart_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}
