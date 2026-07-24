# Pure helpers for uploaded data. These functions intentionally contain no
# reactive reads or writes, which keeps import behavior deterministic and easy
# to exercise with testthat.

detect_and_convert_dates <- function(data) {
  for (col in names(data)) {
    if (!is.numeric(data[[col]])) next
    values <- data[[col]][!is.na(data[[col]])]
    if (length(values) == 0) next

    if (all(values >= 1900 & values <= 2100) && all(values == floor(values))) {
      data[[col]] <- year_to_date(data[[col]])
    } else if (all(values >= 15000 & values <= 60000)) {
      data[[col]] <- as.Date(data[[col]], origin = "1899-12-30")
    } else if (
      all(values >= 19000101 & values <= 21001231) &&
        all(nchar(as.character(values)) == 8)
    ) {
      data[[col]] <- as.Date(as.character(data[[col]]), format = "%Y%m%d")
    }
  }
  data
}

guess_date_value_columns <- function(data) {
  original_names <- names(data)
  lower_names <- tolower(original_names)
  non_standard_names <- setdiff(original_names, c("date", "value", "Year"))

  find_exact_col <- function(possible_names) {
    idx <- which(lower_names %in% tolower(possible_names))
    if (length(idx) == 0) return(character(0))
    original_names[idx[1]]
  }

  exact_fiscal_year_col <- find_exact_col(c(
    "fiscal_year", "fiscal year", "fiscal.year", "fiscal-year", "fy"
  ))
  exact_date_col <- find_exact_col("date")
  exact_year_col <- find_exact_col("year")
  named_date_candidates <- non_standard_names[grepl(
    paste0(
      "(^fy$|^fy[_ .-]*[0-9]*$|fiscal|school.?year|school_year|academic.?year|",
      "test.?year|assessment.?year|^year$|\\byr\\b|date)"
    ),
    non_standard_names,
    ignore.case = TRUE
  )]

  date_candidates <- if (length(exact_fiscal_year_col) > 0) {
    exact_fiscal_year_col
  } else if (length(exact_date_col) > 0) {
    exact_date_col
  } else if (length(exact_year_col) > 0) {
    exact_year_col
  } else if (length(named_date_candidates) > 0) {
    named_date_candidates
  } else {
    non_standard_names
  }

  best_date_col <- NA_character_
  best_nonmissing <- 0L
  for (col in date_candidates) {
    nonmissing <- sum(!is.na(standardize_to_date(data[[col]])))
    if (nonmissing > best_nonmissing) {
      best_nonmissing <- nonmissing
      best_date_col <- col
    }
  }
  if (is.na(best_date_col)) {
    best_date_col <- if (length(original_names) > 0) original_names[1] else NA_character_
  }

  exact_value_col <- find_exact_col("value")
  if (length(exact_value_col) > 0) {
    best_value_col <- exact_value_col
  } else {
    named_value_candidates <- non_standard_names[grepl(
      "pct|percent|percentage|rate|score|measure|amount|result|meets|above|performance|proficien|level|value",
      non_standard_names,
      ignore.case = TRUE
    )]
    candidate_pool <- unique(c(named_value_candidates, non_standard_names))
    candidate_pool <- candidate_pool[!grepl(
      paste0(
        "(^fy$|^fy[_ .-]*[0-9]*$|fiscal|^year$|school.?year|academic.?year|date|",
        "grade|id$|code$|sort$|section$|grouping$|name$|level_achieved)"
      ),
      candidate_pool,
      ignore.case = TRUE
    )]
    candidate_pool <- setdiff(candidate_pool, best_date_col)

    value_candidates <- candidate_pool[vapply(
      candidate_pool,
      function(col) {
        col %in% named_value_candidates || is.numeric(data[[col]]) || is.integer(data[[col]])
      },
      logical(1)
    )]
    best_value_col <- NA_character_
    best_nonmissing_value <- 0L
    for (col in unique(value_candidates)) {
      nonmissing <- sum(!is.na(safe_numeric(data[[col]])))
      if (nonmissing > best_nonmissing_value) {
        best_nonmissing_value <- nonmissing
        best_value_col <- col
      }
    }
    if (is.na(best_value_col)) {
      remaining <- setdiff(original_names, best_date_col)
      best_value_col <- if (length(remaining) > 0) remaining[1] else original_names[1]
    }
  }

  list(date_col = best_date_col, value_col = best_value_col)
}

add_chart_columns <- function(data, date_col_override = NULL, value_col_override = NULL) {
  if (is.null(data) || nrow(data) == 0) return(data)

  data <- tibble::as_tibble(data)
  original_names <- names(data)
  guesses <- guess_date_value_columns(data)
  date_col <- if (!is.null(date_col_override) && date_col_override %in% original_names) {
    date_col_override
  } else {
    guesses$date_col
  }
  value_col <- if (
    !is.null(value_col_override) &&
      value_col_override %in% original_names &&
      !identical(value_col_override, date_col)
  ) {
    value_col_override
  } else {
    guesses$value_col
  }

  best_date <- if (!is.na(date_col)) {
    standardize_to_date(data[[date_col]])
  } else {
    empty_date(nrow(data))
  }
  date_is_missing <- all(is.na(best_date))
  if (date_is_missing) {
    best_date <- as.Date("1970-01-01") + seq_len(nrow(data)) - 1L
  }
  best_value <- if (!is.na(value_col)) {
    safe_numeric(data[[value_col]])
  } else {
    rep(NA_real_, nrow(data))
  }

  data$date <- as.Date(best_date)
  data$value <- as.numeric(best_value)
  data$Year <- lubridate::year(data$date)
  if (date_is_missing) data$Year <- NA_integer_
  data <- dplyr::relocate(data, date, value, dplyr::any_of("Year"), .before = 1)
  names(data)[1:2] <- c("date", "value")
  data
}

extract_data_frame_from_rds <- function(x) {
  if (inherits(x, "data.frame")) return(tibble::as_tibble(x))
  if (is.list(x)) {
    df_locs <- vapply(x, inherits, logical(1), what = "data.frame")
    if (any(df_locs)) return(tibble::as_tibble(x[[which(df_locs)[1]]]))
  }
  stop(
    "The uploaded .rds file must contain a data frame/tibble, or a list containing a data frame/tibble.",
    call. = FALSE
  )
}

grouped_line_endpoints <- function(data, group_var, date_var = "date") {
  if (is.null(data)) return(data.frame())
  if (
    !group_var %in% names(data) ||
      !date_var %in% names(data)
  ) {
    return(data[0, , drop = FALSE])
  }

  data |>
    dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[date_var]])) |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::slice_max(order_by = .data[[date_var]], n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()
}

prepare_grouped_chart_lines <- function(data, group_var) {
  use_groups <- !is.null(group_var) &&
    nzchar(group_var) &&
    group_var %in% names(data) &&
    !is.numeric(data[[group_var]])

  if (!use_groups) {
    data$.plot_group <- factor("Series")
    return(list(data = data, labels = data[0, , drop = FALSE], grouped = FALSE))
  }

  data <- make_discrete_group(data, group_var)
  list(
    data = data,
    labels = grouped_line_endpoints(data, ".plot_group"),
    grouped = TRUE
  )
}

grouping_category_label <- function(data, group_var, fallback = "Cohort") {
  if (
    is.null(data) ||
      is.null(group_var) ||
      !nzchar(group_var) ||
      !group_var %in% names(data)
  ) {
    return(fallback)
  }

  labels <- unique(trimws(as.character(data[[group_var]])))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0) fallback else paste(labels, collapse = " / ")
}
