#' Standardize common longitudinal date representations
#'
#' Converts dates, datetimes, four-digit years, fiscal-year labels such as
#' `FY20`, school-year strings, Excel serial dates, and common explicit date
#' formats into `Date` values. Implausible years are returned as missing.
#'
#' @param x A vector containing dates or date-like values.
#' @param min_year Earliest plausible year.
#' @param max_year Latest plausible year.
#'
#' @return A `Date` vector with the same length as `x`.
#' @export
#'
#' @examples
#' policy_dates(c("2019", "FY20", "2021-06-30"))
policy_dates <- function(
  x,
  min_year = 1990L,
  max_year = as.integer(format(Sys.Date(), "%Y")) + 3L
) {
  n <- length(x)
  empty <- function() as.Date(rep(NA_character_, n))
  plausible <- function(value) {
    value <- as.Date(value)
    year <- suppressWarnings(as.integer(format(value, "%Y")))
    value[is.na(year) | year < min_year | year > max_year] <- as.Date(NA_character_)
    value
  }
  if (inherits(x, "Date")) return(plausible(x))
  if (inherits(x, "POSIXt")) return(plausible(as.Date(x)))

  value <- trimws(as.character(x))
  value[value %in% c("", "NA", "N/A", "NULL", "-")] <- NA_character_
  result <- empty()

  fiscal <- grepl("(?i)\\b(?:FY|FISCAL[ _.-]*YEAR)[ _.-]*([0-9]{2}|20[0-9]{2})\\b", value, perl = TRUE)
  if (any(fiscal, na.rm = TRUE)) {
    token <- sub(
      "(?i).*\\b(?:FY|FISCAL[ _.-]*YEAR)[ _.-]*([0-9]{2}|20[0-9]{2})\\b.*",
      "\\1",
      value[fiscal],
      perl = TRUE
    )
    year <- as.integer(token)
    year[year < 100] <- ifelse(year[year < 100] <= 79, 2000 + year[year < 100], 1900 + year[year < 100])
    result[fiscal] <- as.Date(sprintf("%04d-08-31", year))
  }

  formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d", "%m-%d-%Y", "%B %d, %Y", "%b %d, %Y")
  for (format in formats) {
    candidate <- suppressWarnings(as.Date(value, format = format))
    replace <- is.na(result) & !is.na(candidate)
    result[replace] <- candidate[replace]
  }

  four_digit <- is.na(result) & grepl("(19|20)[0-9]{2}", value)
  year <- suppressWarnings(as.integer(sub(".*?((19|20)[0-9]{2}).*", "\\1", value)))
  valid_year <- four_digit & year >= min_year & year <= max_year
  result[valid_year] <- as.Date(sprintf("%04d-01-01", year[valid_year]))

  numeric_value <- suppressWarnings(as.numeric(value))
  serial <- is.na(result) & !is.na(numeric_value) & numeric_value >= 15000 & numeric_value <= 60000
  result[serial] <- as.Date(numeric_value[serial], origin = "1899-12-30")
  plausible(result)
}

#' Convert common measure values to numeric values
#'
#' Handles numeric vectors, comma-formatted values, percent signs, missing-value
#' tokens, and cells containing a numeric token plus explanatory text. Percent
#' values are returned in the scale shown: `"53.2%"` becomes `53.2`, not `0.532`.
#'
#' @param x A vector of measure values.
#' @return A numeric vector.
#' @export
#' @examples
#' policy_numbers(c("1,240", "53.2%", "score: -4.5", "N/A"))
policy_numbers <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  value <- trimws(as.character(x))
  value[value %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null", "-")] <- NA_character_
  value <- gsub("[,|%]", "", value)
  value <- sub(".*?(-?[0-9]+\\.?[0-9]*).*", "\\1", value)
  suppressWarnings(as.numeric(value))
}

#' Prepare a data frame for longitudinal policy analysis
#'
#' Creates standardized `date` and `value` columns while retaining the original
#' fields. Explicit column selection is preferred because policy datasets often
#' contain several plausible dates and measures.
#'
#' @param data A data frame or tibble.
#' @param date Column containing time values. Supply an unquoted name or a
#'   character string.
#' @param value Column containing the measure. Supply an unquoted name or a
#'   character string.
#' @param drop_missing If `TRUE`, remove rows with missing standardized dates or
#'   values.
#'
#' @return A tibble with `date`, `value`, and `year` as its first columns.
#' @export
#' @examples
#' raw <- data.frame(fiscal_year = c("FY20", "FY21"), rate = c("51%", "54%"))
#' as_policy_data(raw, fiscal_year, rate)
as_policy_data <- function(data, date, value, drop_missing = FALSE) {
  date <- rlang::ensym(date)
  value <- rlang::ensym(value)
  result <- tibble::as_tibble(data)
  result$date <- policy_dates(result[[rlang::as_string(date)]])
  result$value <- policy_numbers(result[[rlang::as_string(value)]])
  result$year <- lubridate::year(result$date)
  result <- dplyr::relocate(result, dplyr::all_of(c("date", "value", "year")), .before = 1)
  if (drop_missing) result <- dplyr::filter(result, !is.na(date), !is.na(value))
  result
}
