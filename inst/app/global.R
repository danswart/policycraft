# System Analysis Dashboard
# Loaded first by shiny::runApp() (before ui.R and server.R): libraries, shared
# constants, and standalone helper functions used by both ui.R and server.R.

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(ggtext)
library(purrr)
library(lubridate)
library(scales)

# Pure data-import and normalization helpers live outside server() so they can
# be unit-tested without starting a Shiny session.
source(file.path("R", "data_helpers.R"), local = FALSE)
source(file.path("R", "chart_download_module.R"), local = FALSE)

# IMPROVED: Safe numeric conversion function
# Handles ordinary numbers, comma-formatted numbers, percent strings,
# and cells that contain text plus a number such as "53.2%".
safe_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null", "-")] <- NA_character_

  # Remove commas and percent signs. Keep digits, decimal points, and minus signs.
  x_chr <- gsub(",", "", x_chr)
  x_chr <- gsub("%", "", x_chr)

  # If a cell contains extra text, keep the first numeric-looking token.
  x_chr <- sub(".*?(-?[0-9]+\\.?[0-9]*).*", "\\1", x_chr)

  result <- suppressWarnings(as.numeric(x_chr))
  return(result)
}

# Safe empty Date vector helper. Avoids as.Date(NA), which can dispatch
# to as.Date.character() in some contexts and trigger charToDate errors.
empty_date <- function(n) {
  as.Date(rep(NA_character_, n), format = "%Y-%m-%d")
}

# Safe year-to-date helper. Always supplies an explicit format so R never
# guesses date formats with charToDate().
year_to_date <- function(y) {
  as.Date(paste0(as.integer(y), "-01-01"), format = "%Y-%m-%d")
}

# Safe fiscal-year-to-date helper.
# For this dashboard, fiscal-year labels such as FY20, FY21, FY2019 are
# translated to the last day of the fiscal year: 2020-08-31, 2021-08-31,
# 2019-08-31, etc.
fiscal_year_to_date <- function(y) {
  as.Date(paste0(as.integer(y), "-08-31"), format = "%Y-%m-%d")
}

# Translate fiscal year strings into four-digit years. Handles examples like:
# FY20, FY 20, FY2020, FY 2020, Fiscal Year 20, Fiscal Year 2020.
extract_fiscal_year_number <- function(x) {
  x_chr <- trimws(as.character(x))
  out <- rep(NA_integer_, length(x_chr))

  has_fy_4 <- !is.na(x_chr) & grepl("(?i)\\b(?:FY|FISCAL[ _.-]*YEAR)[ _.-]*(20[0-9]{2})\\b", x_chr, perl = TRUE)
  if (any(has_fy_4)) {
    out[has_fy_4] <- suppressWarnings(as.integer(sub("(?i).*\\b(?:FY|FISCAL[ _.-]*YEAR)[ _.-]*(20[0-9]{2})\\b.*", "\\1", x_chr[has_fy_4], perl = TRUE)))
  }

  has_fy_2 <- !is.na(x_chr) & is.na(out) & grepl("(?i)\\b(?:FY|FISCAL[ _.-]*YEAR)[ _.-]*([0-9]{2})\\b", x_chr, perl = TRUE)
  if (any(has_fy_2)) {
    yy <- suppressWarnings(as.integer(sub("(?i).*\\b(?:FY|FISCAL[ _.-]*YEAR)[ _.-]*([0-9]{2})\\b.*", "\\1", x_chr[has_fy_2], perl = TRUE)))
    # Dashboard data are modern school/fiscal years. Treat FY00-FY79 as
    # 2000-2079 and FY80-FY99 as 1980-1999. Plausibility filtering below
    # will remove anything outside the usable dashboard range.
    out[has_fy_2] <- ifelse(yy <= 79L, 2000L + yy, 1900L + yy)
  }

  out
}

# Parse common school-year/year/date forms into a real Date column.
# Examples handled: 2018, "2018", "2018-19", "2018-2019",
# "SY 2018-19", "School Year 2023", Excel serial dates, and real Date/POSIXct columns.
standardize_to_date <- function(x) {
  n <- length(x)
  plausible_min_year <- 1990L
  plausible_max_year <- as.integer(format(Sys.Date(), "%Y")) + 3L

  make_empty <- function() {
    as.Date(rep(NA_character_, n), format = "%Y-%m-%d")
  }

  keep_plausible <- function(d) {
    d <- as.Date(d)
    y <- suppressWarnings(as.integer(format(d, "%Y")))
    d[is.na(y) | y < plausible_min_year | y > plausible_max_year] <- as.Date(NA_character_, format = "%Y-%m-%d")
    d
  }

  if (inherits(x, "Date")) {
    return(keep_plausible(x))
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(keep_plausible(as.Date(x)))
  }

  if (is.numeric(x)) {
    x_num <- suppressWarnings(as.numeric(x))
    out <- make_empty()
    ok <- !is.na(x_num)

    # Only plausible four-digit years become yearly dates. This prevents
    # counts/projections such as 2031 or 2044 from becoming false dates.
    year_ok <- ok & x_num >= plausible_min_year & x_num <= plausible_max_year & x_num == floor(x_num)
    out[year_ok] <- year_to_date(x_num[year_ok])

    serial_ok <- ok & x_num >= 15000 & x_num <= 60000 & !year_ok
    if (any(serial_ok)) {
      out[serial_ok] <- keep_plausible(as.Date(x_num[serial_ok], origin = "1899-12-30"))
    }

    ymd_ok <- ok & x_num >= 19000101 & x_num <= 21001231 & !year_ok & !serial_ok
    if (any(ymd_ok)) {
      out[ymd_ok] <- keep_plausible(suppressWarnings(as.Date(as.character(as.integer(x_num[ymd_ok])), format = "%Y%m%d")))
    }

    return(out)
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null", "-")] <- NA_character_

  parsed <- make_empty()

  # FIRST PRIORITY: fiscal-year strings such as FY20, FY21, FY2019,
  # and Fiscal Year 2020. These are deliberately translated to August 31
  # of the fiscal year, not guessed by R's date parser.
  fy_year_num <- extract_fiscal_year_number(x_chr)
  fy_year_ok <- !is.na(fy_year_num) & fy_year_num >= plausible_min_year & fy_year_num <= plausible_max_year
  parsed[fy_year_ok] <- fiscal_year_to_date(fy_year_num[fy_year_ok])

  # SECOND PRIORITY: fiscal/school-year strings such as SY2018-19,
  # 2018-2019, and plain four-digit years. We extract the first plausible
  # 4-digit year and turn it into Jan. 1 of that year.
  has_4_digit_year <- is.na(parsed) & !is.na(x_chr) & grepl("(19|20)[0-9]{2}", x_chr)
  year_txt <- rep(NA_character_, n)
  year_txt[has_4_digit_year] <- sub(".*?((19|20)[0-9]{2}).*", "\\1", x_chr[has_4_digit_year])
  year_num <- suppressWarnings(as.integer(year_txt))
  year_ok <- !is.na(year_num) & year_num >= plausible_min_year & year_num <= plausible_max_year
  parsed[year_ok] <- year_to_date(year_num[year_ok])

  # SECOND PRIORITY: ordinary date strings. Every format is explicit so R does
  # not fall back to charToDate guessing.
  date_formats <- c(
    "%Y-%m-%d",
    "%m/%d/%Y",
    "%m/%d/%y",
    "%Y/%m/%d",
    "%m-%d-%Y",
    "%m-%d-%y",
    "%B %d, %Y",
    "%b %d, %Y"
  )

  for (fmt in date_formats) {
    trial <- tryCatch(
      keep_plausible(suppressWarnings(as.Date(x_chr, format = fmt))),
      error = function(e) make_empty()
    )
    replace_idx <- is.na(parsed) & !is.na(trial)
    parsed[replace_idx] <- trial[replace_idx]
  }

  # THIRD PRIORITY: character cells that are entirely numeric. This catches
  # Excel serial dates and plain year strings, but deliberately ignores mixed
  # strings such as "53.2%".
  numeric_like <- !is.na(x_chr) & grepl("^[0-9]+(\\.0+)?$", x_chr)
  x_num <- rep(NA_real_, n)
  x_num[numeric_like] <- suppressWarnings(as.numeric(x_chr[numeric_like]))
  numeric_date <- make_empty()
  ok <- !is.na(x_num)

  year_ok <- ok & x_num >= plausible_min_year & x_num <= plausible_max_year & x_num == floor(x_num)
  numeric_date[year_ok] <- year_to_date(x_num[year_ok])

  serial_ok <- ok & x_num >= 15000 & x_num <= 60000 & !year_ok
  if (any(serial_ok)) {
    numeric_date[serial_ok] <- keep_plausible(as.Date(x_num[serial_ok], origin = "1899-12-30"))
  }

  ymd_ok <- ok & x_num >= 19000101 & x_num <= 21001231 & !year_ok & !serial_ok
  if (any(ymd_ok)) {
    numeric_date[ymd_ok] <- keep_plausible(suppressWarnings(as.Date(as.character(as.integer(x_num[ymd_ok])), format = "%Y%m%d")))
  }

  replace_idx <- is.na(parsed) & !is.na(numeric_date)
  parsed[replace_idx] <- numeric_date[replace_idx]

  return(parsed)
}

# Helper: make a grouping column safe for ggplot discrete color/fill scales.
# This prevents Date/numeric grouping variables from being treated as continuous
# when scale_color_manual() or scale_fill_manual() is used.
make_discrete_group <- function(data, group_var, new_col = ".plot_group") {
  if (is.null(group_var) || group_var == "" || !group_var %in% names(data)) {
    return(data)
  }

  if (inherits(data[[group_var]], "Date")) {
    data[[new_col]] <- format(data[[group_var]], "%Y")
  } else {
    data[[new_col]] <- as.character(data[[group_var]])
  }

  data[[new_col]] <- factor(data[[new_col]], levels = unique(data[[new_col]]))
  data
}

# IMPROVED: Safe statistical calculations
safe_mean <- function(x, na.rm = TRUE) {
  x_clean <- safe_numeric(x)
  if (length(x_clean[!is.na(x_clean)]) == 0) {
    return(NA)
  }
  return(mean(x_clean, na.rm = na.rm))
}

safe_median <- function(x, na.rm = TRUE) {
  x_clean <- safe_numeric(x)
  if (length(x_clean[!is.na(x_clean)]) == 0) {
    return(NA)
  }
  return(median(x_clean, na.rm = na.rm))
}

safe_sd <- function(x, na.rm = TRUE) {
  x_clean <- safe_numeric(x)
  if (length(x_clean[!is.na(x_clean)]) <= 1) {
    return(0)
  }
  return(sd(x_clean, na.rm = na.rm))
}

# ENHANCED: Safe auto-correlation calculation function (returns BOTH autocorrelation and sample correlation)
safe_autocorr <- function(x, lag = 1, na.rm = TRUE) {
  x_clean <- safe_numeric(x)

  if (length(x_clean[!is.na(x_clean)]) <= lag + 1) {
    return(list(acf = NA, r = NA))
  }

  # Remove NA values for autocorrelation calculation
  if (na.rm) {
    x_clean <- x_clean[!is.na(x_clean)]
  }

  if (length(x_clean) <= lag + 1) {
    return(list(acf = NA, r = NA))
  }

  n <- length(x_clean)
  if (n <= lag) {
    return(list(acf = NA, r = NA))
  }

  # Calculate mean
  x_mean <- mean(x_clean)

  # Calculate autocorrelation coefficient (normalized by variance)
  numerator <- 0
  denominator <- sum((x_clean - x_mean)^2)

  for (i in 1:(n - lag)) {
    numerator <- numerator + (x_clean[i] - x_mean) * (x_clean[i + lag] - x_mean)
  }

  # Autocorrelation coefficient
  acf_value <- numerator / denominator

  # Sample correlation coefficient (Pearson correlation)
  x_lag <- x_clean[1:(n - lag)]
  x_lead <- x_clean[(lag + 1):n]
  r_value <- cor(x_lag, x_lead, use = "complete.obs")

  return(list(acf = acf_value, r = r_value))
}

# NEW: Calculate moving ranges for auto-correlation adjustment
calculate_moving_ranges <- function(values) {
  values <- safe_numeric(values)
  values <- values[!is.na(values)]

  if (length(values) < 2) {
    return(NA)
  }

  # Calculate moving ranges (absolute difference between consecutive points)
  moving_ranges <- abs(diff(values))

  # Return average moving range
  avg_mr <- mean(moving_ranges, na.rm = TRUE)

  return(avg_mr)
}

# IMPROVED: Better runs analysis function with robust error handling
detect_runs_signals <- function(
  values,
  centerline,
  dates = NULL,
  min_run_length = 8
) {
  # Robust numeric conversion
  values <- safe_numeric(values)
  centerline <- safe_numeric(centerline)

  valid_indices <- which(
    !is.na(values) &
      !is.na(centerline) &
      is.finite(values) &
      is.finite(centerline)
  )
  if (length(valid_indices) < min_run_length) {
    return(rep(FALSE, length(values)))
  }

  # Calculate which side of centerline each point is on
  above_cl <- rep(NA, length(values))
  above_cl[valid_indices] <- values[valid_indices] > centerline[valid_indices]

  # Use run length encoding to find consecutive runs
  valid_above <- above_cl[valid_indices]
  runs <- rle(valid_above)

  # Find runs of min_run_length+ consecutive points
  long_runs <- runs$lengths >= min_run_length

  # Initialize signal vector
  signals <- rep(FALSE, length(values))

  if (any(long_runs)) {
    # Calculate positions in the valid subset
    valid_end_positions <- cumsum(runs$lengths)
    valid_start_positions <- c(
      1,
      valid_end_positions[-length(valid_end_positions)] + 1
    )

    for (i in which(long_runs)) {
      valid_run_start <- valid_start_positions[i]
      valid_run_end <- valid_end_positions[i]

      # Convert back to original indices
      run_start_idx <- valid_indices[valid_run_start]
      run_end_idx <- valid_indices[valid_run_end]

      # Mark from the 8th point onward in this run
      signal_start_in_valid <- valid_run_start + min_run_length - 1
      if (signal_start_in_valid <= length(valid_indices)) {
        signal_start_idx <- valid_indices[signal_start_in_valid]
        signals[signal_start_idx:run_end_idx] <- TRUE
      }
    }
  }

  return(signals)
}

# IMPROVED: Runs analysis for recalculated charts with robust error handling
detect_runs_signals_recalc <- function(
  values,
  centerline_orig,
  centerline_recalc,
  dates,
  recalc_date,
  min_run_length = 8
) {
  # Robust numeric conversion
  values <- safe_numeric(values)
  centerline_orig <- safe_numeric(centerline_orig)
  centerline_recalc <- safe_numeric(centerline_recalc)

  signals <- rep(FALSE, length(values))

  # Split data at recalculation point
  before_recalc <- which(!is.na(dates) & dates < recalc_date)
  after_recalc <- which(!is.na(dates) & dates >= recalc_date)

  # Analyze runs separately for each segment
  if (length(before_recalc) >= min_run_length) {
    before_centerline <- rep(centerline_orig[1], length(before_recalc))
    before_signals <- detect_runs_signals(
      values[before_recalc],
      before_centerline,
      dates[before_recalc],
      min_run_length
    )
    signals[before_recalc] <- before_signals
  }

  if (length(after_recalc) >= min_run_length) {
    after_centerline <- rep(centerline_recalc[1], length(after_recalc))
    after_signals <- detect_runs_signals(
      values[after_recalc],
      after_centerline,
      dates[after_recalc],
      min_run_length
    )
    signals[after_recalc] <- after_signals
  }

  return(signals)
}

# Custom 20-color palette for charts - ensures we never run out of colors
chart_colors <- c(
  "#2E86AB", # Ocean blue
  "#A23B72", # Berry purple
  "#F18F01", # Orange
  "#C73E1D", # Red
  "#8E5572", # Mauve
  "#007F5F", # Forest green
  "#7209B7", # Purple
  "#AA6C39", # Brown
  "#FF6B6B", # Coral
  "#4ECDC4", # Teal
  "#45B7D1", # Sky blue
  "#96CEB4", # Mint green
  "#FFEAA7", # Light yellow
  "#DDA0DD", # Plum
  "#F39C12", # Golden orange
  "#E74C3C", # Crimson
  "#3498DB", # Bright blue
  "#2ECC71", # Emerald
  "#9B59B6", # Amethyst
  "#F1C40F" # Sunshine yellow
)

# ---- Shared chart export/styling constants ----------------------------------
# Every chart is saved at the same physical size/resolution, and every chart's
# theme() block used the same sizes/colors copy-pasted six times. Centralizing
# them here means resizing exports or restyling charts is a one-line change.
CHART_EXPORT_WIDTH_IN <- 16
CHART_EXPORT_HEIGHT_IN <- 8
CHART_EXPORT_DPI <- 300

axis_label_or_default <- function(label, default) {
  if (is.null(label) || !nzchar(trimws(label))) default else trimws(label)
}

# Categorical filter dropdowns with more unique values than this send only the
# "All"/"None" sentinel choices to the browser up front; the full value list is
# then loaded via server-side selectize (updateSelectizeInput(..., server=TRUE))
# instead of shipping potentially thousands of <option> tags on every page load.
LARGE_DROPDOWN_THRESHOLD <- 200

# Blank placeholder plot with a centered message, used by every chart reactive
# whenever the data isn't in a plottable state (missing columns, all-NA values,
# no rows matching the current filters/selections, etc). Every chart renders
# this instead of returning NULL, so renderPlot shows the reason on screen and
# the PNG/SVG/PDF download handlers save a real (if blank) file explaining why,
# instead of silently writing a broken/empty export.
empty_chart_message <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label, size = 6) +
    theme_void()
}

# Shared theme applied by every chart (run/line/bar/control/trended/cohort).
# Callers add axis.text.x/axis.text.y/axis.title.y/panel.grid.minor overrides
# afterward with `+ theme(...)` for chart-specific orientation (e.g. bar_plot's
# horizontal bars, cohort_plot's blanked minor gridlines) — theme() composition
# only overrides the elements you specify, so this never changes prior output.
dashboard_chart_theme <- policycraft::policycraft_chart_theme

# Builds one chart tabPanel: title/subtitle/caption inputs, PNG/SVG/PDF download
# buttons, and the plot itself. All six chart tabs share this exact layout, so
# this replaces what used to be ~350 lines of copy-pasted UI. extra_controls
# lets a specific tab (e.g. the recalculation options on the Untrended
# Expectation Chart) inject markup between the info banner and the title row.
chart_tab_ui <- function(tab_label, info_output_id, id_prefix, default_title, extra_controls = NULL) {
  tabPanel(
    tab_label,
    br(),
    fluidRow(column(12, uiOutput(info_output_id))),
    br(),
    extra_controls,
    fluidRow(
      column(4, textInput(paste0(id_prefix, "_title"), "Chart Title:", value = default_title)),
      column(4, textInput(paste0(id_prefix, "_subtitle"), "Subtitle:", value = "")),
      column(4, textInput(paste0(id_prefix, "_caption"), "Caption:", value = ""))
    ),
    fluidRow(
      column(6, textInput(paste0(id_prefix, "_x_label"), "X-axis Label (optional):", value = "")),
      column(6, textInput(paste0(id_prefix, "_y_label"), "Y-axis Label (optional):", value = ""))
    ),
    br(),
    fluidRow(
      column(
        12,
        div(
          style = "text-align: center;",
          chart_download_ui(paste0(id_prefix, "_download"))
        )
      )
    ),
    br(),
    chart_plot_ui(paste0(id_prefix, "_chart"), height = "600px")
  )
}
