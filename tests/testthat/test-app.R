test_that("installed Shiny application is complete", {
  app <- system.file("app", package = "policycraft")
  expect_true(nzchar(app))
  expect_true(all(file.exists(file.path(app, c("global.R", "ui.R", "server.R")))))
})

test_that("chart downloads include compact, readable ggplot2 code", {
  app <- system.file("app", package = "policycraft")
  module_env <- new.env(parent = globalenv())
  sys.source(file.path(app, "R", "chart_download_module.R"), envir = module_env)

  original <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 1, 4)),
                              ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Reproducible chart")
  script <- module_env$ggplot_code_lines(original, "test_chart")
  script_file <- tempfile(fileext = ".R")
  writeLines(script, script_file)

  restored_env <- new.env(parent = globalenv())
  sys.source(script_file, envir = restored_env)
  expect_s3_class(restored_env$chart, "ggplot")
  expect_equal(restored_env$chart$labels$title, "Reproducible chart")
  script_text <- paste(script, collapse = "\n")
  expect_match(script_text, "chart_data <-", fixed = TRUE)
  expect_match(script_text, "ggplot2::ggplot(data = chart_data", fixed = TRUE)
  expect_match(script_text, "ggplot2::geom_line", fixed = TRUE)
  expect_match(script_text, "ggplot2::labs", fixed = TRUE)
  expect_false(grepl("serialize", script_text, fixed = TRUE))
  expect_false(grepl("base64", script_text, fixed = TRUE))
  expect_lt(nchar(script_text, type = "bytes"), 50000)
})

test_that("longitudinal charts include superintendent term annotations", {
  app <- system.file("app", package = "policycraft")
  app_env <- new.env(parent = globalenv())
  old_dir <- setwd(app)
  on.exit(setwd(old_dir), add = TRUE)
  sys.source("global.R", envir = app_env)

  expect_equal(nrow(app_env$SCUC_SUPERINTENDENT_TERMS), 5L)
  expect_identical(
    app_env$SCUC_SUPERINTENDENT_TERMS$superintendent,
    c(
      "Dr. Byron P. Steele II", "Dr. Edward \"Ed\" West",
      "Dr. Greg Gibson", "Dr. Clark C. Ealy", "Mrs. Paige A. Meloni"
    )
  )

  annual_layers <- app_env$superintendent_date_layers(
    as.Date(paste0(1995:2024, "-01-01"))
  )
  expect_length(annual_layers, 2L)
  expect_equal(nrow(annual_layers[[1]]$data), 5L)
  expect_true(any(grepl("Paige A. Meloni", annual_layers[[1]]$data$label)))

  cohort_layers <- app_env$superintendent_cohort_layers(
    data.frame(grade = 3:8, year = 2018:2023)
  )
  expect_length(cohort_layers, 2L)
})

test_that("longitudinal tool has a tool-specific public interface", {
  expect_true(is.function(launch_longitudinal))
  expect_identical(load_longitudinal, launch_longitudinal)
  expect_true(all(c("launch_browser", "host", "port", "max_upload_mb", "...") %in%
                    names(formals(launch_longitudinal))))
})

test_that("longitudinal tool validates its upload limit", {
  expect_error(
    launch_longitudinal(max_upload_mb = 0),
    "must be one positive, finite number",
    fixed = TRUE
  )
})

test_that("legacy application-wide names are deprecated", {
  exports <- getNamespaceExports("policycraft")
  expect_true(all(c("run_policycraft", "run_app") %in% exports))
  expect_match(paste(deparse(body(run_policycraft)), collapse = " "), ".Deprecated", fixed = TRUE)
  expect_match(paste(deparse(body(run_app)), collapse = " "), ".Deprecated", fixed = TRUE)
})

test_that("expectation chart input is restricted to one series", {
  app <- system.file("app", package = "policycraft")
  helper_env <- new.env(parent = globalenv())
  sys.source(file.path(app, "R", "data_helpers.R"), envir = helper_env)

  one <- data.frame(
    date = as.Date(c("2023-01-01", "2024-01-01")),
    value = c(0.4, 0.5),
    series_id = "district__grade_3__reading__meets"
  )
  expect_null(helper_env$expectation_series_problem(one))

  two <- rbind(one, transform(one, series_id = "district__grade_4__reading__meets"))
  expect_match(helper_env$expectation_series_problem(two), "exactly one series_id")
  expect_match(
    helper_env$expectation_series_problem(transform(one, date = as.Date("2024-01-01"))),
    "one observation per date"
  )
})

test_that("missing periods interrupt moving ranges", {
  app <- system.file("app", package = "policycraft")
  app_env <- new.env(parent = globalenv())
  old_dir <- setwd(app)
  on.exit(setwd(old_dir), add = TRUE)
  sys.source("global.R", envir = app_env)

  expect_true(is.na(app_env$calculate_moving_ranges(c(1, NA, 5))))
  expect_equal(app_env$calculate_moving_ranges(c(1, 2, NA, 5, 7)), 1.5)
})

test_that("expectation charts detect monotonic and alternating run rules", {
  app <- system.file("app", package = "policycraft")
  app_env <- new.env(parent = globalenv())
  old_dir <- setwd(app)
  on.exit(setwd(old_dir), add = TRUE)
  sys.source("global.R", envir = app_env)

  dataset_3_values <- c(90.4, 51.4, 61.5, 64.6, 98.1, 98.2, 74.3, 56.2, 60.7, 68.8, 70.3, 79.4, 81.4, 88.9, 89.2)
  dataset_4_values <- c(80.4, 70.1, 81, 82.7, 57.8, 97.4, 90.5, 89.8, 85.3, 68.9, 67, 65.5, 61.1, 60.1, 57.5)

  expect_equal(which(app_env$detect_trend_signals(dataset_3_values)), 13:15)
  expect_equal(which(app_env$detect_trend_signals(dataset_4_values)), 11:15)
  expect_equal(
    which(app_env$detect_alternating_signals(rep(c(1, 2), 8))),
    14:16
  )
  expect_equal(which(app_env$detect_trend_signals(c(1:5, NA, 6:11))), 12L)
  expect_false(any(app_env$detect_runs_signals(
    c(rep(2, 4), NA, rep(2, 4)),
    rep(1, 9)
  )))

  combined <- app_env$detect_run_rule_signals(
    dataset_3_values,
    rep(mean(dataset_3_values), length(dataset_3_values))
  )
  expect_true(all(combined$trend_6[13:15]))
  expect_true(all(combined$any_run_rule[13:15]))
})

test_that("recalculation uses only the pre-intervention baseline", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")
  expect_match(server_text, "emp_cl_orig <- safe_mean(data_before$value)", fixed = TRUE)
  expect_false(grepl("emp_cl_orig <- safe_mean(data$value)", server_text, fixed = TRUE))
  expect_match(server_text, "calculate_moving_ranges(data_before$value)", fixed = TRUE)
})

test_that("full-range sliders retain rows with missing provenance", {
  app <- system.file("app", package = "policycraft")
  helper_env <- new.env(parent = globalenv())
  sys.source(file.path(app, "R", "data_helpers.R"), envir = helper_env)

  source_pages <- c(3, 7, 19, NA_real_)
  expect_true(helper_env$is_full_filter_range(source_pages, c(3, 19)))
  expect_false(helper_env$is_full_filter_range(source_pages, c(3, 7)))

  dates <- as.Date(c("2012-01-01", "2025-01-01", NA))
  expect_true(helper_env$is_full_filter_range(dates, range(dates, na.rm = TRUE)))
})

test_that("series_id is the preferred chart grouping when available", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")
  expect_match(server_text, 'else if ("series_id" %in% potential_groups)', fixed = TRUE)
})

test_that("measure remains available for filtering and grouping", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl('"^measure$"', server_text, fixed = TRUE))
})

test_that("column mapping has an explicit apply action", {
  app <- system.file("app", package = "policycraft")
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, 'actionButton(\n                      "apply_column_mapping"', fixed = TRUE)
  expect_match(ui_text, "Apply mapping to app data", fixed = TRUE)
  expect_match(server_text, "observeEvent(input$apply_column_mapping", fixed = TRUE)
  expect_match(server_text, "mapping <- applied_column_mapping()", fixed = TRUE)
  expect_match(server_text, "Click Apply mapping to use it.", fixed = TRUE)
})

test_that("sequential point mappings remain usable in line charts", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl("date = standardize_to_date(date)", server_text, fixed = TRUE))
  expect_match(server_text, 'choices = c("No grouping" = "", setNames(choices, choices))', fixed = TRUE)
  expect_match(server_text, "potential_groups <- setdiff(potential_groups, mapped_source_columns)", fixed = TRUE)
})

test_that("ordinal inputs retain observation and value as their leading columns", {
  app <- system.file("app", package = "policycraft")
  app_env <- new.env(parent = globalenv())
  old_dir <- setwd(app)
  on.exit(setwd(old_dir), add = TRUE)
  sys.source("global.R", envir = app_env)
  sys.source(file.path("R", "data_helpers.R"), envir = app_env)

  input <- data.frame(observation = 1:15, value = seq(45, 59))
  result <- app_env$add_chart_columns(
    input,
    date_col_override = "observation",
    value_col_override = "value"
  )

  expect_identical(names(result)[1:2], c("observation", "value"))
  expect_equal(result$observation, 1:15)
  expect_false("Year" %in% names(result))

  forced_sequence <- app_env$add_chart_columns(
    data.frame(order = 2020:2022, result = c(5, 6, 7)),
    date_col_override = "order",
    value_col_override = "result",
    order_type = "sequence"
  )
  expect_identical(names(forced_sequence)[1:2], c("observation", "value"))
  expect_equal(forced_sequence$observation, 2020:2022)

  expect_error(
    app_env$add_chart_columns(
      input,
      date_col_override = "observation",
      value_col_override = "value",
      order_type = "date"
    ),
    "no recognizable calendar dates"
  )
})

test_that("mapping UI explicitly distinguishes dates from observation sequences", {
  app <- system.file("app", package = "policycraft")
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, '"map_order_type"', fixed = TRUE)
  expect_match(ui_text, '"Calendar dates" = "date"', fixed = TRUE)
  expect_match(ui_text, '"Observation sequence" = "sequence"', fixed = TRUE)
  expect_match(server_text, "order_type = mapping$order_type", fixed = TRUE)
})

test_that("line charts use the observation axis for ordinal mappings", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")

  expect_match(server_text, 'observation_axis <- "observation" %in% names(data)', fixed = TRUE)
  expect_match(server_text, "data$.chart_x <- if (observation_axis) data$observation else data$date", fixed = TRUE)
  expect_match(server_text, 'if (observation_axis) "Observation" else "Time Period"', fixed = TRUE)
  expect_match(server_text, "chart + ggplot2::scale_x_continuous", fixed = TRUE)
})

test_that("expectation charts use ordinal semantics for observation mappings", {
  app <- system.file("app", package = "policycraft")
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")

  expect_gte(length(gregexpr('x = axis_label_or_default(input$control_x_label, if (observation_axis) "Observation"', server_text, fixed = TRUE)[[1]]), 1L)
  expect_gte(length(gregexpr('x = axis_label_or_default(input$trended_x_label, if (observation_axis) "Observation"', server_text, fixed = TRUE)[[1]]), 1L)
  expect_match(server_text, 'if (observation_axis) " × observations<br>" else " × days<br>"', fixed = TRUE)
  expect_match(server_text, "if (!observation_axis && !is.null(trend_model)", fixed = TRUE)
  expect_match(ui_text, '"recalc_observation"', fixed = TRUE)
  expect_match(ui_text, "Select the observation where the new expectation limits begin", fixed = TRUE)
  expect_match(server_text, "recalc_point <- if (observation_axis)", fixed = TRUE)
  expect_false(grepl("recalc_enabled <- !observation_axis", server_text, fixed = TRUE))
})

test_that("numeric tested grades create separate chart lines", {
  app <- system.file("app", package = "policycraft")
  helper_env <- new.env(parent = globalenv())
  sys.source(file.path(app, "R", "data_helpers.R"), envir = helper_env)

  grade_data <- data.frame(
    date = as.Date(c("2024-01-01", "2025-01-01", "2024-01-01", "2025-01-01")),
    value = c(0.85, 0.92, 0.77, 0.75),
    tested_grade = c(3, 3, 4, 4)
  )

  grouped <- helper_env$prepare_grouped_chart_lines(grade_data, "tested_grade")

  expect_true(grouped$grouped)
  expect_setequal(levels(grouped$data$.plot_group), c("3", "4"))
  expect_equal(nrow(grouped$labels), 2L)
  expect_true(all(grouped$labels$.label_date > grouped$labels$date))
})

test_that("ungrouped chart labels retain the columns required by ggplot", {
  app <- system.file("app", package = "policycraft")
  helper_env <- new.env(parent = globalenv())
  sys.source(file.path(app, "R", "data_helpers.R"), envir = helper_env)

  chart_data <- data.frame(
    date = as.Date("1970-01-01") + 0:14,
    value = seq_len(15)
  )
  prepared <- helper_env$prepare_grouped_chart_lines(chart_data, "")

  expect_false(prepared$grouped)
  expect_equal(nrow(prepared$labels), 0L)
  expect_true(".label_date" %in% names(prepared$labels))
  expect_s3_class(prepared$labels$.label_date, "Date")
})

test_that("line-end labels receive a visible date-aware horizontal offset", {
  app <- system.file("app", package = "policycraft")
  helper_env <- new.env(parent = globalenv())
  sys.source(file.path(app, "R", "data_helpers.R"), envir = helper_env)

  labels <- data.frame(date = as.Date(c("2025-01-01", "2025-01-01")))
  all_dates <- as.Date(c("2016-01-01", "2025-01-01"))
  shifted <- helper_env$offset_line_end_labels(labels, all_dates)

  expected_days <- ceiling(as.numeric(diff(range(all_dates))) * 0.035)
  expect_equal(shifted$.label_date, labels$date + expected_days)
  expect_true(all(shifted$.label_date > labels$date))
})

test_that("line-chart grouping uses filtered data and accepts numeric groups", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")
  helper_text <- paste(
    readLines(file.path(app, "R", "data_helpers.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    server_text,
    "output$grouping_info <- renderUI({\n    data <- filtered_data()",
    fixed = TRUE
  )
  expect_false(grepl("!is.numeric(data[[group_var]])", server_text, fixed = TRUE))
  expect_false(grepl("!is.numeric(data[[group_var]])", helper_text, fixed = TRUE))
})

test_that("canonical tools share filtered observations without re-aggregation", {
  app <- system.file("app", package = "policycraft")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")
  helper_text <- paste(readLines(file.path(app, "R", "data_helpers.R"), warn = FALSE), collapse = "\n")

  expect_match(server_text, "analytical_data <- reactive({", fixed = TRUE)
  expect_match(server_text, "analytical_data <- reactive({\n    filtered_data()", fixed = TRUE)
  expect_gte(length(gregexpr("data <- analytical_data()", server_text, fixed = TRUE)[[1]]), 4L)
  expect_false(grepl("summarise_chart_values", helper_text, fixed = TRUE))
  expect_false(grepl("enable_chart_aggregation", server_text, fixed = TRUE))
})

test_that("canonical exploration keeps filters and grouping available", {
  app <- system.file("app", package = "policycraft")
  ui_text <- paste(readLines(file.path(app, "ui.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(file.path(app, "server.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_text, 'condition = "output.data_uploaded"', fixed = TRUE)
  expect_match(ui_text, 'id = "filter_controls"', fixed = TRUE)
  expect_match(server_text, "Canonical explorer", fixed = TRUE)
  expect_match(ui_text, "Observation-level lineage", fixed = TRUE)
  expect_match(ui_text, "unresolved semantic warnings", fixed = TRUE)
  expect_false(grepl("if (!is.null(canonical_bundle())) return()\n\n    # Get column names", server_text, fixed = TRUE))
})
