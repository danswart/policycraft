test_that("installed Shiny application is complete", {
  app <- system.file("app", package = "policycraft")
  expect_true(nzchar(app))
  expect_true(all(file.exists(file.path(app, c("global.R", "ui.R", "server.R")))))
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
