limit_test_environment <- function() {
  app <- system.file("app", package = "policycraft")
  env <- new.env(parent = globalenv())
  old <- setwd(app)
  on.exit(setwd(old))
  sys.source("global.R", envir = env)
  sys.source("R/data_helpers.R", envir = env)
  sys.source("R/chart_download_module.R", envir = env)
  sys.source("R/limit_helpers.R", envir = env)
  sys.source("server.R", envir = env)
  env
}

test_that("omissions preserve values and never bridge moving ranges or lag pairs", {
  env <- limit_test_environment()
  x <- c(10, 12, 100, 15, 19)
  result <- env$limit_estimate(x, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(result$center, 14)
  expect_equal(result$average_moving_range, 3) # Only 10->12 and 15->19.
  expect_equal(result$upper, 14 + 9 / 1.128)
  expect_equal(x, c(10, 12, 100, 15, 19))
  expect_error(env$limit_estimate(x, rep(TRUE, 5)), "At least two")
  expect_error(env$limit_estimate(x, c(FALSE, TRUE, FALSE, TRUE, FALSE)), "adjacent")
  expect_equal(env$limit_estimate(c(10, 12, NA, 15, 19))$average_moving_range, 3)
  fallback <- env$limit_estimate(c(1, 2, 99, 4, 5), c(FALSE, FALSE, TRUE, FALSE, FALSE), TRUE)
  expect_true(fallback$sd_fallback)
  expect_equal(fallback$sigma, sd(c(1, 2, 4, 5)))
  y <- c(2, 5, 99, 3, 8, 4, 7)
  excluded <- seq_along(y) == 3
  adjusted <- env$limit_estimate(y, excluded, TRUE)
  expect_equal(adjusted$r, cor(c(2, 3, 8, 4), c(5, 8, 4, 7)))
  expect_equal(adjusted$sigma, mean(c(3, 5, 4, 3)) / (1.128 * sqrt(1 - adjusted$r^2)))
})

test_that("Shiny exclusions propagate to charts, phases, diagnostics and editable exports", {
  env <- limit_test_environment()
  upload <- tempfile(fileext = ".csv")
  values <- c(10, 12, 100, 15, 19, 20, 21, 22, 24, 25)
  write.csv(data.frame(observation = 1:10, value = values), upload, row.names = FALSE)
  on.exit(unlink(upload))
  withCallingHandlers(shiny::testServer(env$server, {
    session$setInputs(header = TRUE, rows_display = 30,
                      format_as_percentage = FALSE, grouping_var = "",
                      enable_recalc = FALSE, use_autocorr_modifier = FALSE,
                      file = data.frame(name = "points.csv", datapath = upload,
                                        size = file.info(upload)$size, type = "text/csv"))
    initial <- control_plot()
    expect_equal(initial$data$emp_cl, rep(mean(values), 10))
    id <- expectation_data()$.limit_id[3]
    session$setInputs(limit_exclusions = id)
    chart <- control_plot()
    expect_equal(chart$data$value, values)
    expect_equal(chart$data$emp_cl, rep(mean(values[-3]), 10))
    expect_true(chart$data$.limit_excluded[3])
    expect_true(chart$data$sigma_signals[3])
    expect_equal(control_diagnostics()$Centerline_Used, chart$data$emp_cl)
    expect_match(chart$labels$caption, "Excluded from estimation")
    expect_s3_class(suppressWarnings(ggplot2::ggplot_build(chart)), "ggplot_built")

    trend <- trended_plot()
    model <- lm(values[-3] ~ I((1:10)[-3] - 1))
    expect_equal(trend$data$trended_cl, unname(coef(model)[1] + coef(model)[2] * (0:9)))
    for (name in c("expectation_chart", "trended_expectation_chart")) {
      original <- if (name == "expectation_chart") chart else trend
      script <- ggplot_code_lines(original, name)
      restored <- new.env(parent = globalenv())
      eval(parse(text = script), restored)
      expect_equal(restored$chart_data$.limit_excluded, original$data$.limit_excluded)
      if (name == "trended_expectation_chart") {
        expect_equal(restored$chart_data$trended_cl, original$data$trended_cl)
        expect_equal(restored$chart_data$trended_ucl, original$data$trended_ucl)
      }
    }
    session$setInputs(enable_recalc = TRUE, recalc_observation = "6")
    split <- control_plot()
    expect_equal(split$data$emp_cl_orig, rep(mean(values[c(1, 2, 4, 5)]), 10))
    expect_equal(split$data$emp_cl_recalc, rep(mean(values[6:10]), 10))
    expect_equal(control_diagnostics()$Centerline_Used,
                 c(rep(mean(values[c(1, 2, 4, 5)]), 5), rep(mean(values[6:10]), 5)))
    session$setInputs(limit_exclusions = expectation_data()$.limit_id[c(3, 8, 9, 10)])
    expect_true(control_estimates()$frozen)
    expect_equal(control_plot()$data$emp_cl_orig, control_plot()$data$emp_cl_recalc)
    session$setInputs(limit_exclusions = expectation_data()$.limit_id)
    expect_error(control_plot(), "At least two")
    expect_error(trended_plot(), "At least three")
    session$setInputs(limit_exclusions = character(), enable_recalc = FALSE)
    expect_equal(control_plot()$data$emp_cl, initial$data$emp_cl)

    # Filtered positions do not change source IDs or original data.
    session$setInputs(filter_observation = c(3, 10), limit_exclusions = id)
    expect_identical(expectation_data()$.limit_id[1], id)
    expect_true(expectation_data()$.limit_excluded[1])
    expect_false(".limit_id" %in% names(filtered_data()))
    session$setInputs(clear_limit_exclusions = 1)
    expect_false(any(expectation_data()$.limit_excluded))
    session$setInputs(limit_exclusions = id)
    session$setInputs(filter_observation = c(4, 10))
    session$setInputs(filter_observation = c(1, 10))
    expect_false(any(expectation_data()$.limit_excluded))
    session$setInputs(limit_exclusions = id)
    old_ids <- expectation_data()$.limit_id
    session$setInputs(map_date_col = "observation", map_value_col = "value",
                      map_order_type = "sequence", apply_column_mapping = 1)
    expect_false(any(expectation_data()$.limit_id %in% old_ids))
    expect_false(any(expectation_data()$.limit_excluded))

    # A new calendar upload resets selection and supports dated phase boundaries.
    dated_upload <- tempfile(fileext = ".csv")
    dates <- as.Date("2024-01-01") + 0:9
    write.csv(data.frame(date = dates, value = values), dated_upload, row.names = FALSE)
    session$setInputs(limit_exclusions = expectation_data()$.limit_id[3])
    session$setInputs(file = data.frame(name = "dated.csv", datapath = dated_upload,
                                       size = file.info(dated_upload)$size, type = "text/csv"))
    expect_false(any(expectation_data()$.limit_excluded))
    session$setInputs(limit_exclusions = expectation_data()$.limit_id[3],
                      enable_recalc = TRUE, recalc_date = dates[6])
    expect_equal(control_plot()$data$emp_cl_orig, rep(mean(values[c(1, 2, 4, 5)]), 10))
    expect_equal(control_plot()$data$emp_cl_recalc, rep(mean(values[6:10]), 10))
    unlink(dated_upload)
  }), warning = function(w) {
    if (grepl("All aesthetics have length 1|Removed .* rows containing missing values|Unknown or uninitialised column", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
})
