test_that("expectation chart uses ordered moving ranges", {
  x <- data.frame(year = 2020:2022, measure = c(1, 4, 2))
  result <- expectation_chart_data(x, year, measure)
  expect_equal(result$moving_range, c(NA, 3, 2))
  expect_equal(attr(result, "expectation")$average_moving_range, 2.5)
})

test_that("natural bounds constrain expectation limits", {
  x <- data.frame(year = 2020:2023, measure = c(1, 2, 1, 2))
  result <- expectation_chart_data(x, year, measure, lower_bound = 0)
  expect_gte(result$lower_limit[1], 0)
})

test_that("runs begin at the declared threshold", {
  signal <- detect_runs(c(rep(2, 9), rep(0, 8)), center = 1)
  expect_equal(which(signal), c(8L, 9L, 17L))
})

test_that("values on the center and missing values interrupt runs", {
  signal <- detect_runs(c(rep(2, 4), 1, rep(2, 4), NA, rep(2, 8)), center = 1)
  expect_equal(which(signal), 18L)
})

test_that("longitudinal summary remains neutral and reproducible", {
  x <- data.frame(year = 2015:2024, measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21))
  result <- longitudinal_summary(x, year, measure)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$observations, 10)
  expect_type(result$investigate, "logical")
})

test_that("expectation chart plotting returns a standardized, inspectable ggplot", {
  x <- data.frame(year = 2015:2024, measure = c(10, 11, 9, 12, 10, 11, 10, 19, 20, 21))
  result <- plot_expectation_chart(x, year, measure)

  expect_s3_class(result, "ggplot")
  expect_true(all(c("run_signal", "signal") %in% names(result$data)))
  expect_equal(result$labels$title, "Expectation Chart")
  expect_true(any(result$data$signal))
  expect_s3_class(result$theme$plot.title, "element_markdown")
  expect_equal(result$theme$plot.title$colour, "darkgreen")
  expect_equal(result$theme$plot.title$size, 21)
})

test_that("expectation chart plotting accepts strings and calculation options", {
  x <- data.frame(year = 2020:2023, measure = c(1, 2, 1, 2))
  result <- plot_expectation_chart(
    x, "year", "measure", run_length = NULL, lower_bound = 0,
    title = "A local title"
  )

  expect_equal(result$labels$title, "A local title")
  expect_false(any(result$data$run_signal))
  expect_gte(result$data$lower_limit[1], 0)
})

test_that("expectation chart plotting validates run length", {
  x <- data.frame(year = 2020:2022, measure = c(1, 2, 3))
  expect_error(plot_expectation_chart(x, year, measure, run_length = 1), "at least 2")
})
