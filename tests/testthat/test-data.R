test_that("policy_dates handles years, fiscal years, and explicit dates", {
  expect_equal(
    policy_dates(c("2019", "FY20", "2021-06-30")),
    as.Date(c("2019-01-01", "2020-08-31", "2021-06-30"))
  )
})

test_that("policy_numbers handles common policy data formats", {
  expect_equal(
    policy_numbers(c("1,240", "53.2%", "score: -4.5", "N/A")),
    c(1240, 53.2, -4.5, NA_real_)
  )
})

test_that("as_policy_data preserves original fields", {
  x <- data.frame(year = 2020:2021, measure = c("10", "12"), group = c("A", "B"))
  result <- as_policy_data(x, year, measure)
  expect_equal(names(result)[1:3], c("date", "value", "year"))
  expect_equal(result$group, c("A", "B"))
})
