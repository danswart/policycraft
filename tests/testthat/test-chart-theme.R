test_that("shared chart theme preserves the app visual identity", {
  theme <- policycraft_chart_theme()

  expect_s3_class(theme, "theme")
  expect_s3_class(theme$plot.title, "element_markdown")
  expect_equal(theme$plot.title$colour, "darkgreen")
  expect_equal(theme$plot.subtitle$colour, "darkgreen")
  expect_equal(theme$plot.caption$colour, "darkblue")
  expect_equal(theme$text$colour, "royalblue")
})
