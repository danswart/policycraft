test_that("installed Shiny application is complete", {
  app <- system.file("app", package = "policycraft")
  expect_true(nzchar(app))
  expect_true(all(file.exists(file.path(app, c("global.R", "ui.R", "server.R")))))
})

test_that("longitudinal tool has a tool-specific public interface", {
  expect_true(is.function(launch_longitudinal))
  expect_identical(load_longitudinal, launch_longitudinal)
  expect_true(all(c("launch_browser", "host", "port", "...") %in%
                    names(formals(launch_longitudinal))))
})

test_that("legacy application-wide names are deprecated", {
  exports <- getNamespaceExports("policycraft")
  expect_true(all(c("run_policycraft", "run_app") %in% exports))
  expect_match(paste(deparse(body(run_policycraft)), collapse = " "), ".Deprecated", fixed = TRUE)
  expect_match(paste(deparse(body(run_app)), collapse = " "), ".Deprecated", fixed = TRUE)
})
