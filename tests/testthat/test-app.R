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
  expect_true(all(c("launch_browser", "host", "port", "...") %in%
                    names(formals(launch_longitudinal))))
})

test_that("legacy application-wide names are deprecated", {
  exports <- getNamespaceExports("policycraft")
  expect_true(all(c("run_policycraft", "run_app") %in% exports))
  expect_match(paste(deparse(body(run_policycraft)), collapse = " "), ".Deprecated", fixed = TRUE)
  expect_match(paste(deparse(body(run_app)), collapse = " "), ".Deprecated", fixed = TRUE)
})
