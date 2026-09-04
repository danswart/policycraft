fixture_path <- function() {
  system.file("canonical-handoff", "synthetic_canonical_bundle.rds", package = "policycraft")
}

test_that("supplied synthetic bundle is recognized and validated", {
  raw <- readRDS(fixture_path())
  expect_true(is_canonical_bundle(raw))
  bundle <- load_canonical_bundle(fixture_path())
  expect_s3_class(bundle, "policycraft_canonical_bundle")
  expect_identical(bundle$schema_version, "canonical_longitudinal_bundle/0.1.0")
})

test_that("ordinary frames remain noncanonical", {
  flat <- data.frame(year = 2020:2022, value = 1:3)
  expect_false(is_canonical_bundle(flat))
  expect_error(validate_canonical_bundle(flat), "expected a named list")
  expect_equal(as_policy_data(flat, year, value)$value, 1:3)
})

test_that("malformed, incomplete, and unsupported bundles fail clearly", {
  bundle <- readRDS(fixture_path())
  unsupported <- bundle
  unsupported$schema_version <- "canonical_longitudinal_bundle/9.0.0"
  expect_error(validate_canonical_bundle(unsupported), "unsupported `schema_version`")

  missing_table <- bundle
  missing_table$lineage <- NULL
  expect_error(validate_canonical_bundle(missing_table), "missing required table.*lineage")

  missing_column <- bundle
  missing_column$observations$series_id <- NULL
  expect_error(validate_canonical_bundle(missing_column), "missing required column.*series_id")
})

test_that("duplicate IDs and invalid registry references fail", {
  bundle <- readRDS(fixture_path())
  duplicate <- bundle
  duplicate$observations$observation_id[2] <- duplicate$observations$observation_id[1]
  expect_error(validate_canonical_bundle(duplicate), "duplicate `observation_id`")

  bad_measure <- bundle
  bad_measure$observations$measure_id[1] <- "not_registered"
  expect_error(validate_canonical_bundle(bad_measure), "unknown `measure_id`")

  bad_derivation <- bundle
  bad_derivation$observations$derivation_id[3] <- "not_registered"
  expect_error(validate_canonical_bundle(bad_derivation), "unknown `derivation_id`")
})

test_that("orphaned lineage records fail", {
  bundle <- readRDS(fixture_path())
  bundle$lineage$source_observation_id[1] <- "missing_observation"
  expect_error(validate_canonical_bundle(bundle), "orphaned `source_observation_id`")
})

test_that("one series resolves without aggregation and retains metadata", {
  bundle <- load_canonical_bundle(fixture_path())
  available <- list_canonical_series(bundle)
  expect_setequal(available$series_id, c("series_num", "series_den", "series_rate"))

  resolved <- resolve_canonical_series(bundle, "series_rate")
  expect_s3_class(resolved, "policycraft_resolved_series")
  expect_identical(resolved$observations$value, 0.72)
  expect_identical(resolved$observations$reporting_status, "derived")
  expect_identical(resolved$observations$derivation_id, "ratio_of_sums_v1")
  expect_equal(nrow(resolved$lineage), 2L)
  expect_equal(nrow(resolved$source_observations), 2L)
  expect_error(resolve_canonical_series(bundle, character()), "Exactly one")
})

test_that("expectation input rejects zero or multiple series", {
  bundle <- load_canonical_bundle(fixture_path())
  expect_error(canonical_expectation_input(bundle$observations[0, ]), "found 0")
  expect_error(canonical_expectation_input(bundle$observations), "found 3")
  resolved <- resolve_canonical_series(bundle, "series_rate")
  expect_identical(canonical_expectation_input(resolved), resolved$observations)
})

test_that("status gaps and exact canonical values are preserved", {
  bundle <- readRDS(fixture_path())
  rate <- bundle$observations[rep(3, 3), , drop = FALSE]
  rate$observation_id <- paste0("rate_", 1:3)
  rate$observation_date <- as.Date(c("2019-06-30", "2020-06-30", "2021-06-30"))
  rate$value <- c(0.60, NA, 0.75)
  rate$reporting_status <- c("reported", "no_administration", "reported")
  rate$series_id <- "series_rate"
  bundle$observations <- rbind(bundle$observations[1:2, ], rate)
  bundle$lineage <- bundle$lineage[0, ]
  bundle$observations$derivation_id[3:5] <- NA_character_
  bundle$measures <- bundle$measures
  resolved <- resolve_canonical_series(bundle, "series_rate")
  expect_identical(resolved$observations$value, c(0.60, NA, 0.75))
  expect_identical(resolved$observations$reporting_status, c("reported", "no_administration", "reported"))
  expect_equal(as.numeric(diff(resolved$observations$observation_date)), c(366, 365))
})

test_that("canonical ratio values are never averaged or re-aggregated", {
  bundle <- load_canonical_bundle(fixture_path())
  resolved <- resolve_canonical_series(bundle, "series_rate")
  rows <- resolved$observations
  expect_identical(nrow(rows), 1L)
  expect_identical(rows$value, bundle$observations$value[bundle$observations$series_id == "series_rate"])
  expect_false(any(c("mean", "average", "summarise") %in% names(rows)))
})
