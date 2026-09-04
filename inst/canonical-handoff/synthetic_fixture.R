source(file.path("R", "canonical_staar.R"))

fixture_observations <- data.frame(
  observation_id = c("obs_source_num", "obs_source_den", "obs_derived_rate"),
  observation_date = as.Date(rep("2024-06-30", 3)),
  entity_id = "entity_demo_001", geographic_level = "district",
  subject = "Mathematics", tested_grade = c("3", "3", "3-8"),
  student_group = "all_students", administration = c("Spring 2024", "Spring 2024", "annual"),
  measure_id = c("passing_count", "tests_taken_count", "passing_rate"),
  unit = c("students", "students", "proportion"), value = c(72, 100, 0.72),
  reporting_status = c("reported", "reported", "derived"),
  source_id = c("synthetic_source", "synthetic_source", "derived"),
  series_id = c("series_num", "series_den", "series_rate"),
  derivation_id = c(NA, NA, "ratio_of_sums_v1")
)

fixture <- list(
  schema_version = "canonical_longitudinal_bundle/0.1.0",
  observations = fixture_observations,
  measures = data.frame(measure_id = c("passing_count", "tests_taken_count", "passing_rate"), unit = c("students", "students", "proportion")),
  derivations = data.frame(derivation_id = "ratio_of_sums_v1", definition = "sum(passing_count) / sum(tests_taken_count)"),
  lineage = data.frame(derived_observation_id = "obs_derived_rate", source_observation_id = c("obs_source_num", "obs_source_den"), derivation_id = "ratio_of_sums_v1"),
  screening_report = data.frame(proposal_type = "fixture", validation_status = "synthetic"),
  validation_results = data.frame(check_id = "fixture_rate", status = "pass")
)

saveRDS(fixture, file.path("policycraft_handoff", "synthetic_canonical_bundle.rds"), version = 3)
