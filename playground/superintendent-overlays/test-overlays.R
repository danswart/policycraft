# Lightweight checks for the playground overlay data and ggplot layers.
project_root <- normalizePath(getwd(), mustWork = TRUE)
pkgload::load_all(project_root, quiet = TRUE)
app_dir <- file.path(project_root, "playground", "superintendent-overlays", "app")
setwd(app_dir)
source("global.R")

stopifnot(
  nrow(SCUC_SUPERINTENDENT_TERMS) == 5L,
  identical(
    SCUC_SUPERINTENDENT_TERMS$superintendent,
    c(
      "Dr. Byron P. Steele II", "Dr. Edward \"Ed\" West",
      "Dr. Greg Gibson", "Dr. Clark C. Ealy", "Mrs. Paige A. Meloni"
    )
  ),
  length(superintendent_date_layers(as.Date(c("1995-01-01", "2024-12-31")))) == 2L,
  length(superintendent_date_layers(as.Date(c("2005-01-01", "2008-01-01")))) == 2L,
  length(superintendent_date_layers(as.Date(c("1900-01-01", "1901-01-01")))) == 0L
)

annual_layers <- superintendent_date_layers(as.Date(paste0(1995:2024, "-01-01")))
stopifnot(
  nrow(annual_layers[[1]]$data) == 5L,
  any(grepl("Paige A. Meloni", annual_layers[[1]]$data$label))
)

cohort_example <- data.frame(grade = 3:8, year = 2018:2023)
stopifnot(length(superintendent_cohort_layers(cohort_example)) == 2L)

message("Superintendent overlay playground checks passed.")
