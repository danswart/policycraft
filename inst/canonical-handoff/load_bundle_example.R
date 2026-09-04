load_canonical_bundle <- function(path) {
  x <- readRDS(path)
  required <- c("observations", "measures", "derivations", "lineage", "screening_report", "validation_results")
  if (!is.list(x) || !all(required %in% names(x))) {
    stop("Not a supported canonical longitudinal bundle")
  }
  if (!identical(x$schema_version, "canonical_longitudinal_bundle/0.1.0")) {
    stop("Unsupported canonical bundle schema: ", x$schema_version)
  }
  x
}

resolve_one_series <- function(bundle, series_id) {
  rows <- bundle$observations[bundle$observations$series_id == series_id, , drop = FALSE]
  if (!nrow(rows)) stop("Series not found")
  if (length(unique(rows$series_id)) != 1L) stop("Exactly one series is required")
  rows[order(rows$observation_date), , drop = FALSE]
}
