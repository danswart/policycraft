#' Canonical longitudinal bundle support
#'
#' A canonical longitudinal bundle is a named R list with a supported
#' `schema_version` and six data-frame components: `observations`, `measures`,
#' `derivations`, `lineage`, `screening_report`, and `validation_results`.
#' These helpers validate that contract and resolve one registry-defined
#' analytical series without filtering, grouping, or aggregating its rows.
#'
#' Version `canonical_longitudinal_bundle/0.1.0` requires the core fields used
#' by the canonical handoff. Producers may include the additional fields in the
#' published schema; policycraft preserves them and validates their foreign keys
#' when applicable.
#'
#' @param x An object to inspect or validate.
#' @param path Path to an RDS file.
#' @param error If `TRUE`, stop with all validation problems. If `FALSE`, return
#'   a logical value with the problems in a `problems` attribute.
#' @return `is_canonical_bundle()` returns one logical value.
#'   `validate_canonical_bundle()` and `load_canonical_bundle()` return the
#'   validated bundle with class `policycraft_canonical_bundle`.
#' @name canonical_bundle
NULL

canonical_schema_version <- function() "canonical_longitudinal_bundle/0.1.0"

canonical_table_columns <- function() {
  list(
    observations = c(
      "observation_id", "observation_date", "entity_id", "geographic_level",
      "subject", "tested_grade", "student_group", "administration",
      "measure_id", "unit", "value", "reporting_status", "source_id",
      "series_id", "derivation_id"
    ),
    measures = c("measure_id", "unit"),
    derivations = c("derivation_id", "definition"),
    lineage = c(
      "derived_observation_id", "source_observation_id", "derivation_id"
    ),
    screening_report = c("proposal_type", "validation_status"),
    validation_results = c("check_id", "status")
  )
}

canonical_candidate <- function(x) {
  is.list(x) && !is.data.frame(x) && !is.null(names(x)) &&
    any(c("schema_version", names(canonical_table_columns())) %in% names(x))
}

#' @rdname canonical_bundle
#' @export
is_canonical_bundle <- function(x) {
  if (!canonical_candidate(x)) return(FALSE)
  result <- validate_canonical_bundle(x, error = FALSE)
  isTRUE(result)
}

canonical_problem <- function(table, message) {
  if (nzchar(table)) paste0("`", table, "`: ", message) else message
}

#' @rdname canonical_bundle
#' @export
validate_canonical_bundle <- function(x, error = TRUE) {
  problems <- character()
  add <- function(table = "", message) {
    problems <<- c(problems, canonical_problem(table, message))
  }

  if (!is.list(x) || is.data.frame(x) || is.null(names(x))) {
    add(message = "expected a named list, not an ordinary data frame or unnamed object.")
  } else {
    version <- x$schema_version
    if (is.null(version) || !is.character(version) || length(version) != 1L ||
        is.na(version) || !nzchar(version)) {
      add(message = "`schema_version` must be one non-empty character value.")
    } else if (!identical(version, canonical_schema_version())) {
      add(message = paste0(
        "unsupported `schema_version` ", dQuote(version), "; supported version is ",
        dQuote(canonical_schema_version()), "."
      ))
    }

    schemas <- canonical_table_columns()
    missing_tables <- setdiff(names(schemas), names(x))
    if (length(missing_tables)) {
      add(message = paste0("missing required table(s): ", paste(missing_tables, collapse = ", "), "."))
    }
    for (table in intersect(names(schemas), names(x))) {
      value <- x[[table]]
      if (!is.data.frame(value)) {
        add(table, "must be a data frame.")
        next
      }
      missing_columns <- setdiff(schemas[[table]], names(value))
      if (length(missing_columns)) {
        add(table, paste0("missing required column(s): ", paste(missing_columns, collapse = ", "), "."))
      }
    }

    if ("observations" %in% names(x) && is.data.frame(x$observations)) {
      obs <- x$observations
      if ("observation_id" %in% names(obs)) {
        ids <- as.character(obs$observation_id)
        if (anyNA(ids) || any(!nzchar(ids))) add("observations", "`observation_id` cannot be missing or blank.")
        duplicates <- unique(ids[duplicated(ids) & !is.na(ids)])
        if (length(duplicates)) add("observations", paste0("duplicate `observation_id`: ", paste(duplicates, collapse = ", "), "."))
      }
      if ("series_id" %in% names(obs)) {
        series <- as.character(obs$series_id)
        if (anyNA(series) || any(!nzchar(series))) add("observations", "`series_id` cannot be missing or blank.")
      }
      if ("observation_date" %in% names(obs) && !inherits(obs$observation_date, "Date")) {
        add("observations", "`observation_date` must have class Date.")
      }
      if ("value" %in% names(obs) && !is.numeric(obs$value)) {
        add("observations", "`value` must be numeric (missing values are allowed).")
      }
    }

    if (all(c("observations", "measures") %in% names(x)) &&
        is.data.frame(x$observations) && is.data.frame(x$measures) &&
        all(c("measure_id", "unit") %in% names(x$measures)) &&
        all(c("measure_id", "unit") %in% names(x$observations))) {
      measure_ids <- as.character(x$measures$measure_id)
      if (anyNA(measure_ids) || any(!nzchar(measure_ids)) || anyDuplicated(measure_ids)) {
        add("measures", "`measure_id` must be non-missing and unique.")
      }
      bad <- setdiff(unique(as.character(x$observations$measure_id)), measure_ids)
      if (length(bad)) add("observations", paste0("unknown `measure_id`: ", paste(bad, collapse = ", "), "."))
      registry_units <- stats::setNames(as.character(x$measures$unit), measure_ids)
      expected <- unname(registry_units[as.character(x$observations$measure_id)])
      mismatch <- !is.na(expected) & as.character(x$observations$unit) != expected
      if (any(mismatch, na.rm = TRUE)) {
        add("observations", "`unit` conflicts with the measure registry for one or more rows.")
      }
      if ("denominator_measure_id" %in% names(x$measures)) {
        refs <- as.character(x$measures$denominator_measure_id)
        refs <- refs[!is.na(refs) & nzchar(refs)]
        bad <- setdiff(unique(refs), measure_ids)
        if (length(bad)) add("measures", paste0("unknown `denominator_measure_id`: ", paste(bad, collapse = ", "), "."))
      }
    }

    if (all(c("observations", "derivations") %in% names(x)) &&
        is.data.frame(x$observations) && is.data.frame(x$derivations) &&
        "derivation_id" %in% names(x$observations) && "derivation_id" %in% names(x$derivations)) {
      derivation_ids <- as.character(x$derivations$derivation_id)
      if (anyNA(derivation_ids) || any(!nzchar(derivation_ids)) || anyDuplicated(derivation_ids)) {
        add("derivations", "`derivation_id` must be non-missing and unique.")
      }
      refs <- as.character(x$observations$derivation_id)
      refs <- refs[!is.na(refs) & nzchar(refs)]
      bad <- setdiff(unique(refs), derivation_ids)
      if (length(bad)) add("observations", paste0("unknown `derivation_id`: ", paste(bad, collapse = ", "), "."))
    }

    if (all(c("observations", "derivations", "lineage") %in% names(x)) &&
        is.data.frame(x$observations) && is.data.frame(x$derivations) && is.data.frame(x$lineage) &&
        all(c("observation_id", "derivation_id") %in% names(x$observations)) &&
        "derivation_id" %in% names(x$derivations) &&
        all(c("derived_observation_id", "source_observation_id", "derivation_id") %in% names(x$lineage))) {
      observation_ids <- as.character(x$observations$observation_id)
      derivation_ids <- as.character(x$derivations$derivation_id)
      for (column in c("derived_observation_id", "source_observation_id")) {
        bad <- setdiff(unique(as.character(x$lineage[[column]])), observation_ids)
        if (length(bad)) add("lineage", paste0("orphaned `", column, "`: ", paste(bad, collapse = ", "), "."))
      }
      bad <- setdiff(unique(as.character(x$lineage$derivation_id)), derivation_ids)
      if (length(bad)) add("lineage", paste0("unknown `derivation_id`: ", paste(bad, collapse = ", "), "."))
      derived_index <- match(as.character(x$lineage$derived_observation_id), observation_ids)
      observed_derivation <- as.character(x$observations$derivation_id[derived_index])
      mismatch <- !is.na(derived_index) & !is.na(observed_derivation) &
        observed_derivation != as.character(x$lineage$derivation_id)
      if (any(mismatch)) add("lineage", "`derivation_id` does not match its derived observation.")
    }
  }

  problems <- unique(problems)
  if (length(problems)) {
    if (isTRUE(error)) {
      stop(paste(c("Invalid canonical longitudinal bundle:", paste0("- ", problems)), collapse = "\n"), call. = FALSE)
    }
    result <- FALSE
    attr(result, "problems") <- problems
    return(result)
  }
  if (!isTRUE(error)) return(TRUE)
  class(x) <- unique(c("policycraft_canonical_bundle", class(x)))
  x
}

#' @rdname canonical_bundle
#' @export
load_canonical_bundle <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be one non-empty file path.", call. = FALSE)
  }
  validate_canonical_bundle(readRDS(path))
}

#' List analytical series in a canonical bundle
#'
#' Returns one row per `series_id`, with the defining observation dimensions,
#' valid period, measure definition, unit, derivation definition, and row count.
#' Fields that are absent from a compatible registry are returned as `NA`.
#'
#' @param bundle A canonical bundle accepted by `validate_canonical_bundle()`.
#' @return A data frame with one row per series.
#' @export
list_canonical_series <- function(bundle) {
  bundle <- validate_canonical_bundle(bundle)
  obs <- bundle$observations
  dimensions <- intersect(
    c("series_id", "entity_id", "entity_name", "geographic_level", "subject",
      "tested_grade", "student_group", "administration", "measure_id", "unit"),
    names(obs)
  )
  split_rows <- split(seq_len(nrow(obs)), as.character(obs$series_id), drop = TRUE)
  one <- function(index) {
    rows <- obs[index, , drop = FALSE]
    out <- lapply(dimensions, function(column) {
      values <- unique(as.character(rows[[column]][!is.na(rows[[column]])]))
      if (length(values) == 1L) values else paste(values, collapse = " | ")
    })
    names(out) <- dimensions
    out$valid_start <- min(rows$observation_date, na.rm = TRUE)
    out$valid_end <- max(rows$observation_date, na.rm = TRUE)
    out$n_observations <- nrow(rows)
    out$derivation_id <- {
      values <- unique(as.character(rows$derivation_id[!is.na(rows$derivation_id)]))
      if (length(values)) paste(values, collapse = " | ") else NA_character_
    }
    as.data.frame(out, stringsAsFactors = FALSE)
  }
  result <- do.call(rbind, lapply(split_rows, one))
  rownames(result) <- NULL
  measure_columns <- intersect(c("measure_id", "meaning", "role", "aggregation_rule", "denominator_measure_id"), names(bundle$measures))
  result <- merge(result, bundle$measures[measure_columns], by = "measure_id", all.x = TRUE, sort = FALSE)
  if ("derivation_id" %in% names(result)) {
    derivation_columns <- intersect(c("derivation_id", "definition", "valid_start_year", "valid_end_year", "missing_rule", "suppression_rule"), names(bundle$derivations))
    result <- merge(result, bundle$derivations[derivation_columns], by = "derivation_id", all.x = TRUE, sort = FALSE)
  }
  result[match(names(split_rows), result$series_id), , drop = FALSE]
}

#' Resolve exactly one canonical analytical series
#'
#' The resolver subsets observations exactly once by `series_id`, orders those
#' unchanged rows by `observation_date`, and attaches matching registry,
#' lineage, validation, and screening metadata. It never aggregates values.
#'
#' @param bundle A canonical bundle accepted by `validate_canonical_bundle()`.
#' @param series_id Exactly one available series identifier.
#' @return A `policycraft_resolved_series` list. Its `observations` member is the
#'   exact resolved analytical input.
#' @export
resolve_canonical_series <- function(bundle, series_id) {
  bundle <- validate_canonical_bundle(bundle)
  if (!is.character(series_id) || length(series_id) != 1L || is.na(series_id) || !nzchar(series_id)) {
    stop("Exactly one non-empty `series_id` must be selected.", call. = FALSE)
  }
  index <- which(as.character(bundle$observations$series_id) == series_id)
  if (!length(index)) stop("Unknown `series_id`: ", series_id, ".", call. = FALSE)
  rows <- bundle$observations[index, , drop = FALSE]
  rows <- rows[order(rows$observation_date, seq_len(nrow(rows)), na.last = TRUE), , drop = FALSE]
  measure_ids <- unique(as.character(rows$measure_id))
  derivation_ids <- unique(as.character(rows$derivation_id[!is.na(rows$derivation_id)]))
  lineage <- bundle$lineage[bundle$lineage$derived_observation_id %in% rows$observation_id, , drop = FALSE]
  source_ids <- unique(as.character(lineage$source_observation_id))
  source_observations <- bundle$observations[bundle$observations$observation_id %in% source_ids, , drop = FALSE]
  structure(list(
    series_id = series_id,
    observations = rows,
    definition = list_canonical_series(bundle)[list_canonical_series(bundle)$series_id == series_id, , drop = FALSE],
    measures = bundle$measures[bundle$measures$measure_id %in% measure_ids, , drop = FALSE],
    derivations = bundle$derivations[bundle$derivations$derivation_id %in% derivation_ids, , drop = FALSE],
    lineage = lineage,
    source_observations = source_observations,
    validation_results = bundle$validation_results,
    screening_report = bundle$screening_report,
    schema_version = bundle$schema_version
  ), class = "policycraft_resolved_series")
}

#' Extract expectation-chart input from a resolved canonical series
#'
#' @param x A resolved series, or canonical observation rows already restricted
#'   to exactly one `series_id`.
#' @return The unchanged observation rows ordered by `observation_date`.
#' @export
canonical_expectation_input <- function(x) {
  rows <- if (inherits(x, "policycraft_resolved_series")) x$observations else x
  if (!is.data.frame(rows) || !"series_id" %in% names(rows)) {
    stop("Canonical expectation input must contain `series_id`.", call. = FALSE)
  }
  ids <- unique(as.character(rows$series_id[!is.na(rows$series_id) & nzchar(rows$series_id)]))
  if (length(ids) != 1L) {
    stop("Canonical expectation charts require exactly one series; found ", length(ids), ".", call. = FALSE)
  }
  rows[order(rows$observation_date, seq_len(nrow(rows)), na.last = TRUE), , drop = FALSE]
}
