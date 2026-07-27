#' Launch the longitudinal-analysis tool
#'
#' Starts policycraft's interactive longitudinal-analysis tool. The application
#' accepts CSV, Excel, and RDS files and provides mapping, filtering,
#' longitudinal charts, expectation charts, cohort views, and export controls.
#'
#' @param launch_browser Passed to [shiny::runApp()]. Use `TRUE` to open the
#'   system browser, `FALSE` to run without opening a browser, or supply a
#'   custom browser function.
#' @param host Host address on which to serve the application. The default is
#'   Shiny's local-only default.
#' @param port Optional TCP port. `NULL` asks Shiny to choose an available port.
#' @param max_upload_mb Maximum permitted upload size in megabytes. The default
#'   is 50 MB. The previous `shiny.maxRequestSize` option is restored when the
#'   application closes.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the value produced by [shiny::runApp()]. The
#'   function is normally called for its side effect of starting the app.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   launch_longitudinal()
#' }
launch_longitudinal <- function(
  launch_browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", NULL),
  max_upload_mb = 50,
  ...
) {
  if (!is.numeric(max_upload_mb) || length(max_upload_mb) != 1L ||
      is.na(max_upload_mb) || !is.finite(max_upload_mb) || max_upload_mb <= 0) {
    stop("`max_upload_mb` must be one positive, finite number.", call. = FALSE)
  }
  old_options <- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(old_options), add = TRUE)

  app_dir <- system.file("app", package = "policycraft")
  if (!nzchar(app_dir)) {
    stop("The installed policycraft application could not be located.", call. = FALSE)
  }
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch_browser,
    host = host,
    port = port,
    ...
  )
}

#' @rdname launch_longitudinal
#' @export
load_longitudinal <- launch_longitudinal

#' @rdname launch_longitudinal
#' @section Deprecated names:
#' `run_policycraft()` and `run_app()` are retained temporarily for code written
#' before policycraft became a multi-tool package. New code should use
#' `launch_longitudinal()` (or its `load_longitudinal()` alias).
#' @export
run_policycraft <- function(...) {
  .Deprecated("launch_longitudinal")
  launch_longitudinal(...)
}

#' @rdname launch_longitudinal
#' @export
run_app <- function(...) {
  .Deprecated("launch_longitudinal")
  launch_longitudinal(...)
}
