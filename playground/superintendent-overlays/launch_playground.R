# Launch the experimental app copy without modifying or installing policycraft.
project_root <- normalizePath(getwd(), mustWork = TRUE)
app_dir <- file.path(project_root, "playground", "superintendent-overlays", "app")

if (!file.exists(file.path(app_dir, "server.R"))) {
  stop("Run this script from the policycraft package root.", call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The pkgload package is required to run this development playground.", call. = FALSE)
}

pkgload::load_all(project_root, quiet = TRUE)
shiny::runApp(app_dir)

