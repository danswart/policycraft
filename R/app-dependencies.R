# Keep installed-app dependencies visible to static package checks. The Shiny
# files under inst/app use these namespaces at runtime, but R CMD check does not
# inspect code stored under inst/ when determining whether Imports are used.
.app_dependency_check <- function() {
  list(
    bslib::bs_theme,
    DT::datatable,
    ggplot2::ggplot,
    ggtext::element_markdown,
    purrr::map,
    readr::read_csv,
    readxl::read_excel,
    scales::percent_format,
    tidyr::pivot_longer
  )
}
