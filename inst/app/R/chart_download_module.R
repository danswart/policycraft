chart_download_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::downloadButton(ns("png"), "Download PNG", class = "btn-primary"),
    shiny::tags$span(style = "margin: 0 10px;"),
    shiny::downloadButton(ns("svg"), "Download SVG", class = "btn-success"),
    shiny::tags$span(style = "margin: 0 10px;"),
    shiny::downloadButton(ns("pdf"), "Download PDF", class = "btn-info")
  )
}

chart_plot_ui <- function(id, height = "600px") {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("plot"), height = height)
}

chart_plot_server <- function(id, plot_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plot <- shiny::renderPlot(plot_reactive())
  })
}

chart_download_server <- function(
  id,
  plot_reactive,
  filename_prefix,
  width = CHART_EXPORT_WIDTH_IN,
  height = CHART_EXPORT_HEIGHT_IN
) {
  shiny::moduleServer(id, function(input, output, session) {
    output$png <- shiny::downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".png"),
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = plot_reactive(),
          device = "png",
          width = width,
          height = height,
          dpi = CHART_EXPORT_DPI,
          bg = "white"
        )
      }
    )

    output$svg <- shiny::downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".svg"),
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = plot_reactive(),
          device = "svg",
          width = width,
          height = height,
          bg = "white"
        )
      }
    )

    output$pdf <- shiny::downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        tryCatch(
          ggplot2::ggsave(
            file,
            plot = plot_reactive(),
            device = grDevices::cairo_pdf,
            width = width,
            height = height,
            bg = "white"
          ),
          error = function(e) {
            ggplot2::ggsave(
              file,
              plot = plot_reactive(),
              device = "pdf",
              width = width,
              height = height,
              bg = "white"
            )
          }
        )
      }
    )
  })
}
