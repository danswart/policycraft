#' Apply the policycraft chart theme
#'
#' Provides the shared typography, colors, spacing, and backgrounds used by
#' charts created in the R console and by the `launch_longitudinal()` app.
#' Keeping the theme in the package ensures that both interfaces retain the
#' same visual identity.
#'
#' @param base_size Base text size in points.
#' @param title_size,subtitle_size,caption_size,axis_title_size Sizes in points
#'   for the named plot elements.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
#'   ggplot2::geom_line() +
#'   policycraft_chart_theme()
policycraft_chart_theme <- function(
  base_size = 26,
  title_size = 21,
  subtitle_size = 19,
  caption_size = 22,
  axis_title_size = 23
) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(
        size = axis_title_size,
        margin = ggplot2::margin(t = 20)
      ),
      axis.title.y = ggplot2::element_text(size = axis_title_size),
      text = ggplot2::element_text(colour = "royalblue"),
      plot.title.position = "panel",
      plot.title = ggtext::element_markdown(
        colour = "darkgreen",
        size = title_size,
        face = "bold",
        lineheight = 1.1,
        margin = ggplot2::margin(2, 0, 0, 0, "lines")
      ),
      plot.subtitle = ggtext::element_markdown(
        colour = "darkgreen",
        size = subtitle_size,
        face = "bold",
        lineheight = 1,
        margin = ggplot2::margin(0, 0, 0, 0, "lines")
      ),
      plot.caption = ggplot2::element_text(
        size = caption_size,
        hjust = 0,
        vjust = 2,
        face = "italic",
        colour = "darkblue"
      ),
      axis.text = ggplot2::element_text(colour = "black"),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
    )
}
