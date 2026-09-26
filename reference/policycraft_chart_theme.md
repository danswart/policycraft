# Apply the policycraft chart theme

Provides the shared typography, colors, spacing, and backgrounds used by
charts created in the R console and by the
[`launch_longitudinal()`](https://danswart.github.io/policycraft/reference/launch_longitudinal.md)
app. Keeping the theme in the package ensures that both interfaces
retain the same visual identity.

## Usage

``` r
policycraft_chart_theme(
  base_size = 26,
  title_size = 21,
  subtitle_size = 19,
  caption_size = 22,
  axis_title_size = 23
)
```

## Arguments

- base_size:

  Base text size in points.

- title_size, subtitle_size, caption_size, axis_title_size:

  Sizes in points for the named plot elements.

## Value

A ggplot2 theme object.

## Examples

``` r
ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
  ggplot2::geom_line() +
  policycraft_chart_theme()
```
