very_wide <- 500
wide <- 136
narrow <- 76
options(width = narrow)

theme_custom <- function() {

  transparent_rect <- ggplot2::element_rect(fill = NA, color = NA)
  grey_text <- ggplot2::element_text(color = "#888")
  rotated_grey_text <- ggplot2::element_text(color = "#888", angle = 90)
  light_grey_rect <- ggplot2::element_rect(fill = "#ccc", color = "#ccc")

  plot_margin <- ggplot2::margin(18, 18, 18, 18)

  strip_margin <- ggplot2::margin(4.4, 4.4, 4.4, 4.4)
  strip_background <- ggplot2::element_rect(fill = "#333", color = "#333")
  strip_text <- ggplot2::element_text(colour = "#ddd", margin = strip_margin)

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(),
    ggplot2::theme(

      # plot exterior regions
      plot.margin = plot_margin,
      plot.background   = transparent_rect,
      legend.background = transparent_rect,
      legend.key        = light_grey_rect,
      plot.title    = grey_text,
      plot.subtitle = grey_text,
      plot.caption  = grey_text,
      axis.text.x   = grey_text,
      axis.text.y   = grey_text,
      axis.title.x  = grey_text,
      axis.title.y  = rotated_grey_text, 
      legend.text   = grey_text,
      legend.title  = grey_text,

      # plot strip region
      strip.background = strip_background,
      strip.text = strip_text,

      # plot interior background
      panel.background = light_grey_rect
    ) 
  )
}
ggplot2::theme_set(theme_custom())
