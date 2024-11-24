library(ggplot2)

bernstein <- function(beta, t = seq(0, 1, .01)) {
  n <- length(beta) - 1
  w <- choose(n, 0:n)
  b <- rep(0, length(t))
  for(v in 0:n) {
    b = b + beta[v + 1] * w[v + 1] * t^v * (1 - t)^(n-v)
  }
  b
}

controls <- list(
  tibble::tibble(
    x = c(1, 5, 6, 7, 8),
    y = c(1, 1, 9, 8, 6)
  ),
  tibble::tibble(
    x = c(1, 7, 6, 5, 8),
    y = c(1, 8, 9, 1, 6)
  )
)

for(gif in 1:2) {

  gif_path <- here::here(
    "posts",
    "2024-11-24_bezier-curves",
    "img",
    paste0("weights-", gif, ".gif")
  )

  control <- controls[[gif]]
  n <- nrow(control)

  bezier <- tibble::tibble(
    t = seq(0, 1, .01),
    x = bernstein(control$x, t),
    y = bernstein(control$y, t)
  )

  base <- ggplot() +
    aes(x, y) +
    geom_path(data = bezier) +
    coord_equal(xlim = c(0, 10), ylim = c(0, 10)) +
    theme_bw(base_size = 18)

  t_vals <- seq(0, 1, .005)
  t_vals <- c(t_vals, rev(t_vals))

  gifski::save_gif(
    expr = {
      for(t in t_vals) {
        t_str <- formatC(t, digits = 2, format = "f")
        weighted_control <- control |>
          dplyr::mutate(
            w = dbinom(0:(n-1), n-1, prob = t),
            i = 1:n
          )
        p <- base +
          geom_point(
            data = tibble::tibble(
              x = bernstein(control$x, t),
              y = bernstein(control$y, t)
            ),
            size = 4
          ) +
          geom_point(
            mapping = aes(size = w),
            data = weighted_control,
            color = "red",
            show.legend = FALSE
          ) +
          geom_label(
            mapping = aes(label = i),
            data = weighted_control,
            nudge_x = -0.75,
            size = 6
          ) +
          annotate(
            "label",
            x = 0,
            y = 10,
            label = paste("t =", t_str),
            vjust = "inward",
            hjust = "inward",
            size = 6,
            color = "grey50"
          ) +
          scale_size_area(max_size = 12)
        plot(p)
      }
    },
    gif_file = gif_path,
    delay = .025,
    loop = TRUE
  )

}
