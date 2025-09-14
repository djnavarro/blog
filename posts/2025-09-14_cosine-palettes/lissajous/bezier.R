

# additional classes to provide bezier support ----------------------------

bernstein <- function(beta, t = seq(0, 1, .01)) {
  n <- length(beta) - 1
  w <- choose(n, 0:n)
  b <- rep(0, length(t))
  for(v in 0:n) {
    b = b + beta[v + 1] * w[v + 1] * t^v * (1 - t)^(n-v)
  }
  b
}

bezier <- new_class(
  name = "bezier",
  parent = S7_object,
  properties = list(
    x = class_numeric,
    y = class_numeric,
    n = new_property(class = class_numeric, default = 100L),
    curve = new_property(
      class = class_data.frame,
      getter = function(self) {
        t <- seq(0, 1, length.out = self@n)
        data.frame(
          x = bernstein(self@x, t),
          y = bernstein(self@y, t)
        )
      }
    )
  ),
  validator = function(self) {
    if (length(self@x) != length(self@y)) return("x and y must have same length")
    if (length(self@x) < 2) return("at least two control points are required")
    if (length(self@n) != 1) return("n must be length 1")
    if (self@n <= 0) return("n must be a non-negative number")
  })

bezier_ribbon <- new_class(
  name = "bezier_ribbon",
  parent = drawable,
  properties = list(
    x          = class_numeric,
    y          = class_numeric,
    xend       = class_numeric,
    yend       = class_numeric,
    xctr_1     = class_numeric,
    yctr_1     = class_numeric,
    xctr_2     = class_numeric,
    yctr_2     = class_numeric,
    width      = class_numeric,
    smooth     = class_numeric,
    n          = class_integer,
    frequency  = class_numeric,
    octaves    = class_integer,
    seed       = class_integer,
    bezier = new_property(
      class = bezier,
      getter = function(self) {
        bezier(
          x = c(self@x, self@xctr_1, self@xctr_2, self@xend),
          y = c(self@y, self@yctr_1, self@yctr_2, self@yend),
          n = self@n
        )
      }
    ),
    points = new_property(
      class = points,
      getter = function(self) {
        x <- self@bezier@curve$x
        y <- self@bezier@curve$y
        displacement <- ambient::fracture(
          noise = ambient::gen_simplex,
          fractal = ambient::fbm,
          x = x,
          y = y,
          frequency = self@frequency,
          seed = self@seed,
          octaves = self@octaves
        ) |>
          ambient::normalize(to = c(0, 1))
        taper <- sqrt(
          seq(0, 1, length.out = self@n) * seq(1, 0, length.out = self@n)
        )
        width <- displacement * taper * self@width
        dx <- self@xend - self@x
        dy <- self@yend - self@y
        points(
          x = c(x - width * dy, x[self@n:1L] + width[self@n:1L] * dy),
          y = c(y + width * dx, y[self@n:1L] - width[self@n:1L] * dx)
        )
      }
    )
  ),
  constructor = function(x = 0,
                         y = 0,
                         xend = 1,
                         yend = 1,
                         xctr_1 = .5,
                         yctr_1 = .5,
                         xctr_2 = 0,
                         yctr_2 = 0,
                         width = 0.2,
                         smooth = 3L,
                         n = 100L,
                         frequency = 1,
                         octaves = 2L,
                         seed = 1L,
                         ...) {
    new_object(
      drawable(),
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      xctr_1 = xctr_1,
      yctr_1 = yctr_1,
      xctr_2 = xctr_2,
      yctr_2 = yctr_2,
      width = width,
      smooth = smooth,
      n = n,
      frequency = frequency,
      octaves = octaves,
      seed = seed,
      style = style(...)
    )
  },
  validator = function(self) {
    if (length(self@x) != 1) return("x must be length 1")
    if (length(self@y) != 1) return("y must be length 1")
    if (length(self@xend) != 1) return("xend must be length 1")
    if (length(self@yend) != 1) return("yend must be length 1")
    if (length(self@width) != 1) return("width must be length 1")
    if (length(self@n) != 1) return("n must be length 1")
    if (length(self@frequency) != 1) return("frequency must be length 1")
    if (length(self@octaves) != 1) return("octaves must be length 1")
    if (length(self@seed) != 1) return("seed must be length 1")
    if (self@width < 0) return("width must be a non-negative number")
    if (self@frequency < 0) return("frequency must be a non-negative number")
    if (self@n < 1L) return("n must be a positive integer")
    if (self@octaves < 1L) return("octaves must be a positive integer")
  }
)
