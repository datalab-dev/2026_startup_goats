# pelvic arch
# rear udder height trait is scored by assessing the halfway spot
# between the base of the pelvic arch and the point of hock:
# midpoint, a score of 10 on the linear scale
# Top of milk secretory tissue in relation to the hock 1/2=10 points,
# 5/8 = 20 points, 3/4 = 30 points, 7/8 = 40 points,
# at pelvic arch = 50 points

# desmos equations
# g(x) = -0.03x(x + 5) {0 > x > -leg_width} ---> left arch
# v(x) = -0.03x(x - 5) {leg_width > x > 0}   ---> right arch
# y > g(x)
# y > v(x)

# leg_width = horizontal distance between the legs, constrained to 0–20 inches

library(ggplot2)

pelvic_arch_df <- function(leg_width = 4.6) {
  if (!is.numeric(leg_width)) stop("Input must be numeric")

  leg_width <- max(0, min(20, leg_width))

  left_x  <- seq(-leg_width, 0, length.out = 200)
  right_x <- seq(0, leg_width, length.out = 200)

  rbind(
    data.frame(x = left_x,  y = -0.03 * left_x  * (left_x  + 5)),
    data.frame(x = right_x, y = -0.03 * right_x * (right_x - 5))
  )
}


# Closed polygon for the region above the pelvic arch, capped at top_y.
pelvic_polygon_df <- function(leg_width = 4.6, top_y = 3, n_points = 200) {
  if (!is.numeric(leg_width) || !is.numeric(top_y) || !is.numeric(n_points)) {
    stop("Input must be numeric")
  }

  leg_width <- max(0, min(20, leg_width))

  x_left  <- seq(-leg_width, 0, length.out = n_points)
  y_left  <- -0.03 * x_left  * (x_left  + 5)

  x_right <- seq(0, leg_width, length.out = n_points)
  y_right <- -0.03 * x_right * (x_right - 5)

  data.frame(
    x     = c(x_left, x_right, leg_width, -leg_width),
    y     = c(y_left, y_right, top_y, top_y),
    group = "pelvic"
  )
}


pelvic_arch_visualization <- function(leg_width = 4.6) {
  if (!is.numeric(leg_width)) stop("Input must be numeric")

  leg_width <- max(0, min(20, leg_width))

  ggplot(pelvic_arch_df(leg_width)) +
    aes(x, y) +
    geom_line()
}


if (sys.nframe() == 0) {
  pelvic_arch_visualization()
}
