# TEATS EQUATIONS

# Desmos Equations:

# Left Teat
# f(x) = teat_diameter * (x + teat_placement) * (x + (teat_roundness + teat_placement)) - (udder_floor_height + teat_length)

# Right Teat
# g(x) = teat_diameter * (x - teat_placement) * (x - (teat_roundness + teat_placement)) - (udder_floor_height + teat_length)

# Parameters:
# teat_placement   = distance from center to each teat (halves the teat when 0)
# teat_roundness   = distance between parabola roots; controls teat width
# udder_floor_height = where the udder floor sits (shared with medial)
# teat_length      = how far the teat hangs below the udder floor
# teat_diameter    = width scaling of the teat
# leg_width = horizontal boundary for the plot x-range

library(tidyverse)

teat_model <- function(teat_placement, teat_roundness, udder_floor_height,
                       teat_length, teat_diameter, leg_width,
                       teat_length_score = NULL, n_points = 200) {

  if (!is.null(teat_length_score)) {
    if      (teat_length_score == 50) teat_length <- 5.0
    else if (teat_length_score == 45) teat_length <- 4.5
    else if (teat_length_score == 40) teat_length <- 4.0
    else if (teat_length_score == 35) teat_length <- 3.5
    else if (teat_length_score == 30) teat_length <- 3.0
    else if (teat_length_score == 25) teat_length <- 2.5
    else if (teat_length_score == 20) teat_length <- 2.0
    else if (teat_length_score == 15) teat_length <- 1.5
    else if (teat_length_score == 10) teat_length <- 1.0
    else if (teat_length_score == 5)  teat_length <- 0.5
  }

  x_left  <- seq(-leg_width, 0, length.out = n_points)
  x_right <- seq(0, leg_width, length.out = n_points)

  y_left  <- teat_diameter * (x_left  + teat_placement) *
             (x_left  + (teat_roundness + teat_placement)) -
             (udder_floor_height + teat_length)

  y_right <- teat_diameter * (x_right - teat_placement) *
             (x_right - (teat_roundness + teat_placement)) -
             (udder_floor_height + teat_length)

  df <- rbind(
    data.frame(x = x_left,  y = y_left),
    data.frame(x = x_right, y = y_right)
  )

  ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    coord_fixed(xlim = c(-20, 20), ylim = c(-30, 10)) +
    theme_minimal()
}


# Closed polygon for both teats. The top boundary of each teat polygon follows
# the medial curve (udder floor) so the teats connect seamlessly to the udder body.
# The bottom boundary is the teat parabola.
teats_polygon_df <- function(teat_placement, teat_roundness, udder_floor_height,
                              teat_length, teat_diameter, leg_width,
                              closeness_of_halves, depth_of_medial,
                              n_points = 200) {

  x_left  <- seq(-leg_width, 0, length.out = n_points)
  x_right <- seq(0, leg_width, length.out = n_points)

  # teat parabolas (bottom boundary)
  y_teat_left  <- teat_diameter * (x_left  + teat_placement) *
                  (x_left  + (teat_roundness + teat_placement)) -
                  (udder_floor_height + teat_length)
  y_teat_right <- teat_diameter * (x_right - teat_placement) *
                  (x_right - (teat_roundness + teat_placement)) -
                  (udder_floor_height + teat_length)

  # medial curves (top boundary — the udder floor the teats hang from)
  y_floor_left  <- depth_of_medial * (x_left  + closeness_of_halves) *
                   (x_left  + (closeness_of_halves + 2)) +
                   depth_of_medial - udder_floor_height
  y_floor_right <- depth_of_medial * (x_right - closeness_of_halves) *
                   (x_right - (closeness_of_halves + 2)) +
                   depth_of_medial - udder_floor_height

  left_mask  <- y_teat_left  < y_floor_left
  right_mask <- y_teat_right < y_floor_right

  polys <- list()

  if (any(left_mask)) {
    lx   <- x_left[left_mask]
    ly_t <- y_teat_left[left_mask]
    ly_f <- y_floor_left[left_mask]
    polys[["left"]] <- data.frame(
      x     = c(lx, rev(lx)),
      y     = c(ly_t, rev(ly_f)),
      group = "left_teat"
    )
  }

  if (any(right_mask)) {
    rx   <- x_right[right_mask]
    ry_t <- y_teat_right[right_mask]
    ry_f <- y_floor_right[right_mask]
    polys[["right"]] <- data.frame(
      x     = c(rx, rev(rx)),
      y     = c(ry_t, rev(ry_f)),
      group = "right_teat"
    )
  }

  do.call(rbind, polys)
}


if (sys.nframe() == 0) {
  teat_model(
    teat_placement     = 2.5,
    teat_roundness     = 0.15,
    udder_floor_height = 13,
    teat_length        = 2,
    teat_diameter      = 4.5,
    leg_width = 4.3
  )
}
