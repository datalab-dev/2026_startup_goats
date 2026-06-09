# TEATS GEOMETRY
#
# The app builds teats directly from physical measurements (inches), produced
# by score_geometry.R's scores_to_geometry(). Each teat is a symmetric parabola
# whose vertex sits below the medial (udder-floor) curve; the polygon's top edge
# traces that medial curve so the teat joins the udder body cleanly.
#
# Parameters (all in inches unless noted):
#   teat_x_center       = horizontal offset of each teat from center
#   teat_diameter_in    = teat width
#   teat_length_in      = how far the teat hangs below the udder floor
#   udder_floor_height  = where the udder floor sits (shared with the medial curve)
#   closeness_of_halves = root spacing of the medial parabola (udder cleft)
#   depth_of_medial     = depth scaling of the medial parabola

# Closed polygons for both teats built directly from physical measurements
# (used by the score-driven flow). Each teat is a symmetric parabola whose
# vertex sits at (+/- teat_x_center, medial(teat_x_center) - teat_length_in)
# and which reaches teat_diameter_in wide at the medial-floor level. The
# top of each polygon traces the medial curve over the teat's x-range so
# the teat joins the udder cleanly.
teats_polygon_from_measurements <- function(teat_x_center, teat_diameter_in,
                                            teat_length_in,
                                            udder_floor_height,
                                            closeness_of_halves,
                                            depth_of_medial,
                                            n_points = 200) {

  # default teat paraboula with the height and width of the teat length and diameter respectively
  teat_curvature <- 4 * teat_length_in / teat_diameter_in^2

  build_side <- function(x_c, side) {
    # building the x-range for the parabola and medial curve
    x <- seq(x_c - (teat_diameter_in / 2), x_c + (teat_diameter_in / 2), length.out = n_points)

    if (side == "r") {
      # for the floor y-values, we need to evaluate the medial curve at the same x-values as the parabola.
      # The medial curve is defined as a parabola that opens upwards, with its vertex at (0, depth_of_medial - udder_floor_height).
      # The equation for the medial curve is:
      floor_y <- depth_of_medial * (x - closeness_of_halves) *
                 (x - (closeness_of_halves + 2)) +
                 depth_of_medial - udder_floor_height
      base_y  <- depth_of_medial * (x_c - closeness_of_halves) *
                 (x_c - (closeness_of_halves + 2)) +
                 depth_of_medial - udder_floor_height
    } else {
      floor_y <- depth_of_medial * (x + closeness_of_halves) *
                 (x + (closeness_of_halves + 2)) +
                 depth_of_medial - udder_floor_height
      base_y  <- depth_of_medial * (x_c + closeness_of_halves) *
                 (x_c + (closeness_of_halves + 2)) +
                 depth_of_medial - udder_floor_height
    }

    # the parabola equation is defined as y = a(x - h)^2 + k, where (h, k) is the vertex of the teat, so the bottom curve of the teat
    parab_y <- teat_curvature * (x - x_c)^2 + base_y - teat_length_in # subtract the teat length to get the vertex at the correct height

    # creating a mask to keep points where the parabola is below the medial curve (the udder floor)
    # this ensures that when its rendered all together, the teats will be drawn below the udder floor and not intersect it
    mask <- parab_y < floor_y
    if (!any(mask)) {
      return(data.frame(x = numeric(0), y = numeric(0), group = character(0)))
    }
    x       <- x[mask]
    parab_y <- parab_y[mask]
    floor_y <- floor_y[mask]

    # combine into a closed polygon: parabola (bottom) out, floor (top) back.
    data.frame(
      x     = c(x, rev(x)),
      y     = c(parab_y, rev(floor_y)),
      group = paste0(side, "_teat")
    )
  }

  rbind(
    build_side( teat_x_center, "r"),
    build_side(-teat_x_center, "l")
  )
}
