# this is SPECIFICALLY for standard goats NOT miniature

# Coordinate system:
#   y = 0 - hock line (used as reference)
#   x = 0 - center of animal
#   Most udder geometry IDEALLY is above the hock

# Desmos equations:
#   m(x) = depth_of_medial*(x + closeness_of_halves)*(x + (closeness_of_halves + 2)) + depth_of_medial - udder_floor_height   for -leg_width < x < 0  (left)
#   n(x) = depth_of_medial*(x - closeness_of_halves)*(x - (closeness_of_halves + 2)) + depth_of_medial - udder_floor_height   for  0 < x < leg_width  (right)

# Parameters:
#   udder_floor_height  = height of the udder floor above the ground baseline
#   closeness_of_halves = how close the two udder halves are to each other
#   depth_of_medial     = depth/curvature of the medial suspensory ligament
#   leg_width  = horizontal distance between the legs
#   hock_height         = height of the rear knee joint above the ground baseline

# Medial Linear Appraisal Score: (closeness_of_halves and depth_of_medial)
#   5  - bulging udder floor, negative cleft
#   15 - flat udder floor, no clear halving, little or no cleft
#   25 - clearly defined halving, clean and supported
#   35 - deep cleft
#   45 - extreme cleft

# Udder Depth Linear Appraisal Score: (udder_floor_height and hock_height)
#   Note: proportional adjustment made for miniature breeds
#   1  - 3 inches below hock
#   5  - 2 inches below hock
#   25 - 2 inches above hock
#   45 - 6 inches above hock

library(ggplot2)

check_num <- function(x) {
  if (is.null(x))     print("Argument is NULL")
  if (!is.numeric(x)) print("Argument is not numeric")
}


generate_left_curve <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                depth_of_medial = 0.15, leg_width = 4.6,
                                n_points = 200) {
  check_num(udder_floor_height)
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  check_num(leg_width)
  check_num(n_points)

  x <- seq(-leg_width, 0, length.out = n_points)
  y <- depth_of_medial * (x + closeness_of_halves) *
       (x + (closeness_of_halves + 2)) +
       depth_of_medial - udder_floor_height
  data.frame(x = x, y = y)
}

generate_right_curve <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                 depth_of_medial = 0.15, leg_width = 4.6,
                                 n_points = 200) {
  check_num(udder_floor_height)
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  check_num(leg_width)
  check_num(n_points)

  x <- seq(0, leg_width, length.out = n_points)
  y <- depth_of_medial * (x - closeness_of_halves) *
       (x - (closeness_of_halves + 2)) +
       depth_of_medial - udder_floor_height
  data.frame(x = x, y = y)
}

medial_df <- function(udder_floor_height = 13, closeness_of_halves = 1,
                      depth_of_medial = 0.15, leg_width = 4.6,
                      n_points = 200) {
  rbind(
    generate_left_curve(udder_floor_height, closeness_of_halves,
                        depth_of_medial, leg_width, n_points),
    generate_right_curve(udder_floor_height, closeness_of_halves,
                         depth_of_medial, leg_width, n_points)
  )
}


# Closed polygon for the full udder body.
# Traced: left medial curve (bottom-left) → right medial curve (bottom-right)
# → reversed udder arch (top-right to top-left). Requires udder_curve.R to be sourced.
body_polygon_df <- function(udder_floor_height, closeness_of_halves, depth_of_medial,
                            arch_roundness, arch_height, arch_shape,
                            leg_width, n_points = 200) {
  left  <- generate_left_curve(udder_floor_height, closeness_of_halves,
                               depth_of_medial, leg_width, n_points)
  right <- generate_right_curve(udder_floor_height, closeness_of_halves,
                                depth_of_medial, leg_width, n_points)
  arch  <- generate_arch(arch_roundness, arch_height, arch_shape,
                         leg_width, n_points)

  arch_rev <- arch[nrow(arch):1, ]

  data.frame(
    x     = c(left$x,  right$x,  arch_rev$x),
    y     = c(left$y,  right$y,  arch_rev$y),
    group = "body"
  )
}


# --- Scoring functions ---

get_medial_score <- function(closeness_of_halves = 1, depth_of_medial = 0.15) {
  check_num(closeness_of_halves)
  check_num(depth_of_medial)

  cleft_depth <- depth_of_medial * (closeness_of_halves + 1)^2

  if (cleft_depth <= 0) {
    score <- max(1, min(5, 5 + cleft_depth * 5))
  } else {
    score <- 15 + cleft_depth * 10
  }

  return(max(1, min(50, round(score))))
}

get_udder_depth_score <- function(udder_floor_height = 13, hock_height = 18) {
  check_num(udder_floor_height)
  check_num(hock_height)

  distance <- hock_height - udder_floor_height

  if (distance <= -3) {
    score <- 1
  } else if (distance <= -2) {
    score <- 1 + (distance + 3) * 4
  } else {
    score <- 15 + distance * 5
  }

  return(max(1, min(50, round(score))))
}


medial_visualization <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                 depth_of_medial = 0.15, leg_width = 4.6,
                                 hock_height = 18) {
  check_num(udder_floor_height)
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  check_num(leg_width)
  check_num(hock_height)

  df <- medial_df(udder_floor_height, closeness_of_halves,
                  depth_of_medial, leg_width)

  ggplot(df) +
    aes(x = x, y = y) +
    geom_point() +
    geom_hline(yintercept = -hock_height, linetype = "dashed", color = "blue")

  print("Medial Score:")
  print(get_medial_score(closeness_of_halves, depth_of_medial))

  print("Udder Depth Score:")
  print(get_udder_depth_score(udder_floor_height, hock_height))
}


if (sys.nframe() == 0) {
  medial_visualization()
}
