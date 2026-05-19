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

check_numeric_input <- function(x) { # checks input if it is numeric 
  name <- deparse(substitute(x))
  
  if (!is.numeric(x)) {
    stop(name, " must be numeric")
  }
}

# this will check if the linear appraisal score is numeric and within the given range
check_score_in_valid_range <- function(score, min_score, max_score) {
  check_numeric_input(score)
  
  if (score < min_score || score > max_score) {
    stop("score must be between ", min_score, " and ", max_score)
  }
}

generate_left_curve <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                depth_of_medial = 0.15, leg_width = 4.6,
                                n_points = 200) {
  check_numeric_input(udder_floor_height)
  check_numeric_input(closeness_of_halves)
  check_numeric_input(depth_of_medial)
  check_numeric_input(leg_width)
  check_numeric_input(n_points)

  x <- seq(-leg_width, 0, length.out = n_points)
  y <- depth_of_medial * (x + closeness_of_halves) *
       (x + (closeness_of_halves + 2)) +
       depth_of_medial - udder_floor_height
  data.frame(x = x, y = y)
}

generate_right_curve <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                 depth_of_medial = 0.15, leg_width = 4.6,
                                 n_points = 200) {
  check_numeric_input(udder_floor_height)
  check_numeric_input(closeness_of_halves)
  check_numeric_input(depth_of_medial)
  check_numeric_input(leg_width)
  check_numeric_input(n_points)

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

score_to_estimated_medial_cleft_inches <- function(score) {
  check_score_in_valid_range(score, min_score = 5, max_score = 45)
  
  scales::rescale(score, to = c(-1, 3), from = c(5, 45))
}

# the medial scores that tend to be higher should show a more defined cleft
score_to_closeness_of_halves_input <- function(score) {
  check_score_in_valid_range(score, min_score = 5, max_score = 45)
  
  scales::rescale(score, to = c(0.25, 1.4), from = c(5, 45))
}

estimated_medial_cleft_inches_to_depth_input <- function(estimated_medial_cleft_inches,
                                                         closeness_of_halves) {
  estimated_medial_cleft_inches / (closeness_of_halves + 1)^2
}

# udder depth score has inch anchors relative to the hock
# negative = below hock + positive = above hock
score_to_udder_depth_inches_from_hock <- function(score) {
  check_score_in_valid_range(score, min_score = 1, max_score = 45)
  
  approx(
    x = c(1, 5, 25, 45),
    y = c(-3, -2, 2, 6),
    xout = score,
    rule = 2
  )$y
}

score_to_medial_inputs <- function(medial_score,
                                   udder_depth_score,
                                   leg_width) {
  check_score_in_valid_range(medial_score, min_score = 5, max_score = 45)
  check_score_in_valid_range(udder_depth_score, min_score = 1, max_score = 45)
  check_numeric_input(leg_width)
  
  estimated_medial_cleft_inches <- score_to_estimated_medial_cleft_inches(medial_score)
  closeness_of_halves <- score_to_closeness_of_halves_input(medial_score)
  
  list(
    udder_floor_height_from_hock = score_to_udder_depth_inches_from_hock(udder_depth_score),
    closeness_of_halves = closeness_of_halves,
    depth_of_medial     = estimated_medial_cleft_inches_to_depth_input(
      estimated_medial_cleft_inches,
      closeness_of_halves
    ),
    leg_width           = leg_width
  )
}

medial_visualization <- function(udder_floor_height_from_hock = 2,
                                 closeness_of_halves = 1,
                                 depth_of_medial = 0.15,
                                 leg_width = 4.6,
                                 n_points = 200) {
  check_numeric_input(udder_floor_height_from_hock)
  check_numeric_input(closeness_of_halves)
  check_numeric_input(depth_of_medial)
  check_numeric_input(leg_width)
  check_numeric_input(n_points)
  
  df <- medial_df(
    udder_floor_height = udder_floor_height_from_hock,
    closeness_of_halves = closeness_of_halves,
    depth_of_medial = depth_of_medial,
    leg_width = leg_width,
    n_points = n_points
  )
  
  ggplot(df, aes(x = x, y = y)) +
    geom_line(linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    coord_equal() +
    theme_minimal() +
    labs(
      title = "Goat Medial Suspensory Ligament",
      subtitle = paste(
        "udder floor from hock =", udder_floor_height_from_hock,
        "| closeness_of_halves =", closeness_of_halves,
        "| depth_of_medial =", depth_of_medial,
        "| leg_width =", leg_width
      ),
      x = "Horizontal position",
      y = "Vertical position relative to hock"
    )
}

medial_visualization_from_scores <- function(medial_score,
                                             udder_depth_score,
                                             leg_width = 4.6,
                                             n_points = 200) {
  medial_inputs <- score_to_medial_inputs(
    medial_score = medial_score,
    udder_depth_score = udder_depth_score,
    leg_width = leg_width
  )
  
  medial_visualization(
    udder_floor_height_from_hock = medial_inputs$udder_floor_height_from_hock,
    closeness_of_halves = medial_inputs$closeness_of_halves,
    depth_of_medial = medial_inputs$depth_of_medial,
    leg_width = medial_inputs$leg_width,
    n_points = n_points
  )
}

if (sys.nframe() == 0) {
  p <- medial_visualization_from_scores(
    medial_score = 30,
    udder_depth_score = 25,
    leg_width = 4.6
  )
  
  print(p)
}

