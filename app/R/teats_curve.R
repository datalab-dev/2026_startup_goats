# TEATS EQUATIONS

# Desmos Equations:

# Left Teat
# f(x) = teat_diameter * (x + teat_placement) * (x + (teat_roundness + teat_placement)) - (udder_floor_height + teat_length)

# Right Teat
# g(x) = teat_diameter * (x - teat_placement) * (x - (teat_roundness + teat_placement)) - (udder_floor_height + teat_length)

# Parameters:
# teat_placement   = distance from center to each teat (halves the teat when 0)
# teat_roundness/depth_of_medial   = distance between parabola roots; controls teat width (q: depth of medial on desmos)
# udder_floor_height = where the udder floor sits (shared with medial)
# teat_length      = how far the teat hangs below the udder floor
# teat_diameter    = width scaling of the teat
# leg_width = horizontal boundary for the plot x-range

library(ggplot2)
source("app/R/utils.R")

# inches -> score 

score_to_teat_length <- function(score, goat_size = "standard") {
  if (goat_size == "standard") {
    scales::rescale(score, to = c(0.5, 5.0), from = c(5, 50))
  } else if (goat_size == "miniature") {
    scales::rescale(score, to = c(0.25, 2.5), from = c(5, 50))
  } else {
    stop("goat_size must be 'standard' or 'miniature'")
  }
}

#
# functions to be used for making visualizations
#

# makes a dataframe for plotting the teats
teats_df <- function (teat_placement = 2.7, depth_of_medial = 0.15, 
                      udder_floor_height = 13, teat_length = 1.4,
                      teat_diameter = 3.5, leg_width = 4.6,
                      n_points = 200) {
  if (!is.numeric(c(teat_placement, depth_of_medial, udder_floor_height, 
                    teat_length, teat_diameter, leg_width, n_points))) {
    stop("Input must be numeric")
  }
  
  x_left  <- seq(-leg_width, 0, length.out = n_points)
  x_right <- seq(0, leg_width, length.out = n_points)
  
  y_left  <- teat_diameter * (x_left  + teat_placement) *
    (x_left  + (depth_of_medial + teat_placement)) -
    (udder_floor_height + teat_length)
  
  y_right <- teat_diameter * (x_right - teat_placement) *
    (x_right - (depth_of_medial + teat_placement)) -
    (udder_floor_height + teat_length)
  
  df <- rbind(
    data.frame(x = x_left,  y = y_left),
    data.frame(x = x_right, y = y_right)
  )
}

# creates a visualization of the teats based off input parameters
teat_model <- function(teat_placement = 2.7, depth_of_medial = 0.15, 
                       udder_floor_height = 13, teat_length = 1.4, 
                       teat_diameter = 3.5, leg_width = 4.6,
                       teat_length_score = NULL, teat_diameter_score = NULL, 
                       teat_placement_score = NULL, score_as_input = FALSE,
                       goat_size = "standard",
                       n_points = 200) {
  
  if (score_as_input) {
    if (anyNA(c(teat_length_score, teat_diameter_score, teat_placement_score))) {
      stop("Score must be set as an input")
    }
    if (!is.numeric(c(teat_length_score, teat_diameter_score, 
                      teat_placement_score))) {
      stop("Input must be numeric")
    }
    
    # calculate the goat measurements if scores are provided
    teat_length <- score_to_teat_length(teat_length_score, goat_size)
    teat_diameter <- score_to_teat_diameter(teat_diameter_score, goat_size)
    teat_placement <- score_to_teat_placement(teat_placement_score, leg_width)
    
    # need to take depth of medial, udder floor height, and leg width as an input
  }
  
  if (!is.numeric(c(teat_placement, depth_of_medial, udder_floor_height, teat_length, 
                    teat_diameter, leg_width))) {
    stop("Input must be numeric")
  }

  df <- teats_df(teat_placement, depth_of_medial, udder_floor_height, 
                 teat_length, teat_diameter, leg_width, n_points)
  
  ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    coord_fixed(xlim = c(-20, 20), ylim = c(-30, 10)) +
    theme_minimal()
}

# Closed polygon for both teats. The top boundary of each teat polygon follows
# the medial curve (udder floor) so the teats connect seamlessly to the udder body.
# The bottom boundary is the teat parabola.
teats_polygon_df <- function(teat_placement, depth_of_medial, udder_floor_height,
                             teat_length, teat_diameter, leg_width, 
                             closeness_of_halves, teat_length_score = NULL, 
                             teat_diameter_score = NULL, 
                             teat_placement_score = NULL, score_as_input = FALSE,
                             goat_size = "standard", n_points = 200) {
  if (score_as_input) {
    if (anyNA(c(teat_length_score, teat_diameter_score, teat_placement_score))) {
      stop("Score must be set as an input")
    }
    if (!is.numeric(c(teat_length_score, teat_diameter_score, 
                      teat_placement_score))) {
      stop("Input must be numeric")
    }
    
    # calculate the goat measurements if scores are provided
    teat_length <- score_to_teat_length(teat_length_score, goat_size)
    teat_diameter <- score_to_teat_diameter(teat_diameter_score, goat_size)
    teat_placement <- score_to_teat_placement(teat_placement_score, leg_width)
    
    # need to take depth of medial, udder floor height, and leg width as an input
  }
  
  if (!is.numeric(c(teat_placement, depth_of_medial, udder_floor_height,
                    teat_length, teat_diameter, leg_width, closeness_of_halves))) {
    stop("Input must be numeric")
  }
  
  x_left  <- seq(-leg_width, 0, length.out = n_points)
  x_right <- seq(0, leg_width, length.out = n_points)

  # teat parabolas (bottom boundary)
  y_teat_left  <- teat_diameter * (x_left  + teat_placement) *
                  (x_left  + (depth_of_medial + teat_placement)) -
                  (udder_floor_height + teat_length)
  y_teat_right <- teat_diameter * (x_right - teat_placement) *
                  (x_right - (depth_of_medial + teat_placement)) -
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

  # When the teat parabola sits entirely above the udder floor (e.g. a user
  # snaps teat_length to its minimum), there's nothing to draw. Return an
  # empty data.frame with the right columns so ggplot's aes() can still bind.
  if (length(polys) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), group = character(0)))
  }

  do.call(rbind, polys)
}

if (sys.nframe() == 0) {
  teat_model(
    depth_of_medial = 0.15, 
    udder_floor_height = 13, 
    leg_width = 4.6,
    teat_length_score = 14, 
    teat_diameter_score = 50, 
    teat_placement_score = 31, 
    score_as_input = TRUE
  )
  
  teat_model()
}
