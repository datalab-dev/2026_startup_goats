# this is SPECIFCALLY for standard goats NOT miniature

# Coordinate system:
#   y = 0 - hock line (used as reference a lot)
#   x = 0 - center of animal
#   Most udder geometry IDEALLY is above the hock


# Desmos equations:
#   m(x) = depth_of_medial * (x + closeness_of_halves) * (x + (closeness_of_halves + 2)) + depth_of_medial - udder_floor_height   for -leg_width < x < 0  (left)
#   n(x) = depth_of_medial * (x - closeness_of_halves) * (x - (closeness_of_halves + 2)) + depth_of_medial - udder_floor_height   for  0 < x < leg_width  (right)


# Parameters:
#   udder_floor_height  = udder floor height
#   closeness_of_halves = closeness of halves
#   depth_of_medial     = depth of medial
#   leg_width           = leg width
#   hock_height         = hock height


# Medial Linear Appraisal Score: (closeness_of_halves and depth_of_medial)
#   5 - bulging udder floor, negative cleft
#   15 - flat udder floor, lack clear halving, little or no cleft
#   25 - Clearly defined halving, clean and support
#   35 - deep cleft
#   45 - extreme cleft

#     How to find:
#       a = minimum point of either curve at udder_floor_height (they have the same minimum)
#       b = if not the same curve find the y-cord of their intersection (height of vertex of cleft)
#         *but if its the same curve then score is 1 to 5
#         *where it is from 1 to 5 depends on its "curvy-ness", so how close depth_of_medial is to 0
#       find (b - a) which will be in inches


# Udder Depth Linear Appraisal Score: (udder_floor_height, closeness_of_halves and depth_of_medial)
#   Note: proportional adjustment made for miniature breeds
#   1 - 3 inches below hock
#   5 - 2 inches below hock
#   25 - 2 inches above hock
#   45 - 6 inches above hock

#     How to find:
#       a = minimum point of either curve at udder_floor_height (they have the same minimum)
#       b = hock height, hock_height
#       find (a - b) which will be in inches

library(ggplot2)


# Defensive check: argument must be a non-null numeric scalar
check_num <- function(x) {
  if (is.null(x)) print("Argument is NULL")
  if (!is.numeric(x)) print("Argument is not numeric")
}


# Generate the left medial curve:
#   m(x) = depth_of_medial*(x+closeness_of_halves)*(x+(closeness_of_halves+2)) + depth_of_medial - udder_floor_height
generate_left_curve <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                depth_of_medial = 0.15, leg_width = 4.6,
                                n_points = 200) {
  check_num(udder_floor_height)
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  check_num(leg_width)
  check_num(n_points)
  
  x <- seq(-leg_width, 0, length.out = n_points)
  y <- depth_of_medial * (x + closeness_of_halves) * (x + (closeness_of_halves + 2)) +
       depth_of_medial - udder_floor_height
  data.frame(x = x, y = y)
}

# Generate the right medial curve:
#   n(x) = depth_of_medial*(x-closeness_of_halves)*(x-(closeness_of_halves+2)) + depth_of_medial - udder_floor_height
generate_right_curve <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                 depth_of_medial = 0.15, leg_width = 4.6,
                                 n_points = 200) {
  check_num(udder_floor_height)
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  check_num(leg_width)
  check_num(n_points)

  x <- seq(0, leg_width, length.out = n_points)
  y <- depth_of_medial * (x - closeness_of_halves) * (x - (closeness_of_halves + 2)) +
       depth_of_medial - udder_floor_height
  data.frame(x = x, y = y)
}

# Combined points for both halves
medial_df <- function(udder_floor_height = 13, closeness_of_halves = 1,
                      depth_of_medial = 0.15, leg_width = 4.6, n_points = 200) {
  rbind(
    generate_left_curve(udder_floor_height, closeness_of_halves, depth_of_medial, leg_width, n_points),
    generate_right_curve(udder_floor_height, closeness_of_halves, depth_of_medial, leg_width, n_points)
  )
}


# Closed polygon for the full udder body, composed from the medial
# curves in this file and the udder arch (generate_arch) in
# udder_curve.R. The polygon is traversed bottom left -> bottom right
# via the medial curves, then top right -> top left via the reversed
# udder arch. Requires udder_curve.R to be sourced before this runs.
body_polygon_df <- function(udder_floor_height, closeness_of_halves, depth_of_medial,
                            arch_a, arch_d, arch_s,
                            leg_width, n_points = 200) {
  left  <- generate_left_curve(udder_floor_height, closeness_of_halves, depth_of_medial, leg_width, n_points)
  right <- generate_right_curve(udder_floor_height, closeness_of_halves, depth_of_medial, leg_width, n_points)
  arch  <- generate_arch(arch_a, arch_d, arch_s, leg_width, n_points)

  arch_rev <- arch[nrow(arch):1, ]

  data.frame(
    x = c(left$x, right$x, arch_rev$x),
    y = c(left$y, right$y, arch_rev$y),
    group = "body"
  )
}


# --- Scoring functions ---

#   At x = 0 (where the two curves meet):
#     m(0) = depth_of_medial*closeness_of_halves*(closeness_of_halves+2) + depth_of_medial - udder_floor_height

#   So:
#     a (vertex, lowest point of udder floor) = -udder_floor_height
#     b (intersection at x=0, top of cleft)   = depth_of_medial*closeness_of_halves*(closeness_of_halves+2) + depth_of_medial - udder_floor_height
#     cleft_depth = b - a = depth_of_medial*closeness_of_halves*(closeness_of_halves+2) + depth_of_medial = depth_of_medial * (closeness_of_halves+1)^2

# If cleft_depth <= 0 the curves are the same / flat / bulging:
#   score is 1-5, scaled by how close depth_of_medial is to 0 (the "curvy-ness")
#   each 1 inch of cleft = 10 points, anchored at 0 = 15


get_medial_score <- function(closeness_of_halves = 1, depth_of_medial = 0.15) {
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  cleft_depth <- depth_of_medial * (closeness_of_halves + 1)^2

  if (cleft_depth <= 0) {
    # No real cleft / bulging: score 1-5
    # depth_of_medial = 0 means perfectly flat -> 5
    # depth_of_medial negative means bulging outward -> toward 1
    score <- max(1, min(5, 5 + cleft_depth * 5))
  } else {
    score <- 15 + cleft_depth * 10
  }

  return(max(1, min(50, round(score))))
}

#   much more simpler
#   a = Vertex y of both curves = -udder_floor_height (the lowest point of the udder floor)
#   b = hock_height
#   distance = b - a

get_udder_depth_score <- function(udder_floor_height = 13, hock_height = 18) {
  check_num(udder_floor_height)
  check_num(hock_height)
  # distance from udder floor vertex to hock (positive = above hock)
  distance <- hock_height - udder_floor_height

  if (distance <= -3) {
    score <- 1
  } else if (distance <= -2) {
    # Interpolate: at -3" -> 1, at -2" -> 5
    score <- 1 + (distance + 3) * 4
  } else {
    # Linear: -2" = 5, 0" = 15, +2" = 25, etc.
    score <- 15 + distance * 5
  }

  return(max(1, min(50, round(score))))
}

# Parameter section in decimals - EXPERIMENT WITH THIS
# udder_floor_height  = 13.0
# closeness_of_halves = 1.0
# depth_of_medial     = 0.15
# leg_width           = 5.0
# hock_height         = 14.3

# generate curve data

medial_visualization <- function(udder_floor_height = 13, closeness_of_halves = 1,
                                 depth_of_medial = 0.15, leg_width = 4.6,
                                 hock_height = 18) {
  check_num(udder_floor_height)
  check_num(closeness_of_halves)
  check_num(depth_of_medial)
  check_num(leg_width)
  check_num(hock_height)

  left_df  <- generate_left_curve(udder_floor_height, closeness_of_halves, depth_of_medial, leg_width)
  right_df <- generate_right_curve(udder_floor_height, closeness_of_halves, depth_of_medial, leg_width)
  medial_df <- rbind(left_df, right_df)

  head(left_df)
  tail(left_df)

  head(right_df)
  tail(right_df)


  ggplot(medial_df) +
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
