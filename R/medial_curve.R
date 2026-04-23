# this is SPECIFCALLY for standard goats NOT miniature

# Coordinate system:
#   y = 0 - hock line (used as reference a lot)
#   x = 0 - center of animal 
#   Most udder geometry IDEALLY is above the hock


# Desmos equations:
#   m(x) = q * (x + p) * (x + (p + 2)) + q - o   for -l < x < 0  (left)
#   n(x) = q * (x - p) * (x - (p + 2)) + q - o   for  0 < x < l  (right)


# Parameters:
#   o = udder floor height 
#   p = closeness of halves 
#   q = depth of medial cleft 
#   l = leg boundary
#   r = hock height


# Medial Linear Appraisal Score: (p and q)
#   5 - bulging udder floor, negative cleft
#   15 - flat udder floor, lack clear halving, little or no cleft
#   25 - Clearly defined halving, clean and support
#   35 - deep cleft
#   45 - extreme cleft

#     How to find:
#       a = minimum point of either curve at o (they have the same minimum)
#       b = if not the same curve find the y-cord of their intersection (height of vertex of cleft)
#         *but if its the same curve then score is 1 to 5
#         *where it is from 1 to 5 depends on its "curvy-ness", so how close @q is to 0
#       find (b - a) which will be in inches 


# Udder Depth Linear Appraisal Score: (o, p and q)
#   Note: proportional adjustment made for miniature breeds
#   1 - 3 inches below hock
#   5 - 2 inches below hock
#   25 - 2 inches above hock
#   45 - 6 inches above hock

#     How to find:
#       a = minimum point of either curve at o (they have the same minimum)
#       b = hock height, r
#       find (a- b) which will be in inches 

library(ggplot2)


# Generate the left medial curve: m(x) = q*(x+p)*(x+(p+2)) + q - o
generate_left_curve <- function(o, p, q, l, n_points = 200) {
  x <- seq(-l, 0, length.out = n_points)
  y <- q * (x + p) * (x + (p + 2)) + q - o
  data.frame(x = x, y = y)
}

# Generate the right medial curve: n(x) = q*(x-p)*(x-(p+2)) + q - o
generate_right_curve <- function(o, p, q, l, n_points = 200) {
  x <- seq(0, l, length.out = n_points)
  y <- q * (x - p) * (x - (p + 2)) + q - o
  data.frame(x = x, y = y)
}

# Combined points for both halves
medial_df <- function(o, p, q, l, n_points = 200) {
  rbind(
    generate_left_curve(o, p, q, l, n_points),
    generate_right_curve(o, p, q, l, n_points)
  )
}


# Closed polygon for the full udder body, composed from the medial
# curves in this file and the udder arch (generate_arch) in
# udder_curve.R. The polygon is traversed bottom left -> bottom right
# via the medial curves, then top right -> top left via the reversed
# udder arch. Requires udder_curve.R to be sourced before this runs.
body_polygon_df <- function(med_o, med_p, med_q,
                            arch_a, arch_d, arch_s,
                            l, n_points = 200) {
  left  <- generate_left_curve(med_o, med_p, med_q, l, n_points)
  right <- generate_right_curve(med_o, med_p, med_q, l, n_points)
  arch  <- generate_arch(arch_a, arch_d, arch_s, l, n_points)

  arch_rev <- arch[nrow(arch):1, ]

  data.frame(
    x = c(left$x, right$x, arch_rev$x),
    y = c(left$y, right$y, arch_rev$y),
    group = "body"
  )
}


# --- Scoring functions ---

#   At x = 0 (where the two curves meet):
#     m(0) = q*p*(p+2) + q - o

#   So:
#     a (vertex, lowest point of udder floor) = -o
#     b (intersection at x=0, top of cleft)   = q*p*(p+2) + q - o
#     cleft_depth = b - a = q*p*(p+2) + q = q * (p+1)^2

# If cleft_depth <= 0 the curves are the same / flat / bulging:
#   score is 1-5, scaled by how close q is to 0 (the "curvy-ness")
#   each 1 inch of cleft = 10 points, anchored at 0 = 15


get_medial_score <- function(p, q) {
  cleft_depth <- q * (p + 1)^2
  
  if (cleft_depth <= 0) {
    # No real cleft / bulging: score 1-5
    # q = 0 means perfectly flat -> 5
    # q negative means bulging outward -> toward 1
    score <- max(1, min(5, 5 + cleft_depth * 5))
  } else {
    score <- 15 + cleft_depth * 10
  }
  
  return(max(1, min(50, round(score))))
}

#   much more simpler 
#   a = Vertex y of both curves = -o (the lowest point of the udder floor)
#   b = r
#   distance = b - a

get_udder_depth_score <- function(o, r) {
  # distance from udder floor vertex to hock (positive = above hock)
  distance <- r - o
  
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
# o_param = 13.0
# p_param = 1.0
# q_param = 0.15
# l_param = 5.0
# r_param = 14.3

# generate curve data

medial_visualization <- function(o_param, p_param, q_param, l_param, r_param) {

  left_df  <- generate_left_curve(o_param, p_param, q_param, l_param)
  right_df  <- generate_right_curve(o_param, p_param, q_param, l_param)
  medial_df <- rbind(left_df, right_df)

  head(left_df)
  tail(left_df)

  head(right_df)
  tail(right_df)


  ggplot(medial_df) +
    aes(x = x, y = y) +
    geom_point() +
    geom_hline(yintercept = -r_param, linetype = "dashed", color = "blue") 


  print("Medial Score:")
  print(get_medial_score(p_param, q_param))

  print("Udder Depth Score:")
  print(get_udder_depth_score(o_param, r_param))


}

if (sys.nframe() == 0) {
    medial_visualization(13.0, 1.0, 0.15, 5.0, 14.3)
  }