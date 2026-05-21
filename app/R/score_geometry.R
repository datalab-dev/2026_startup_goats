# Created as an intermediary file between the linear appraisal scores and the curve functions. 
# The curve functions expect physical measurements (inches, percentages) and the linear appraisal scores are unitless, thus 
# this file contains functions that convert the linear appraisal scores to physical measurements


# Coordinate system (shared across the app):
#   x = 0           - midline of animal
#   y = 0           - pelvic arch (top)
#   y = -hock_height - hock line (bottom)

# Vertical assembly order, following Ag-GOAT notes:
#   1. Hock at (0, -hock_height)
#   2. Medial vertex = hock + udder_depth_in (upward)
#   3. Medial cleft  = medial vertex + medial_in (upward)
#   4. Udder arch vertex sits rear_udder_height_pct of the way from
#      hock to pelvic arch
#   5. Arch <-> leg intersection y is user-controllable; arch_roundness
#      and arch_height are solved so the inverse-sqrt arch passes
#      through both the vertex and (+/- leg_width, arch_leg_y).

# I NOTICED THAT THIS WAS AN EASIER WAY TO DO THIS, THAN USING THE SCALES PACKAGE
# I hope this is right but I'm pretty sure it works

# Udder depth: lowest point of udder (excl. teats) above hock.
# 10 -> -1", 20 -> 1", 30 -> 3", 40 -> 5", 50 -> 7"
score_to_udder_depth_inches <- function(s) (s - 15) / 5

# Rear udder height: udder arch position as % of hock->pelvic.
# 10 -> 60%, 20 -> 70%, 30 -> 80%, 40 -> 90%, 50 -> 100%
score_to_rear_udder_height_pct <- function(s) (s + 50) / 100

# Medial suspensory ligament: cleft depth above the udder vertex.
# 5 -> -1", 15 -> 0", 25 -> 1", 35 -> 2", 45 -> 3"
score_to_medial_inches <- function(s) (s - 15) / 10

# Teat length: vertex to teat-base center.
# 10 -> 1", 20 -> 2", 30 -> 3", 40 -> 4", 50 -> 5"
score_to_teat_length_inches <- function(s) s / 10

# Teat diameter: width at the udder attachment.
# 5 -> 0.5", 15 -> 1", 25 -> 1.5", 35 -> 2", 45 -> 2.5"
score_to_teat_diameter_inches <- function(s) (s + 5) / 20

# Teat placement: fraction of udder half that sits inside the teat
# center (1 = teat fully outside, 1/3 = teat well inside)
# thus this means:
# 5 -> 1, 15 -> 5/6, 25 -> 2/3, 35 -> 1/2, 45 -> 1/3
score_to_teat_placement_fraction <- function(s) 1 - (s - 5) / 60



# defines how close the medial halves are to each other. This is a constant value for now, but could be adjustable?
# depth_of_medial is uniquely determined by the medial score
CLOSENESS_OF_HALVES_DEFAULT <- 1

# inverse-sqrt udder arch needs arch_shape > leg_width, but some padding was neecessary to keep the curve well-defined 
# and avoid singular  edges
ARCH_SHAPE_PAD <- 1.5


# main function that takes in the linear appraisal scores, adjustable params, and converts them to physical measurements, 
# then returns a list of all the measurements needed to draw the curves
scores_to_geometry <- function(scores, hock_height, leg_width, arch_leg_y) {
  udder_depth_in   <- score_to_udder_depth_inches(scores$udder_depth_score)
  rud_pct          <- score_to_rear_udder_height_pct(scores$rear_udder_height_score)
  medial_in        <- score_to_medial_inches(scores$medial_score)
  teat_length_in   <- score_to_teat_length_inches(scores$teat_length_score)
  teat_diameter_in <- score_to_teat_diameter_inches(scores$teat_diameter_score)
  teat_place_frac  <- score_to_teat_placement_fraction(scores$teat_placement_score)

  closeness <- CLOSENESS_OF_HALVES_DEFAULT

  # medial floor: medial vertex y = -hock_height + udder_depth_in,
  # so udder_floor_height (which the curve eq uses as -y_vertex) = hock - depth.
  udder_floor_height <- hock_height - udder_depth_in
  depth_of_medial    <- medial_in / (closeness + 1)^2

  # udder arch: vertex y from rear udder height, leg-intersection y from user.
  arch_shape    <- leg_width + ARCH_SHAPE_PAD
  arch_vertex_y <- -hock_height * (1 - rud_pct)

  # for arch_roundness, arch_height given y(0) = arch_vertex_y and
  # y(leg_width) = arch_leg_y.
  inv_sqrt_inner <- 1 / sqrt(arch_shape - leg_width)  # at x = leg_width
  inv_sqrt_outer <- 1 / sqrt(arch_shape)              # at x = 0
  # roundness defined as the vertical distance from the vertex to the arch at x = leg_width, so
  arch_roundness <- (arch_vertex_y - arch_leg_y) / (inv_sqrt_inner - inv_sqrt_outer)
  # height defined as the y-value of the arch at x = 0, so
  arch_height    <- arch_vertex_y + arch_roundness * inv_sqrt_outer

  # teat x center defined by the width of the leg and the ratio of the teat to leg_width placement
  teat_x_center <- leg_width * teat_place_frac

  # all of this gets passed to the curve functions, which are responsible for drawing the curves, in server.R
  list(
    udder_floor_height  = udder_floor_height,
    closeness_of_halves = closeness,
    depth_of_medial     = depth_of_medial,
    arch_shape          = arch_shape,
    arch_roundness      = arch_roundness,
    arch_height         = arch_height,
    arch_vertex_y       = arch_vertex_y,
    teat_x_center       = teat_x_center,
    teat_diameter_in    = teat_diameter_in,
    teat_length_in      = teat_length_in,
    hock_height         = hock_height,
    leg_width           = leg_width,
    arch_leg_y          = arch_leg_y
  )
}
