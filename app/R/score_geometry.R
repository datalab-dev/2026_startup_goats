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

# These linear formulas reproduce the ADGA linear-score tables exactly (see
# docs/linear score tables.pdf and the 2025 SOP Appendix, page 17) without
# pulling in scales::rescale.

# --- Goat size ---------------------------------------------------------------
# goat_size encodes the breed frame: 0 = normal/standard, 1 = dwarf/miniature.
# Per the SOP page-17 scales, every *absolute* udder measurement (udder depth,
# medial cleft, teat length, teat diameter) for a miniature is exactly half the
# standard value at the same linear score. The two *proportional* traits (rear
# udder height %, teat placement fraction) are scored relative to body frame, so
# they are breed-independent and ignore goat_size.
GOAT_SIZE_STANDARD <- 0
GOAT_SIZE_DWARF    <- 1

# inch-scaling factor applied to the absolute-measurement conversions.
goat_size_factor <- function(goat_size = GOAT_SIZE_STANDARD) {
  if (identical(as.numeric(goat_size), as.numeric(GOAT_SIZE_DWARF))) 0.5 else 1
}

# --- Score -> measurement ----------------------------------------------------

# Udder depth: lowest point of udder (excl. teats) above hock.
# standard:  10 -> -1", 20 -> 1", 30 -> 3", 40 -> 5", 50 -> 7"
# dwarf:     half of standard (15 -> 0", 25 -> 0.5", 35 -> 1", 45 -> 1.5")
score_to_udder_depth_inches <- function(s, goat_size = GOAT_SIZE_STANDARD) {
  check_score_in_valid_range(s)
  (s - 15) / 5 * goat_size_factor(goat_size)
}

# Rear udder height: udder arch position as % of hock->pelvic. Proportional —
# breed-independent, so no goat_size term.
# 10 -> 60%, 20 -> 70%, 30 -> 80%, 40 -> 90%, 50 -> 100%
score_to_rear_udder_height_pct <- function(s) {
  check_score_in_valid_range(s)
  (s + 50) / 100
}

# Medial suspensory ligament: cleft depth above the udder vertex.
# standard:  5 -> -1", 15 -> 0", 25 -> 1", 35 -> 2", 45 -> 3"
# dwarf:     half of standard (5 -> -0.5", 25 -> 0.5", 45 -> 1.5")
score_to_medial_inches <- function(s, goat_size = GOAT_SIZE_STANDARD) {
  check_score_in_valid_range(s)
  (s - 15) / 10 * goat_size_factor(goat_size)
}

# Teat length: vertex to teat-base center.
# standard:  10 -> 1", 20 -> 2", 30 -> 3", 40 -> 4", 50 -> 5"
# dwarf:     half of standard (50 -> 2.5")
score_to_teat_length_inches <- function(s, goat_size = GOAT_SIZE_STANDARD) {
  check_score_in_valid_range(s)
  s / 10 * goat_size_factor(goat_size)
}

# Teat diameter: width at the udder attachment.
# standard:  5 -> 0.5", 15 -> 1", 25 -> 1.5", 35 -> 2", 45 -> 2.5"
# dwarf:     half of standard (5 -> 0.25", 25 -> 0.75", 45 -> 1.25")
score_to_teat_diameter_inches <- function(s, goat_size = GOAT_SIZE_STANDARD) {
  check_score_in_valid_range(s)
  (s + 5) / 20 * goat_size_factor(goat_size)
}

# Teat placement: fraction of udder half that sits inside the teat
# center (1 = teat fully outside, 1/3 = teat well inside). Proportional —
# breed-independent, so no goat_size term.
# 5 -> 1, 15 -> 5/6, 25 -> 2/3, 35 -> 1/2, 45 -> 1/3
score_to_teat_placement_fraction <- function(s) {
  check_score_in_valid_range(s)
  1 - (s - 5) / 60
}

# Closeness of the udder halves, derived from the medial score: a higher score
# means a more sharply defined cleft. (Previously a fixed constant of 1.)
# 5 -> 0.25 (halves blend together) ... 45 -> 1.4 (well-separated halves)
score_to_closeness_of_halves <- function(s) {
  check_score_in_valid_range(s)
  0.25 + (s - 5) / (45 - 5) * (1.4 - 0.25)
}

# --- Measurement -> score (inverses) -----------------------------------------
# Each is the exact algebraic inverse of the matching score_to_* function, so
# round-tripping (score -> measurement -> score) returns the original score for
# both goat sizes. Results are NOT clamped to 1-50; clamp/round at the call site
# (e.g. via clamp_numeric in R/utils.R) if a valid linear score is required.

udder_depth_inches_to_score <- function(inches, goat_size = GOAT_SIZE_STANDARD) {
  inches / goat_size_factor(goat_size) * 5 + 15
}

rear_udder_height_pct_to_score <- function(pct) pct * 100 - 50

medial_inches_to_score <- function(inches, goat_size = GOAT_SIZE_STANDARD) {
  inches / goat_size_factor(goat_size) * 10 + 15
}

teat_length_inches_to_score <- function(inches, goat_size = GOAT_SIZE_STANDARD) {
  inches / goat_size_factor(goat_size) * 10
}

teat_diameter_inches_to_score <- function(inches, goat_size = GOAT_SIZE_STANDARD) {
  inches / goat_size_factor(goat_size) * 20 - 5
}

teat_placement_fraction_to_score <- function(fraction) 65 - 60 * fraction



# main function that takes in the linear appraisal scores, adjustable params, and converts them to physical measurements,
# then returns a list of all the measurements needed to draw the curves
scores_to_geometry <- function(scores, hock_height, leg_width, arch_leg_y,
                                arch_shape_pad = 1.5,
                                goat_size = GOAT_SIZE_STANDARD) {
  udder_depth_in   <- score_to_udder_depth_inches(scores$udder_depth_score, goat_size)
  rud_pct          <- score_to_rear_udder_height_pct(scores$rear_udder_height_score)
  medial_in        <- score_to_medial_inches(scores$medial_score, goat_size)
  teat_length_in   <- score_to_teat_length_inches(scores$teat_length_score, goat_size)
  teat_diameter_in <- score_to_teat_diameter_inches(scores$teat_diameter_score, goat_size)
  teat_place_frac  <- score_to_teat_placement_fraction(scores$teat_placement_score)

  # closeness of the udder halves now tracks the medial score (was a constant).
  closeness <- score_to_closeness_of_halves(scores$medial_score)

  # medial floor: medial vertex y = -hock_height + udder_depth_in,
  # so udder_floor_height (which the curve eq uses as -y_vertex) = hock - depth.
  udder_floor_height <- hock_height - udder_depth_in
  depth_of_medial    <- medial_in / (closeness + 1)^2

  # udder arch: vertex y from rear udder height, leg-intersection y from user.
  # arch_shape_pad (a user-controlled knob, > 0) sets shoulder roundness — a
  # larger pad gives a gentle, rounded dome; a smaller one steeper, pointier
  # shoulders. It only reshapes the curve between the vertex and arch_leg_y.
  arch_shape    <- leg_width + arch_shape_pad
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
