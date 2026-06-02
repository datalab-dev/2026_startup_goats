library(shiny)
library(ggplot2)

# source all the curves and the UI helper
source("./R/leg_curve.R")
source("./R/pelvic_curve.R")
source("./R/udder_curve.R")
source("./R/medial_curve.R")
source("./R/teats_curve.R")
source("./R/ui_helpers.R")

view_top    <-  3
view_bottom <- -20

# default parameter values which the reset button will reset the input to
PARAM_DEFAULTS <- list(
  leg_width  = 4.5,
  hock_height = 18,
  arch_height = 2,
  arch_roundness = 8.7,
  arch_shape = 6,
  udder_floor_height  = 13,
  closeness_of_halves = 1,
  depth_of_medial = 0.15,
  teat_placement = 2.5,
  teat_length = 3,
  teat_diameter = 4.5
)

# bounds for each parameter. Min badge = light shade of part color,
# max badge = dark shade. Step is for the numeric input's keyboard arrows;
# the +/- buttons always move by ±0.5 per the Ag-GOAT notes.
PARAM_BOUNDS <- list(
  leg_width = list(min = 1,    max = 8,  step = 0.1),
  hock_height = list(min = 5,    max = 25, step = 0.1),
  arch_height = list(min = -5,   max = 10, step = 0.1),
  arch_roundness = list(min = 0.1,  max = 20, step = 0.1),
  arch_shape = list(min = 1,    max = 20, step = 0.1),
  udder_floor_height  = list(min = 5,    max = 25, step = 0.1),
  closeness_of_halves = list(min = 0,    max = 5,  step = 0.05),
  depth_of_medial = list(min = -1,   max = 1,  step = 0.01),
  teat_placement = list(min = 0.5,  max = 5,  step = 0.1),
  teat_length = list(min = 0.5,  max = 6,  step = 0.1),
  teat_diameter = list(min = 0.5,  max = 10, step = 0.1)
)

# Colors per body part — pulled from the graph polygon fills so the
# input cards visually match the polygons on the right.
#   legs   -> gray60 / black              (legs_poly)
#   udder  -> salmon / firebrick          (body_poly: arch portion)
#   medial -> same salmon / firebrick     (body_poly: floor portion)
#   teats  -> mediumpurple / mediumpurple3 (teats_poly)
SECTION_PALETTE <- list(
  legs   = list(light = "#d4d4d4", main = "#999999", dark = "#2e2e2e", border = "#666666"),
  udder  = list(light = "#ffd1c0", main = "#fa8072", dark = "#b22222", border = "#cc4444"),
  medial = list(light = "#ffd1c0", main = "#fa8072", dark = "#b22222", border = "#cc4444"),
  teats  = list(light = "#d8c5ed", main = "#9370db", dark = "#5d478b", border = "#7d5dbf")
)