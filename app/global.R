library(shiny)
library(ggplot2)

# source all the curves, score-geometry layer, and the UI helper
source("./R/leg_curve.R")
source("./R/pelvic_curve.R")
source("./R/udder_curve.R")
source("./R/medial_curve.R")
source("./R/teats_curve.R")
source("./R/score_geometry.R")
source("./R/ui_helpers.R")

view_top    <-  3
view_bottom <- -24

# linear appraisal scores (1-50). Defaults pick mid-range values that
# produce a plausible standard-breed goat at the default adjustable params
SCORE_DEFAULTS <- list(
  udder_depth_score       = 30,  # 3" above hock
  rear_udder_height_score = 30,  # 80% of hock -> pelvic
  medial_score            = 25,  # 1" cleft
  teat_length_score       = 25,  # 2.5"
  teat_diameter_score     = 25,  # 1.5"
  teat_placement_score    = 35   # 1/2 (teat centered on udder half)
)

SCORE_BOUNDS <- list(
  udder_depth_score       = list(min = 1, max = 50, step = 1),
  rear_udder_height_score = list(min = 1, max = 50, step = 1),
  medial_score            = list(min = 1, max = 50, step = 1),
  teat_length_score       = list(min = 1, max = 50, step = 1),
  teat_diameter_score     = list(min = 1, max = 50, step = 1),
  teat_placement_score    = list(min = 1, max = 50, step = 1)
)

# cdjustable geometric params exposed to the user after the visual is
# created. Everything else is derived from the scores
ADJUSTABLE_DEFAULTS <- list(
  hock_height = 18,
  leg_width   = 4.5,
  arch_leg_y  = -5.5
)

ADJUSTABLE_BOUNDS <- list(
  hock_height = list(min = 5,    max = 25, step = 0.5),
  leg_width   = list(min = 1,    max = 8,  step = 0.1),
  arch_leg_y  = list(min = -15,  max = -1, step = 0.1)
)

# combined for the param_row helper (which keys off these lists).
PARAM_DEFAULTS <- c(SCORE_DEFAULTS, ADJUSTABLE_DEFAULTS)
PARAM_BOUNDS   <- c(SCORE_BOUNDS, ADJUSTABLE_BOUNDS)

SCORE_IDS      <- names(SCORE_DEFAULTS)
ADJUSTABLE_IDS <- names(ADJUSTABLE_DEFAULTS)

# colors per UI section. Scores card uses the same blue as the title
# (#559FD9); adjustable card uses the turquoise from the calc button.
SECTION_PALETTE <- list(
  scores     = list(light = "#cfe9ff", main = "#559FD9", dark = "#2e5d8b", border = "#559FD9"),
  adjustable = list(light = "#cef0e4", main = "#61D3B8", dark = "#3aa68b", border = "#4cbfa3"),
  legs       = list(light = "#d4d4d4", main = "#999999", dark = "#2e2e2e", border = "#666666"),
  udder      = list(light = "#ffd1c0", main = "#fa8072", dark = "#b22222", border = "#cc4444"),
  medial     = list(light = "#ffd1c0", main = "#fa8072", dark = "#b22222", border = "#cc4444"),
  teats      = list(light = "#d8c5ed", main = "#9370db", dark = "#5d478b", border = "#7d5dbf")
)
