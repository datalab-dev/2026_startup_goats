#
# functions that convert scores to inches
#

# calculates the teat length based off the corresponding score
score_to_teat_length <- function(score, goat_size = "standard") {
  if (!is.numeric(score)) {
    stop("Input must be numeric")
  }
  
  if (goat_size == "standard") {
    teat_length <- scales::rescale(score, to = c(0.5, 5.0), from = c(5, 50))
  } else if (goat_size == "miniature") {
    teat_length <- scales::rescale(score, to = c(0.25, 2.5), from = c(5, 50))
  } else {
    stop("goat_size must be 'standard' or 'miniature'")
  }
}

# calculates teat diameter measurement based off the corresponding score
score_to_teat_diameter <- function(score, goat_size = "standard") {
  if (!is.numeric(score)) {
    stop("Input must be numeric")
  }
  
  if (goat_size == "standard") {
    teat_diameter <- scales::rescale(score, to = c(0.5, 2.5), from = c(5, 45))
  } else if (goat_size == "miniature") {
    teat_diameter <- scales::rescale(score, to = c(0.25, 1.25), from = c(5, 45))
  } else {
    stop("goat_size must be 'standard' or 'miniature'")
  }
}

# not sure if this is biologically/mathematically correct? more testing needed
# calculates teat placement measurement based off the corresponding score 
# does teat placement score differ based on the goat size?
# SOP doesn't provide information about goat size in relation to teat placement
score_to_teat_placement <- function(score, leg_width = 4.6) {
  if (!is.numeric(c(score, leg_width))) {
    stop("Input must be numeric")
  }
  
  s <- (50 - score) / 49 # normalizing score
  
  teat_placement <- max(0, min(10, 0.94 * leg_width * sqrt(s)))
  teat_placement <- round(teat_placement, 1)
}


#
# functions that calculate the scores from the goat measurements
#

# returns the teat placement score given goat measurements
get_teat_placement_score <- function(teat_placement = 2.7, leg_width = 4.6) {
  if (!is.numeric(c(teat_placement, leg_width))) {
    stop("Input must be numeric")
  }
  leg_width <- max(0, min(20, leg_width))
  teat_placement <- max(0, min(10, teat_placement))
  
  score <- ( 1 - (teat_placement ^ 2) / (leg_width - (leg_width / 17.5)) ^ 2) * 50
  max(1, min(50, round(score)))
}

# returns the teat length score given goat measurements
get_teat_length_score <- function(teat_placement = 2.7, depth_of_medial = 0.15, 
                                  udder_floor_height = 13,
                                  teat_length = 1.4, teat_diameter = 3.5,
                                  closeness_of_halves = 1) {
  if (!is.numeric(c(teat_placement, depth_of_medial, udder_floor_height, 
                    teat_length, teat_diameter, closeness_of_halves))) {
    stop("Input must be numeric")
  }
  
  teat_input <- -teat_placement - 0.13
  teat <- teat_diameter * (teat_input  + teat_placement) *
    (teat_input  + (teat_roundness + teat_placement)) -
    (udder_floor_height + teat_length)
  
  medial_input <- -1 - closeness_of_halves
  medial <- depth_of_medial * (medial_input + closeness_of_halves) *
    (medial_input + (closeness_of_halves + 2)) +
    depth_of_medial - udder_floor_height
  
  score <- -10 * (teat - medial)
  max(1, min(50, round(score)))
}
