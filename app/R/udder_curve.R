# Udder Arch

# Desmos equation
# f(x) = -arch_roundness * (arch_shape - |x|)^(-1/2) + arch_height

# Parameters:
# arch_height        = vertical height of the arch (d in desmos)
# arch_roundness     = controls the curvature/steepness of the arch (a in desmos)
# arch_shape         = attachment shape; must be greater than leg_width
# leg_width = horizontal span of the arch

library(tidyverse)

# checks that an input will exist and is numeric before it is used in calculations.
check_numeric_input <- function(x) {
  name <- deparse(substitute(x))
  
  if (!is.numeric(x)) {
    stop(name, " must be numeric")
  }
}

# source these utilities script into each one of these scripts so its easier to read 


# Checks that a linear appraisal score is numeric and within the valid 5-50 range.
check_score_in_valid_range <- function(score) {
  check_numeric_input(score)
  
  if (score < 5 || score > 50) {
    stop("score must be between 5 and 50")
  }
}

# scoring functions 

score_to_arch_roundness <- function(score) {
  scales::rescale(score, to = c(18, 4), from = c(5, 50))
}

score_to_arch_shape <- function(score, leg_width) {
  scales::rescale(score, to = c(leg_width + 0.25, leg_width + 4), from = c(5, 50))
}

score_to_arch_height <- function(score) {
  scales::rescale(score, to = c(12, 16), from = c(5, 50))
}

score_to_arch_inputs <- function(rear_udder_arch_score,
                                 rear_udder_height_score,
                                 leg_width) {
  check_score_in_valid_range(rear_udder_arch_score)
  check_score_in_valid_range(rear_udder_height_score)
  check_numeric_input(leg_width)
  
  list(
    arch_roundness = score_to_arch_roundness(rear_udder_arch_score),
    arch_shape     = score_to_arch_shape(rear_udder_arch_score, leg_width),
    arch_height    = score_to_arch_height(rear_udder_height_score)
  )
}

# arch generation

generate_arch <- function(arch_roundness, arch_height, arch_shape,
                          leg_width, n_points = 300) {
  check_numeric_input(arch_roundness)
  check_numeric_input(arch_height)
  check_numeric_input(arch_shape)
  check_numeric_input(leg_width)
  check_numeric_input(n_points)

  if (arch_shape <= leg_width) {
    stop("Need arch_shape > leg_width so that arch_shape - |x| > 0 for all x.")
  }
  
  x <- seq(-leg_width, leg_width, length.out = n_points)
  y <- -arch_roundness * (arch_shape - abs(x))^(-1/2) + arch_height

  data.frame(x = x, y = y)
}

generate_and_plot_udder_arch <- function(arch_roundness, arch_height, arch_shape,
                                         leg_width, n_points = 300) {
  check_numeric_input(arch_roundness)
  check_numeric_input(arch_height)
  check_numeric_input(arch_shape)
  check_numeric_input(leg_width)
  check_numeric_input(n_points)

  arch_df <- tryCatch(
    generate_arch(arch_roundness, arch_height, arch_shape, leg_width, n_points),
    error = function(e) {
      print("Error generating udder arch")
      print(e)
      return(NULL)
    }
  )

  if (is.null(arch_df)) return(NULL)

  ggplot(arch_df, aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_vline(xintercept = c(-leg_width, leg_width),
               linetype = "dotted") +
    coord_equal() +
    theme_minimal() +
    labs(
      title    = "Goat Udder Arch",
      subtitle = paste(
        "arch_roundness =", arch_roundness,
        "| arch_height =",  arch_height,
        "| arch_shape =",   arch_shape,
        "| leg_width =", leg_width
      ),
      x = "Horizontal position",
      y = "Vertical position"
    )
}

generate_and_plot_udder_arch_from_scores <- function(rear_udder_arch_score,
                                                     rear_udder_height_score,
                                                     leg_width,
                                                     n_points = 300) {
  params <- score_to_arch_inputs(
    rear_udder_arch_score   = rear_udder_arch_score,
    rear_udder_height_score = rear_udder_height_score,
    leg_width               = leg_width
  )
  
  generate_and_plot_udder_arch(
    arch_roundness = params$arch_roundness,
    arch_height    = params$arch_height,
    arch_shape     = params$arch_shape,
    leg_width      = leg_width,
    n_points       = n_points
  )
}

main <- function() {
  rear_udder_arch_score   <- 35
  rear_udder_height_score <- 25
  leg_width               <- 2
  
  p <- generate_and_plot_udder_arch_from_scores(
    rear_udder_arch_score   = rear_udder_arch_score,
    rear_udder_height_score = rear_udder_height_score,
    leg_width               = leg_width
  )
  
  if (!is.null(p)) print(p)
}

if (sys.nframe() == 0) {
  main()
}
