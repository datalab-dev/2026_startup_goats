# Udder Arch Condensed

# Desmos equation 
# f(x) = -a(x + s)^(-1/2) + d{-l < x <0}
# m(x) < y < f(x)
# w(x) = -a(-x + s)^(-1/2) + d{l > x > 0}
# n(x) < y < w(x)

# OLD Parameters
# d = udder arch height
# a = udder arch (roundness)
# s = udder arch (attachment shape)

# NEW Parameters 
# arch_roundness - a value
# arch_height - d value
# arch_shape - s value
# inner_leg - l value 

library(tidyverse)

# Defensive check, this will check if argument must be non-null and numeric
check_num <- function(x) {
  if (is.null(x)) print("Argument is NULL")
  if (!is.numeric(x)) print("Argument is not numeric")
}

generate_arch <- function(arch_roundness, arch_height, arch_shape, inner_leg, n_points = 300) {
  
  check_num(arch_roundness)
  check_num(arch_height)
  check_num(arch_shape)
  check_num(inner_leg)
  check_num(n_points)
  
  if (arch_shape <= inner_leg) {
    print("Need arch_shape > inner_leg so that s - |x| > 0 for all x.")
  }
  
  x <- seq(-inner_leg, inner_leg, length.out = n_points)
  
  y <- -arch_roundness * (arch_shape - abs(x))^(-1/2) + arch_height
  
  data.frame(x = x, y = y)
}

generate_and_plot_udder_arch <- function(arch_roundness, arch_height, arch_shape, inner_leg, 
                                         n_points = 300) {
  
  check_num(arch_roundness)
  check_num(arch_height)
  check_num(arch_shape)
  check_num(inner_leg)
  check_num(n_points)
  
  arch_df <- tryCatch( # AI recommended me use tryCatch, from my understanding it will print a message + return NULL
    generate_arch(arch_roundness, arch_height, arch_shape, inner_leg, n_points),
    error = function(e) {
      print("Error generating udder arch")
      print(e)
      return(NULL)
    }
  )
  
  if (is.null(arch_df)) {
    return(NULL)
  }
  
  ggplot(arch_df, aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_vline(xintercept = c(-inner_leg, inner_leg), linetype = "dotted") +
    coord_equal() +
    theme_minimal() +
    labs(
      title = "Goat Udder Arch",
      subtitle = paste(
        "roundness =", arch_roundness,
        "| height =", arch_height,
        "| shape =", arch_shape,
        "| inner leg =", inner_leg
      ),
      x = "Horizontal position",
      y = "Vertical position"
    )
}

main <- function() {
  arch_roundness_param <- 14
  arch_height_param <- 14
  arch_shape_param <- 3
  inner_leg_param <- 2
  
  p <- generate_and_plot_udder_arch(
    arch_roundness = arch_roundness_param,
    arch_height = arch_height_param,
    arch_shape = arch_shape_param,
    inner_leg = inner_leg_param
  )
  
  if (!is.null(p)) {
    print(p)
  }
}


if (sys.nframe() == 0) {
  main()
}