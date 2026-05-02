# Udder Arch

# Desmos equation
# f(x) = -arch_roundness * (arch_shape - |x|)^(-1/2) + arch_height

# Parameters:
# arch_height        = vertical height of the arch (d in desmos)
# arch_roundness     = controls the curvature/steepness of the arch (a in desmos)
# arch_shape         = attachment shape; must be greater than leg_width
# leg_width = horizontal span of the arch

library(tidyverse)

check_num <- function(x) {
  if (is.null(x))     print("Argument is NULL")
  if (!is.numeric(x)) print("Argument is not numeric")
}

generate_arch <- function(arch_roundness, arch_height, arch_shape,
                          leg_width, n_points = 300) {
  check_num(arch_roundness)
  check_num(arch_height)
  check_num(arch_shape)
  check_num(leg_width)
  check_num(n_points)

  if (arch_shape <= leg_width) {
    print("Need arch_shape > leg_width so that arch_shape - |x| > 0 for all x.")
  }

  x <- seq(-leg_width, leg_width, length.out = n_points)
  y <- -arch_roundness * (arch_shape - abs(x))^(-1/2) + arch_height

  data.frame(x = x, y = y)
}

generate_and_plot_udder_arch <- function(arch_roundness, arch_height, arch_shape,
                                         leg_width, n_points = 300) {
  check_num(arch_roundness)
  check_num(arch_height)
  check_num(arch_shape)
  check_num(leg_width)
  check_num(n_points)

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

main <- function() {
  arch_roundness     <- 14
  arch_height        <- 14
  arch_shape         <- 3
  leg_width <- 2

  p <- generate_and_plot_udder_arch(arch_roundness, arch_height,
                                    arch_shape, leg_width)
  if (!is.null(p)) print(p)
}

if (sys.nframe() == 0) {
  main()
}
