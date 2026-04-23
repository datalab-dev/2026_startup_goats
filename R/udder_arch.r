# Udder Arch Condensed

# Desmos equation 
# f(x) = -a(x + s)^(-1/2) + d{-l < x <0}
# m(x) < y < f(x)
# w(x) = -a(-x + s)^(-1/2) + d{l > x > 0}
# n(x) < y < w(x)

# Parameters
# d = udder arch height
# a = udder arch (roundness)
# s = udder arch (attachment shape)

# a_param / a - arch roundness
# d_param / d -  arch height
# s_param / s  -  udder arch shape (must be > l if using this curve type)
# l_param / l- inner leg part where it stops
# pelvic arch - blue point representing the origin 


generate_arch <- function(a, d, s, l, n_points = 300) {
  
  x <- seq(-l, l, length.out = n_points)
  
  if (any(s - abs(x) <= 0)) {
    stop("Need s > l so that s - |x| > 0 for all x.")
  }
  
  y <- -a * (s - abs(x))^(-1/2) + d
  
  side <- ifelse(x < 0, "left", "right")
  
  data.frame(x = x, y = y, side = side)
}

generate_and_plot_udder_arch <- function(a, d, s, l, n_points = 300) {
  
  arch_df <- generate_arch(a, d, s, l, n_points)
  
  ggplot(arch_df, aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_vline(xintercept = c(-l, l), linetype = "dotted") +
    coord_equal() +
    theme_minimal() +
    labs(
      title = "Goat Udder Arch",
      subtitle = paste("a =", a, "| d =", d, "| s =", s, "| l =", l),
      x = "Horizontal position",
      y = "Vertical position"
    )
}

main <- function() {
  a_param <- 16
  d_param <- 14
  s_param <- 3
  l_param <- 2
  
  p <- generate_and_plot_udder_arch(a_param, d_param, s_param, l_param)
  print(p)
}

if (sys.nframe() == 0) {
  main()
}

