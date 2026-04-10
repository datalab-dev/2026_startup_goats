# Udder Arch Draft

# Desmos equation 
# f(x) = -a(x + s)^(-1/2) + d{-l < x <0}
# m(x) < y < f(x)
# w(x) = -a(-x + s)^(-1/2) + d{l > x > 0}

# Parameters
# d = udder arch height
# a = udder arch (roundness)
# s = udder arch (attachment shape)

#
# a_param / a - arch drop
#d_param / d -  arch height
#s_param / s  -  attachment shape (must be > l if using this curve type)
#l_param / l- half width
#hock_param - reference line

library(ggplot2)

# Left side
generate_left_arch <- function(a, d, s, l, n_points = 300) {
  x <- seq(-l, 0, length.out = n_points)
  
  if (any(x + s <= 0)) {
    stop("Need x + s > 0. Make sure s > l.")
  }
  
  y <- -a * (x + s)^(-1/2) + d
  data.frame(x = x, y = y, side = "left")
}

# Right side
generate_right_arch <- function(a, d, s, l, n_points = 300) {
  x <- seq(0, l, length.out = n_points)
  
  if (any(-x + s <= 0)) {
    stop("Need -x + s > 0. Make sure s > l.")
  }
  
  y <- -a * (-x + s)^(-1/2) + d
  data.frame(x = x, y = y, side = "right")
}

# Combine both sides
generate_udder_arch <- function(a, d, s, l, n_points = 300) {
  left_df  <- generate_left_arch(a, d, s, l, n_points)
  right_df <- generate_right_arch(a, d, s, l, n_points)
  rbind(left_df, right_df)
}

# Plot
plot_udder_arch <- function(a, d, s, l, hock_height = 0, n_points = 300) {
  arch_df <- generate_udder_arch(a, d, s, l, n_points)
  
  ggplot(arch_df, aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_hline(yintercept = hock_height, linetype = "dashed", color = "blue") +
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

# Parameters
a_param <- 19
d_param <- 2
s_param <- 6.2
l_param <- 4.5
hock_param <- 0

# Create plot
p <- plot_udder_arch(
  a = a_param,
  d = d_param,
  s = s_param,
  l = l_param,
  hock_height = hock_param
)

print(p)