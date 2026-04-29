# pelvic arch
# rear udder height trait is scored by assessing the halfway spot 
# between the base of the pelvic arch and the point of hock:
# midpoint, a score of 10 on the linear scale
# Top of milk secretory tissue in relation to the hock 1/2=10 points,
# 5/8 = 20 points, 3/4 = 30 points, 7/8 = 40 points, 
# at pelvic arch = 50 points

# desmos equations
# g(x) = -0.03x(x + 5) {0 > x > -l} ---> left arch
# v(x) = -0.03x(x - 5) {l > x > 0} ---> right arch
# y > g(x) 
# y > v(x)

# l: l = width between legs, constrained to be from 0-20 inches
# assumption that units are in inches

library(ggplot2)

pelvic_arch_df <- function(width_between_legs = 4.6) {
  # type check of width_between_legs, should be an int/double
  if (!is.numeric(width_between_legs)) {
    stop("Input must be numeric")
  }
  
  # bounds check of width_between_legs, if out of bounds, constrain
  # to be within bounds
  if (width_between_legs < 0) {
    width_between_legs = 0
  }
  if (width_between_legs > 20) {
    width_between_legs = 20
  }
  
  # make the data frame
  # left side of pelvic arch
  left_values = seq(-width_between_legs, 0, length.out = 200) # 200 points to plot
  left_arch_eq = -0.03 * left_values * (left_values + 5)
  left_arch <- data.frame(x = left_values, y = left_arch_eq)
  
  # right side of pelvic arch
  right_values = seq(0, width_between_legs, length.out = 200) # 200 points to plot
  right_arch_eq = -0.03 * right_values * (right_values - 5)
  right_arch = data.frame(x = right_values, y = right_arch_eq)
  
  # combine and return
  pelvic_arch <- rbind(left_arch, right_arch)
}
 
# Closed polygon for the region above the pelvic arch, capped at top_y.
pelvic_polygon_df <- function(width_between_legs = 4.6, top_y = 3, n_points = 200) {
  # type check of input variables, should be an int/double
  if (!is.numeric(width_between_legs) | !is.numeric(top_y) | !is.numeric(n_points)) {
    stop("Input must be numeric")
  }
  
  # bounds check of width_between_legs, if out of bounds, constrain
  # to be within bounds
  if (width_between_legs < 0) {
    width_between_legs = 0
  }
  if (width_between_legs > 20) {
    width_between_legs = 20
  }
  
   x_left  <- seq(-width_between_legs, 0, length.out = n_points)
   y_left  <- -0.03 * x_left  * (x_left  + 5)
 
   x_right <- seq(0, width_between_legs, length.out = n_points)
   y_right <- -0.03 * x_right * (x_right - 5)
 
   data.frame(
     x = c(x_left, x_right, width_between_legs, -width_between_legs),
     y = c(y_left, y_right, top_y, top_y),
     group = "pelvic"
   )
}

# drawing a simple visualization of the pelvic arch
pelvic_arch_visualization <- function(width_between_legs = 4.6) {
  # type check of width_between_legs, should be an int/double
  if (!is.numeric(width_between_legs)) {
    stop("Input must be numeric")
  }
  
  # bounds check of width_between_legs, if out of bounds, constrain
  # to be within bounds
  if (width_between_legs < 0) {
    width_between_legs = 0
  }
  if (width_between_legs > 20) {
    width_between_legs = 20
  }
  
  # get dataframe to plot
  pelvic_arch <- pelvic_arch_df(width_between_legs)
  
  # plot the dataframe
  ggplot(pelvic_arch) +
    aes(x, y) +
    geom_line()
}

# only run this when the script is ran directly
if(sys.nframe() == 0) {
  # make the visualization using the default value of 4.6
  # as specified by demos plot
  pelvic_arch_visualization()
}
