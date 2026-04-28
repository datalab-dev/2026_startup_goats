# TEATS EQUATIONS 

# Desmos Equations: 

# Left Teat
# f(x) = h(x + j)(x + (q + j)) - (o + u)

# Right Teat
# g(x) = h(x - j)(x - (q - j)) - (o + u)

library(tidyverse)

#function with default values 

teat_model = function(
    teat_placement = 2,
    depth_of_medial = 3,
    udder_floor_height = 5,
    teat_length = 2,
    teat_diameter = 1,
    leg_width = 10,
    teat_length_score = NULL,
    n_points = 200
) {
  
  # input validation (defensive programming)
  check_num = function(x, name) {
    if (is.null(x)) stop(paste(name, "Input is NULL"))
    if (!is.numeric(x)) stop(paste(name, "Input must be numeric."))
  }
  
  # validate all inputs
  check_num(teat_placement, "teat_placement")
  check_num(depth_of_medial, "depth_of_medial")
  check_num(udder_floor_height, "udder_floor_height")
  check_num(teat_length, "teat_length")
  check_num(teat_diameter, "teat_diameter")
  check_num(leg_width, "leg_width")
  check_num(n_points, "n_points")
  
# converting teat scores to inches 
  if (!is.null(teat_length_score)) {
    if (teat_length_score == 50) teat_length = 5.0
    else if (teat_length_score == 45) teat_length = 4.5
    else if (teat_length_score == 40) teat_length = 4.0
    else if (teat_length_score == 35) teat_length = 3.5
    else if (teat_length_score == 30) teat_length = 3.0
    else if (teat_length_score == 25) teat_length = 2.5
    else if (teat_length_score == 20) teat_length = 2.0
    else if (teat_length_score == 15) teat_length = 1.5
    else if (teat_length_score == 10) teat_length = 1.0
    else if (teat_length_score == 5)  teat_length = 0.5
  }
  
  #equation 
  
  x_left  = seq(-leg_width, 0, length.out = n_points)
  x_right = seq(0, leg_width, length.out = n_points)
  
  y_left  = teat_diameter * (x_left + teat_placement) * (x_left + (depth_of_medial + teat_placement)) - (udder_floor_height + teat_length)
  y_right = teat_diameter * (x_right - teat_placement) * (x_right - (depth_of_medial + teat_placement)) - (udder_floor_height + teat_length)
  
  df = rbind(
    data.frame(x = x_left,  y = y_left),
    data.frame(x = x_right, y = y_right)
  )
  
 # plot
  ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    coord_fixed(xlim = c(-20, 20), ylim = c(-30, 10)) +
    theme_minimal()
}

if (sys.nframe() == 0) {
  teat_model()
  }