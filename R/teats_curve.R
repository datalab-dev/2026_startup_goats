# TEATS EQUATIONS 

# Desmos Equations: 

# Left Teat
# f(x) = h(x + j)(x + (q + j)) - (o + u)

# Right Teat
# g(x) = h(x - j)(x - (q - j)) - (o + u)

# where:
# j is the distance from origin or x = 0, does halve the singular teat when j = 0
# q is the roundness of the teat, the sharpness (this is dependent on the medial)
# o is where the udder starts from the (medial)
# o and q are discrete and I think independent, like doesn't affect the teats at all really? Just the placement in the$
# u is the length of the teat (not mentioned in the rating systems)
# h is the width of the teat 

library(tidyverse)

teat_model = function(j, q, o, u, h, l, teat_length_score = NULL, n_points = 200) {
  
# converting teat scores to inches 
  if (!is.null(teat_length_score)) {
    if (teat_length_score == 50) u = 5.0
    else if (teat_length_score == 45) u = 4.5
    else if (teat_length_score == 40) u = 4.0
    else if (teat_length_score == 35) u = 3.5
    else if (teat_length_score == 30) u = 3.0
    else if (teat_length_score == 25) u = 2.5
    else if (teat_length_score == 20) u = 2.0
    else if (teat_length_score == 15) u = 1.5
    else if (teat_length_score == 10) u = 1.0
    else if (teat_length_score == 5)  u = 0.5
  }
  
  #equation 
  
  x_left  = seq(-l, 0, length.out = n_points)
  x_right = seq(0, l, length.out = n_points)
  
  y_left  = h * (x_left + j) * (x_left + (q + j)) - (o + u)
  y_right = h * (x_right - j) * (x_right - (q + j)) - (o + u)
  
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

source("R/teats_curve.R")

