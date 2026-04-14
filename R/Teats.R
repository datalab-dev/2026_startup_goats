# TEATS EQUATIONS 

# Desmos Equations: 

# Left Teat
# f(x) = h(x + j)(x + (q + j)) - (o + u)

# Right Teat
# g(x) = h(x - j)(x - (q - j)) - (o + u)

# where:
# j is the distance from origin or x = 0, does halve the singular teat when j = 0
# q is the roundness of the teat, the sharpness (this is dependent on the medial)
# o is the length of the udder, also affects another equation (medial)
# o and q are discrete and I think independent, like doesn't affect the teats at all really? Just the placement in the$
# u is the length of the teat (not mentioned in the rating systems)
# h is the width of the teat 

library(tidyverse)

## Loading Some Data to Use/ Cleaning just for me. 

goats = read.csv("data/goats-la-data-cleaned.csv")

View(goats)

str(goats)

library(dplyr)

goats_subset = goats %>%
  select(Size, UdderDepth, Rear.Udder.Height, Rear.Udder.Arch, 
         Medial.Suspensory.Ligament, Teat.Placement, 
         Teat.Diameter, Teat.Length)

goats_subset

teats_subset = goats_subset %>% 
  select(Size, Teat.Placement, 
         Teat.Diameter, Teat.Length)

teats_subset

## Start working with the Equation:

# Expanded Equation
# t(x) = h − x² + jx + q + jo + u
# k(x) = h − x² − jx − q + jo + u

# m > y > t(x)
# n > y > k(x)

# parameters
o = 0      # udder height
p = 1.6    # spacing (used later for medial)
q = 0.3    # depth influence
j = 2.5    # placement
u = 2      # length
h = 1.5    # diameter/shape

# right teat

t_rfun <- function(x) {
  -x^2 + j*x + q + j*o + u
}

k_rfun <- function(x) {
  -x^2 + j*x + q + j*o
}

#left teat
t_lfun <- function(x) {
  -x^2 - j*x + q - j*o + u
}

k_lfun <- function(x) {
  -x^2 - j*x + q - j*o
}

# sample x values
x_vals = seq(-10, 10, length.out = 1000)

# create data for curves
curve_data = data.frame(
  x = c(x_vals, x_vals, x_vals, x_vals),
  y = c(
    t_rfun(x_vals),
    k_rfun(x_vals),
    t_lfun(x_vals),
    k_lfun(x_vals)
  ),
  curve = rep(c("t_right", "k_right", "t_left", "k_left"), each = length(x_vals))
)
  
# plot
ggplot(curve_data, aes(x, y, color = curve)) +
  geom_line(size = 1) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Teat Boundaries Only")