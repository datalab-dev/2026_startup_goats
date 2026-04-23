# pelvic arch
# rear udder hight trait is scored by assessing the halfway spot 
# between the base of the pelvic arch and the point of hock:
# midpoint, a score of 10 on the linear scale
# Top of milk secretory tissue in relation to the hock ½=10 points,
# 5/8 = 20 points, 3/4 = 30 points, 7/8 = 40 points, 
# at pelvic arch = 50 points

# desmos equations
# g(x) = -0.03x(x + 5) {0 > x > -l} ---> left arch
# v(x) = -0.03x(x - 5) {l > x > 0} ---> right arch
# y > g(x) 
# y > v(x)

# l: l = width between legs
# assumption that units are in inches

library(ggplot2)

pelvic_arch_df <- function(l) {
  l_x = seq(-l, 0, length.out = 200)
  left_arch_eq = -0.03 * l_x * (l_x + 5)
  left_arch <- data.frame(x = l_x, y = left_arch_eq)
  
  r_x = seq(0, l, length.out = 200)
  right_arch_eq = -0.03 * r_x * (r_x - 5)
  right_arch = data.frame(x = r_x, y = right_arch_eq)
  
  pelvic_arch <- rbind(left_arch, right_arch)
}
 
 # Closed polygon for the region above the pelvic arch, capped at top_y.
 pelvic_polygon_df <- function(l, top_y, n_points = 200) {
   x_left  <- seq(-l, 0, length.out = n_points)
   y_left  <- -0.03 * x_left  * (x_left  + 5)
 
   x_right <- seq(0, l, length.out = n_points)
   y_right <- -0.03 * x_right * (x_right - 5)
 
   data.frame(
     x = c(x_left, x_right, l, -l),
     y = c(y_left, y_right, top_y, top_y),
     group = "pelvic"
   )
 }
 

# drawing the pelvic arch
pelvic_arch_visualization <- function(l) {
  pelvic_arch <- pelvic_arch_df(l)
  
  ggplot(pelvic_arch) +
    aes(x, y) +
    geom_line()
}

# only run this when the script is ran directly
if(sys.nframe() == 0) {
  # setting l to the initial value of 4.6 as specified by the desmos visulalization
  pelvic_arch_visualization(4.6)
}
