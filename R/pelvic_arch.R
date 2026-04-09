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

# setting l to the initial value of 4.6 as specified by the desmos visulalization
l = 4.6

l_x = seq(-l, 0, length.out = 200)
left_arch_eq = -0.03 * l_x * (l_x + 5)
left_arch <- data.frame(x = l_x, y = left_arch_eq)

r_x = seq(0, l, length.out = 200)
right_arch_eq = -0.03 * r_x * (r_x - 5)
right_arch = data.frame(x = r_x, y = right_arch_eq)


pelvic_arch <- rbind(right_arch, left_arch)

# drawing the pelvic arch
ggplot(pelvic_arch) +
  aes(x, y) +
  geom_line()
