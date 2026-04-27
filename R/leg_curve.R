library(tidyverse)
library(ggplot2)

#######
#DESMOS EQUATIONS
#######

#r = hock height
#l = width between legs

#left and right leg; effectively shade out two vertical 
#strips on the graph and l controls how far apart they are 

#  l+2>x>l
# -(l+2)<x<-l


#halfway point through knees; change in vertical position
#depending on r value and horizontal positions on l value 

# y=-r{l+2>x>l}
# y=-r{-(l+2)<x<-l}


#knee circles; simply circle equations that change in vertical position
#depending on r value and horizontal positions on l value 

# (x-(l+1))^{2}+(y+r)^{2}=.75^{2}
# (x+(l+1))^{2}+(y+r)^{2}=.75^{2}



#####
#PLOTTING
#####

# --- variables where l is width between legs and r is hock (rear knee) height ---
leg_gap_width <- 2
hock_height <- 0

# --- knee midline lines ---
df_segments <- data.frame(
  x    = c(leg_gap_width, -(leg_gap_width+2)),
  xend = c(leg_gap_width+2, -leg_gap_width),
  y    = c(-r, -r),
  yend = c(-r, -r)
)


# --- knee circles ---
theta <- seq(0, 2*pi, length.out = 300)

circle_right <- data.frame(
  x = (leg_gap_width+1) + 0.75 * cos(theta),
  y = -r + 0.75 * sin(theta)
)

circle_left <- data.frame(
  x = -(leg_gap_width+1) + 0.75 * cos(theta),
  y = -r + 0.75 * sin(theta)
)



#------vertical leg lines------
leg_height <- 20

df_legs <- data.frame(
  x = c((leg_gap_width+1)-0.75, (leg_gap_width+1)+0.75, -(leg_gap_width+1)-0.75, -(leg_gap_width+1)+0.75),
  xend = c((leg_gap_width+1)-0.75, (leg_gap_width+1)+0.75, -(leg_gap_width+1)-0.75, -(leg_gap_width+1)+0.75),
  y = -r - leg_height/2,
  yend = -r + leg_height/2
)



# --- plot the graph ---

ggplot() +
  geom_segment(data = df_segments, aes(x = x, xend = xend, y = y, yend = yend), linewidth = 1) +
  geom_path(data = circle_right, aes(x, y), linewidth = 1) +
  geom_path(data = circle_left, aes(x, y), linewidth = 1) +
  coord_equal() +
  geom_segment(data = df_legs, aes(x = x, xend = xend, y = y, yend = yend), linewidth = 1) +
  theme_minimal()



##############
#SHAPE FUNCTIONS
##############

#first try __________________________________________
#hockmidline <- function(r, leg_gap_width) {
  #df_segments <- data.frame(
   # x    = c(leg_gap_width, -(leg_gap_width+2)),
   # xend = c(leg_gap_width+2, -leg_gap_width),
   # y    = c(-r, -r),
   # yend = c(-r, -r))}


hockmidline <- function(r, leg_gap_width) {
  df_segments <- data.frame(
    x    = c(leg_gap_width, -(leg_gap_width+2)),
    xend = c(leg_gap_width+2, -leg_gap_width),
    y    = c(-r, -r),
    yend = c(-r, -r)
  )
  return(df_segments)
}

#first try __________________________________________
#hocks <- function(r, leg_gap_width) {
  #theta <- seq(0, 2*pi, length.out = 300)
  
 # circle_right <- data.frame(
  #  x = (leg_gap_width+1) + 0.75 * cos(theta),
   # y = -r + 0.75 * sin(theta))
  
  #circle_left <- data.frame(
   # x = -(leg_gap_width+1) + 0.75 * cos(theta),
  #  y = -r + 0.75 * sin(theta))}

hocks <- function(r, leg_gap_width) {
  theta <- seq(0, 2*pi, length.out = 300)
  
  circle_right <- data.frame(
    x = (leg_gap_width+1) + 0.75 * cos(theta),
    y = -r + 0.75 * sin(theta),
    side = "right")
  
  circle_left <- data.frame(
    x = -(leg_gap_width+1) + 0.75 * cos(theta),
    y = -r + 0.75 * sin(theta),
    side = "left")
  
  return(rbind(circle_right, circle_left))}

#first try ___________________________________________
#legs <- function(r, leg_gap_width, leg_height) {
#  df_legs <- data.frame(
#    x = c((leg_gap_width+1)-0.75, (leg_gap_width+1)+0.75, -(leg_gap_width+1)-0.75, -(leg_gap_width+1)+0.75),
#    xend = c((leg_gap_width+1)-0.75, (leg_gap_width+1)+0.75, -(leg_gap_width+1)-0.75, -(leg_gap_width+1)+0.75),
#    y = -r - leg_height/2,
#    yend = -r + leg_height/2)}

legs <- function(r, leg_gap_width, leg_height) {
  df_legs <- data.frame(
    x = c((leg_gap_width+1)-0.75, (leg_gap_width+1)+0.75, -(leg_gap_width+1)-0.75, -(leg_gap_width+1)+0.75),
    xend = c((leg_gap_width+1)-0.75, (leg_gap_width+1)+0.75, -(leg_gap_width+1)-0.75, -(leg_gap_width+1)+0.75),
    y = -r - leg_height/2,
    yend = -r + leg_height/2
  )
  return(df_legs)}




###################
# COMBINED FUNCTION
####################


full_legs <- function(r, leg_gap_width, leg_height) {
  list(
    midline = hockmidline(r, leg_gap_width),
    legs    = legs(r, leg_gap_width, leg_height),
    hocks   = hocks(r, leg_gap_width))}
