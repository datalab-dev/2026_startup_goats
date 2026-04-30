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
width_between_legs <- 2
hock_height <- 0

# --- knee midline lines ---
df_segments <- data.frame(
  x    = c(width_between_legs, -(width_between_legs+2)),
  xend = c(width_between_legs+2, -width_between_legs),
  y    = c(-hock_height, -hock_height),
  yend = c(-hock_height, -hock_height)
)


# --- knee circles ---
theta <- seq(0, 2*pi, length.out = 300)

circle_right <- data.frame(
  x = (width_between_legs+1) + 0.75 * cos(theta),
  y = -hock_height + 0.75 * sin(theta)
)

circle_left <- data.frame(
  x = -(width_between_legs+1) + 0.75 * cos(theta),
  y = -hock_height + 0.75 * sin(theta)
)



#------vertical leg lines------
leg_height <- 20

df_legs <- data.frame(
  x = c((width_between_legs+1)-0.75, (width_between_legs+1)+0.75, -(width_between_legs+1)-0.75, -(width_between_legs+1)+0.75),
  xend = c((width_between_legs+1)-0.75, (width_between_legs+1)+0.75, -(width_between_legs+1)-0.75, -(width_between_legs+1)+0.75),
  y = -hock_height - leg_height/2,
  yend = -hock_height + leg_height/2
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
#hockmidline <- function(hock_height, width_between_legs) {
  #df_segments <- data.frame(
   # x    = c(width_between_legs, -(width_between_legs+2)),
   # xend = c(width_between_legs+2, -width_between_legs),
   # y    = c(-hock_height, -hock_height),
   # yend = c(-hock_height, -hock_height))}


hockmidline <- function(hock_height, width_between_legs) {
  df_segments <- data.frame(
    x    = c(width_between_legs, -(width_between_legs+2)),
    xend = c(width_between_legs+2, -width_between_legs),
    y    = c(-hock_height, -hock_height),
    yend = c(-hock_height, -hock_height)
  )
  return(df_segments)
}

#first try __________________________________________
#hocks <- function(hock_height, width_between_legs) {
  #theta <- seq(0, 2*pi, length.out = 300)
  
 # circle_right <- data.frame(
  #  x = (width_between_legs+1) + 0.75 * cos(theta),
   # y = -hock_height + 0.75 * sin(theta))
  
  #circle_left <- data.frame(
   # x = -(width_between_legs+1) + 0.75 * cos(theta),
  #  y = -hock_height + 0.75 * sin(theta))}

hocks <- function(hock_height, width_between_legs) {
  theta <- seq(0, 2*pi, length.out = 300)
  
  circle_right <- data.frame(
    x = (width_between_legs+1) + 0.75 * cos(theta),
    y = -hock_height + 0.75 * sin(theta),
    side = "right")
  
  circle_left <- data.frame(
    x = -(width_between_legs+1) + 0.75 * cos(theta),
    y = -hock_height + 0.75 * sin(theta),
    side = "left")
  
  return(rbind(circle_right, circle_left))}

#first try ___________________________________________
#legs <- function(hock_height, width_between_legs, leg_height) {
#  df_legs <- data.frame(
#    x = c((width_between_legs+1)-0.75, (width_between_legs+1)+0.75, -(width_between_legs+1)-0.75, -(width_between_legs+1)+0.75),
#    xend = c((width_between_legs+1)-0.75, (width_between_legs+1)+0.75, -(width_between_legs+1)-0.75, -(width_between_legs+1)+0.75),
#    y = -hock_height - leg_height/2,
#    yend = -hock_height + leg_height/2)}

legs <- function(hock_height, width_between_legs, leg_height) {
  df_legs <- data.frame(
    x = c((width_between_legs+1)-0.75, (width_between_legs+1)+0.75, -(width_between_legs+1)-0.75, -(width_between_legs+1)+0.75),
    xend = c((width_between_legs+1)-0.75, (width_between_legs+1)+0.75, -(width_between_legs+1)-0.75, -(width_between_legs+1)+0.75),
    y = -hock_height - leg_height/2,
    yend = -hock_height + leg_height/2
  )
  return(df_legs)}




###################
# COMBINED FUNCTION
####################


full_legs <- function(hock_height = 18, width_between_legs = 4.6, leg_height = 20) {
#^ set defaults right away (they are from the desmos calculator - average/expected values?)
  
  # check variable input is appropriate
  if (!is.numeric(width_between_legs) || 
      !is.numeric(hock_height) || 
      !is.numeric(leg_height)) {
    stop("Input must be numeric")}
  
  # return list of other functions
  list(
    midline = hockmidline(hock_height, width_between_legs),
    legs    = legs(hock_height, width_between_legs, leg_height),
    hocks   = hocks(hock_height, width_between_legs))}


#"sourcing guard" as per Rash
if(sys.nframe() == 0) {
  full_legs()
}

