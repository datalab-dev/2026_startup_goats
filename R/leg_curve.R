library(tidyverse)
library(ggplot2)

#######
# DESMOS EQUATIONS
#######

# hock_height       = height of the rear knee joint above the ground baseline
# leg_width = horizontal distance between the legs

# Left and right leg strips:
#   leg_width + 2 > x > leg_width
#   -(leg_width + 2) < x < -leg_width

# Knee midline (horizontal at hock level):
#   y = -hock_height  { leg_width + 2 > x > leg_width }
#   y = -hock_height  { -(leg_width + 2) < x < -leg_width }

# Knee circles at (±(leg_width + 1), -hock_height) with radius 0.75:
#   (x - (leg_width + 1))^2 + (y + hock_height)^2 = 0.75^2
#   (x + (leg_width + 1))^2 + (y + hock_height)^2 = 0.75^2


#####
# SHAPE FUNCTIONS
#####

hockmidline <- function(hock_height, leg_width) {
  data.frame(
    x    = c(leg_width,  -(leg_width + 2)),
    xend = c(leg_width + 2, -leg_width),
    y    = c(-hock_height, -hock_height),
    yend = c(-hock_height, -hock_height)
  )
}

hocks <- function(hock_height, leg_width) {
  theta <- seq(0, 2 * pi, length.out = 300)

  right <- data.frame(
    x    = (leg_width + 1) + 0.75 * cos(theta),
    y    = -hock_height + 0.75 * sin(theta),
    side = "right"
  )
  left <- data.frame(
    x    = -(leg_width + 1) + 0.75 * cos(theta),
    y    = -hock_height + 0.75 * sin(theta),
    side = "left"
  )
  rbind(right, left)
}

legs <- function(hock_height, leg_width, leg_height) {
  cx_r <-  leg_width + 1
  cx_l <- -(leg_width + 1)
  data.frame(
    x    = c(cx_r - 0.75, cx_r + 0.75, cx_l - 0.75, cx_l + 0.75),
    xend = c(cx_r - 0.75, cx_r + 0.75, cx_l - 0.75, cx_l + 0.75),
    y    = -hock_height - leg_height / 2,
    yend = -hock_height + leg_height / 2
  )
}


###################
# POLYGON FUNCTION
###################

# Closed rectangular polygons for both legs spanning the full vertical range.
# The inner edges sit at x = ±leg_width (matching the pelvic polygon
# boundary) and each leg is 2 units wide, matching the Desmos strip definition.
legs_polygon_df <- function(leg_width, hock_height = 18, top_y = 3, bot_y = -20) {
  if (!is.numeric(leg_width) || !is.numeric(hock_height) ||
      !is.numeric(top_y) || !is.numeric(bot_y)) {
    stop("All inputs must be numeric")
  }

  right_leg <- data.frame(
    x     = c(leg_width,     leg_width + 2,
              leg_width + 2, leg_width),
    y     = c(bot_y, bot_y, top_y, top_y),
    group = "right_leg"
  )

  left_leg <- data.frame(
    x     = c(-(leg_width + 2), -leg_width,
              -leg_width,        -(leg_width + 2)),
    y     = c(bot_y, bot_y, top_y, top_y),
    group = "left_leg"
  )

  rbind(right_leg, left_leg)
}


###################
# COMBINED FUNCTION
###################

full_legs <- function(hock_height = 18, leg_width = 4.6, leg_height = 20) {
  if (!is.numeric(leg_width) ||
      !is.numeric(hock_height) ||
      !is.numeric(leg_height)) {
    stop("Input must be numeric")
  }

  list(
    midline = hockmidline(hock_height, leg_width),
    legs    = legs(hock_height, leg_width, leg_height),
    hocks   = hocks(hock_height, leg_width)
  )
}


if (sys.nframe() == 0) {
  full_legs()
}
