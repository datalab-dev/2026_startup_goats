library(ggplot2)

source('R/udder_curve.R')
source('R/leg_curve.R')
source('R/pelvic_curve.R')
source('R/medial_curve.R')
source('R/teats_curve.R')

view_top <-  3
view_bottom <- -20

# legs
leg_width <- 4.5
hock_height <- 18.0

# udder arch
arch_height <- 2.0
arch_roundness <- 8.7
arch_shape <- 6.0

# medial
udder_floor_height <- 13.0
closeness_of_halves <- 1.0
depth_of_medial <- 0.15

# teats
teat_placement <- 2.5
teat_length <- 3
teat_diameter <- 4.5
teat_roundness <- depth_of_medial

teats_poly <- teats_polygon_df(
  teat_placement      = teat_placement,
  teat_roundness      = teat_roundness,
  udder_floor_height  = udder_floor_height,
  teat_length         = teat_length,
  teat_diameter       = teat_diameter,
  leg_width  = leg_width,
  closeness_of_halves = closeness_of_halves,
  depth_of_medial     = depth_of_medial
)

legs_poly <- legs_polygon_df(
  leg_width = leg_width,
  hock_height = hock_height,
  top_y = view_top,
  bot_y = view_bottom
)

pelvic_poly <- pelvic_polygon_df(
  leg_width = leg_width,
  top_y = view_top
)

body_poly <- body_polygon_df(
  udder_floor_height = udder_floor_height,
  closeness_of_halves = closeness_of_halves,
  depth_of_medial = depth_of_medial,
  arch_roundness = arch_roundness,
  arch_height = arch_height,
  arch_shape = arch_shape,
  leg_width = leg_width
)

ggplot() +
  geom_polygon(data = teats_poly,
               aes(x = x, y = y, group = group),
               fill = "mediumpurple", color = "mediumpurple3",
               linetype = "solid", linewidth = 1, alpha = 0.31) +
  geom_polygon(data = pelvic_poly,
               aes(x = x, y = y, group = group),
               fill = "steelblue", color = "steelblue",
               linewidth = 1, alpha = 0.45) +
  geom_polygon(data = body_poly,
               aes(x = x, y = y, group = group),
               fill = "salmon", color = "firebrick",
               linewidth = 1, alpha = 1.0) +
  geom_polygon(data = legs_poly,
               aes(x = x, y = y, group = group),
               fill = "gray60", color = "black",
               linetype = "solid", linewidth = 0.4, alpha = 0.5) +
  coord_fixed(xlim = c(-8, 8), ylim = c(view_bottom, view_top), expand = FALSE) +
  theme_minimal() +
  labs(title = "Combined Goat Curves",
       x = "Horizontal position",
       y = "Vertical position") +
  geom_point(aes(x = 0, y = 0), color = "steelblue", size = 5)
