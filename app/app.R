library(ggplot2)

# source all of the parts of the goat
source('R/leg_curve.R')
source('R/pelvic_curve.R')
source('R/medial_curve.R')
source('R/teats_curve.R')
source('R/udder_curve.R')

# shared boundaries matching the desmos reference
leg_width <- 2
view_top <- 3
view_bottom <- -20

# these params come from the desmos reference
# legs
leg_width <- 4.3
hock_height <- 18.0

# udder arch
udder_arch_height <- 2.0
udder_arch_roundness <- 8.7
attachment_shape <- 6.0

# medial
udder_floor_height <- 13.0
closeness_of_halves <- 1.0
depth_of_medial <- 0.15

# teats
teat_placement <- 2.5
teat_length <- 2
teat_diameter <- 4.5

# define the polygons for each part of the goat
teats_poly  <- teats_polygon_df(j = teat_placement, q = depth_of_medial, o = udder_floor_height,
                                u = teat_length, h = teat_diameter,
                                o_body = udder_floor_height, p = closeness_of_halves, q_body = depth_of_medial,
                                l_body = width_between_legs)
legs_poly   <- legs_polygon_df(l = width_between_legs, w = leg_width,
                               top_y = view_top, bot_y = view_bottom)
pelvic_poly <- pelvic_polygon_df(l = width_between_legs, top_y = view_top)
body_poly   <- body_polygon_df(udder_floor_height = udder_floor_height,
                               closeness_of_halves = closeness_of_halves,
                               depth_of_medial = depth_of_medial,
                               arch_a = udder_arch_roundness,
                               arch_d = udder_arch_height,
                               arch_s = attachment_shape,
                               leg_width = width_between_legs)

ggplot() +
  geom_polygon(data = teats_poly,
               aes(x = x, y = y, group = group),
               fill = "mediumpurple", color = "mediumpurple3",
               linetype = "dashed", linewidth = 0.6, alpha = 0.31) +

  geom_polygon(data = pelvic_poly,
               aes(x = x, y = y, group = group),
               fill = "steelblue", color = "steelblue",
               linewidth = 0.8, alpha = 0.45) +
  geom_polygon(data = body_poly,
               aes(x = x, y = y, group = group),
               fill = "salmon", color = "firebrick",
               linewidth = 0.8, alpha = 1.0) +
 geom_polygon(data = legs_poly,
               aes(x = x, y = y, group = group),
               fill = "gray60", color = "black",
               linetype = "dashed", linewidth = 0.6, alpha = 0.5) +
  coord_fixed(xlim = c(-8, 8), ylim = c(view_bottom, view_top), expand = FALSE) +
  theme_minimal() +
  labs(title = "Combined Goat Curves",
       x = "Horizontal position",
       y = "Vertical position") +
  geom_point(aes(x = 0, y = 0), color = "steelblue", size = 5) # pelvic arch point
