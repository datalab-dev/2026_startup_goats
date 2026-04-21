library(ggplot2)


# source all of the parts of the goat
source('R/leg_curve.R')
source('R/pelvic_curve.R')
source('R/medial_curve.R')
source('R/teats_curve.R')
source('R/udder_curve.R')


# shared boundaries matching the desmos reference
leg_width    <- 2
view_top     <- 3
view_bottom  <- -20


# these params come from the desmos reference
l_param = 4.3
r_param = 18.0

d_param = 2.0
a_param = 8.7
s_param = 6.0

o_param = 13.0
p_param = 1.0
q_param = 0.15

j_param = 2.5
u_param = 2
h_param = 4.5

# define the polygons for each part of the goat
teats_poly  <- teats_polygon_df(j = j_param, q = q_param, o = o_param, u = u_param, h = h_param,
                                o_body = o_param, p = p_param, q_body = q_param,
                                l_body = l_param)
legs_poly   <- legs_polygon_df(l = l_param, w = leg_width,
                               top_y = view_top, bot_y = view_bottom)
pelvic_poly <- pelvic_polygon_df(l = l_param, top_y = view_top)
body_poly   <- body_polygon_df(med_o = o_param, med_p = p_param, med_q = q_param,
                               arch_a = a_param, arch_d = d_param, arch_s = s_param,
                               l = l_param)

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
