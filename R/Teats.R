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

generate_left_teat = function(j, q, o, u, h, l, n_points = 200) {
  x <- seq(-l, 0, length.out = n_points)
  y <- h * (x + j) * (x + (q + j)) - (o + u)
  data.frame(x = x, y = y)
}

generate_right_teat = function(j, q, o, u, h, l, n_points = 200) {
  x <- seq(0, l, length.out = n_points)
  y <- h * (x - j) * (x - (q + j)) - (o + u)
  data.frame(x = x, y = y)
}

teat_visualization <- function(j_param, q_param, o_param, u_param, h_param, l_param) {
  
  left_df  <- generate_left_teat(j_param, q_param, o_param, u_param, h_param, l_param)
  right_df <- generate_right_teat(j_param, q_param, o_param, u_param, h_param, l_param)
  
  teat_df <- rbind(left_df, right_df)
  
  ggplot(teat_df, aes(x = x, y = y)) +
    geom_line() +
    theme_minimal() + 
    xlim(-20, 20) + ylim(-30, 30)
}

teat_visualization(
  j_param = 5,
  q_param = 8,
  o_param = 13.0, 
  u_param = 2.0, # length
  h_param = 1.5, # Roundness (smaller = flatter)
  l_param = 10 # Leg Boundaries
)

# Wanted to make a slider to make things easier, chatgpt gave, honestly idk what's going on here: 

# install.packages("shiny")
library(shiny)

# --- UI (sliders) ---
ui <- fluidPage(
  titlePanel("Teat Model"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("j", "Placement (j)", min = 0, max = 5, value = 2.5),
      sliderInput("q", "Roundness (q)", min = 0, max = 2, value = 0.3),
      sliderInput("o", "Udder Height (o)", min = 0, max = 20, value = 13),
      sliderInput("u", "Teat Length (u)", min = 0, max = 5, value = 2),
      sliderInput("h", "Width (h)", min = 0.1, max = 3, value = 1.5),
      sliderInput("l", "Boundary (l)", min = 1, max = 10, value = 5)
    ),
    
    mainPanel(
      plotOutput("teatPlot")
    )
  )
)

# --- server (reactive plot) ---
server <- function(input, output) {
  
  output$teatPlot <- renderPlot({
    
    left_df  <- generate_left_teat(input$j, input$q, input$o, input$u, input$h, input$l)
    right_df <- generate_right_teat(input$j, input$q, input$o, input$u, input$h, input$l)
    
    df <- rbind(left_df, right_df)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      coord_fixed(xlim = c(-20, 20), ylim = c(-50, 30)) +
      theme_minimal()
  })
}

# run app
shinyApp(ui = ui, server = server)

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

# Translating Points to Inches (input in the ggplot)

# LINEAR SCALE – TEAT LENGTH
# Standard Numbers 

# 5.0”  50
# 4.5”  45
# 4.0”  40
# 3.5”  35
# 3.0”  30
# 2.5”  25
# 2.0”  20
# 1.5”  15
# 1.0”  10
# 0.5”  5

View(teats_subset)

trf_length = teats_subset$Teat.Length[1:5]/10
trf_length

# trf_dia = teats_subset$Teat.Diameter[1:5]

trf_plc = teats_subset$Teat.Placement



