# TEATS EQUATIONS 

# Desmos Equations: 

# Left Teat
# f(x) = h(x + j)(x + (q + j)) - (o + u)

# Right Teat
# g(x) = h(x - j)(x - (q - j)) - (o + u)

# where:
# j is the distance from origin or x = 0, does halve the singular teat when j = 0
# q is the roundness of the teat, the sharpness (this is dependent on the medial)
# o is where the udder starts from the (medial)
# o and q are discrete and I think independent, like doesn't affect the teats at all really? Just the placement in the$
# u is the length of the teat (not mentioned in the rating systems)
# h is the width of the teat 

library(tidyverse)

teat_model <- function(j, q, o, u, h, l, teat_length_score = NULL, n_points = 200) {
  
# converting teat scores to inches 
  if (!is.null(teat_length_score)) {
    if (teat_length_score == 50) u = 5.0
    else if (teat_length_score == 45) u = 4.5
    else if (teat_length_score == 40) u = 4.0
    else if (teat_length_score == 35) u = 3.5
    else if (teat_length_score == 30) u = 3.0
    else if (teat_length_score == 25) u = 2.5
    else if (teat_length_score == 20) u = 2.0
    else if (teat_length_score == 15) u = 1.5
    else if (teat_length_score == 10) u = 1.0
    else if (teat_length_score == 5)  u = 0.5
  }
  
  #equation 
  
  x_left  = seq(-l, 0, length.out = n_points)
  x_right = seq(0, l, length.out = n_points)
  
  y_left  = h * (x_left + j) * (x_left + (q + j)) - (o + u)
  y_right = h * (x_right - j) * (x_right - (q + j)) - (o + u)
  
  df = rbind(
    data.frame(x = x_left,  y = y_left),
    data.frame(x = x_right, y = y_right)
  )
  
 # plot
  ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    coord_fixed(xlim = c(-20, 20), ylim = c(-30, 10)) +
    theme_minimal()
}

if (sys.nframe() == 0) {
  teat_model(
    j = 2,
    q = 3,
    o = 5,
    u = 2,
    h = 1.5,
    l = 10
  )
}

# refined ver of the shiny UI 
# install.packages("shiny")

library(shiny)

ui = fluidPage(
  titlePanel("Teat Model"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("j", "Placement (j)", min = 0, max = 5, value = 2.5),
      sliderInput("q", "Roundness (q)", min = 0, max = 6, value = 3),
      sliderInput("o", "Medial Attachment (o)", min = 0, max = 20, value = 13),
      sliderInput("u", "Teat Length (u)", min = 0, max = 5, value = 2),
      sliderInput("h", "Width (h) (larger number = skinnier)", min = 0.1, max = 3, value = 1.5),
      sliderInput("l", "Boundary (l)", min = 5, max = 20, value = 10)
    ),
    
    mainPanel(
      plotOutput("teatPlot")
    )
  )
)

# server
server = function(input, output) {
  
  output$teatPlot = renderPlot({
    
    teat_model(
      j = input$j,
      q = input$q,
      o = input$o,
      u = input$u,
      h = input$h,
      l = input$l
    )
    
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

# Translating Points to Inches 

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



