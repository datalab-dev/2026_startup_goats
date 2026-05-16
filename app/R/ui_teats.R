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

# function to run the app
run_teat_app = function() {
  shinyApp(ui = ui, server = server)
}

# run the shiny app only when this script is executed directly
if (sys.nframe() == 0) {
  run_teat_app()
}