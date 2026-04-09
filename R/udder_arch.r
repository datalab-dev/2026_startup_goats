
install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)

# -----------------------------
# Helper functions
# -----------------------------
rescale01 <- function(x, from = c(1, 50)) {
  (x - from[1]) / (from[2] - from[1])
}

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

# Rear udder arch generator
# low score = narrow + pointed
# high score = wide + rounded
make_rear_udder <- function(score_arch = 25,
                            score_height = 25,
                            score_medial = 25,
                            score_depth = 25,
                            n = 400) {
  
  arch01   <- rescale01(score_arch)
  height01 <- rescale01(score_height)
  medial01 <- rescale01(score_medial)
  depth01  <- rescale01(score_depth)
  
  half_width <- 2.2 + 2.8 * arch01
  top_y <- 6.5 + 3.5 * height01
  bottom_y <- 0.8 + 3.8 * depth01
  
  # low arch = pointier, high arch = rounder
  shape_exp <- 0.55 + 1.25 * arch01
  
  # stronger medial = deeper center cleft
  cleft_depth <- 0.15 + 1.15 * medial01
  cleft_width <- 0.35 + 0.18 * medial01
  
  x <- seq(-half_width, half_width, length.out = n)
  
  dome_base <- pmax(0, 1 - (abs(x) / half_width)^shape_exp)
  y_top <- bottom_y + (top_y - bottom_y) * dome_base
  
  floor_curve <- 0.28 * (abs(x) / half_width)^1.8
  medial_cleft <- cleft_depth * exp(-(x / cleft_width)^2)
  y_bottom <- bottom_y + floor_curve - medial_cleft
  
  y_bottom <- pmin(y_bottom, y_top - 0.25)
  
  outline <- bind_rows(
    tibble(x = x, y = y_top, part = "top"),
    tibble(x = rev(x), y = rev(y_bottom), part = "bottom")
  )
  
  list(
    outline = outline,
    top = tibble(x = x, y = y_top),
    bottom = tibble(x = x, y = y_bottom),
    params = list(
      half_width = half_width,
      top_y = top_y,
      bottom_y = bottom_y
    )
  )
}

plot_rear_udder <- function(score_arch = 25,
                            score_height = 25,
                            score_medial = 25,
                            score_depth = 25) {
  udder <- make_rear_udder(
    score_arch = score_arch,
    score_height = score_height,
    score_medial = score_medial,
    score_depth = score_depth
  )
  
  top_y <- udder$params$top_y
  half_width <- udder$params$half_width
  
  ggplot() +
    geom_polygon(
      data = udder$outline,
      aes(x = x, y = y),
      fill = "mistyrose2",
      color = "firebrick3",
      linewidth = 1.1,
      alpha = 0.85
    ) +
    geom_line(
      data = udder$top,
      aes(x = x, y = y),
      color = "firebrick4",
      linewidth = 1.2
    ) +
    geom_line(
      data = udder$bottom,
      aes(x = x, y = y),
      color = "firebrick4",
      linewidth = 1.2
    ) +
    geom_segment(aes(x = -half_width, xend = -half_width, y = top_y + 0.2, yend = -0.8),
                 linetype = "dashed", color = "gray40") +
    geom_segment(aes(x = half_width, xend = half_width, y = top_y + 0.2, yend = -0.8),
                 linetype = "dashed", color = "gray40") +
    geom_segment(aes(x = 0, xend = 0, y = top_y + 0.5, yend = -1.2),
                 color = "gray70") +
    coord_fixed() +
    labs(
      title = "Rear Udder Trainer Prototype",
      subtitle = paste(
        "Rear Udder Arch =", score_arch,
        "| Rear Udder Height =", score_height,
        "| Medial =", score_medial,
        "| Udder Depth =", score_depth
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank()
    )
}

ui <- fluidPage(
  titlePanel("Goat Udder Appraisal Trainer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("rear_arch", "Rear Udder Arch", min = 1, max = 50, value = 25),
      sliderInput("rear_height", "Rear Udder Height", min = 1, max = 50, value = 25),
      sliderInput("medial", "Medial Suspensory Ligament", min = 1, max = 50, value = 25),
      sliderInput("udder_depth", "Udder Depth", min = 1, max = 50, value = 25),
      fileInput("udder_photo", "Upload reference udder photo",
                accept = c("image/png", "image/jpeg", "image/jpg")),
      helpText("Move the sliders to test different trait values live.")
    ),
    mainPanel(
      plotOutput("udder_plot", height = "520px"),
      tags$hr(),
      h4("Reference photo"),
      imageOutput("real_photo")
    )
  )
)

server <- function(input, output, session) {
  output$udder_plot <- renderPlot({
    plot_rear_udder(
      score_arch = input$rear_arch,
      score_height = input$rear_height,
      score_medial = input$medial,
      score_depth = input$udder_depth
    )
  })
  
  output$real_photo <- renderImage({
    req(input$udder_photo)
    list(
      src = input$udder_photo$datapath,
      alt = "Uploaded goat udder photo",
      width = "500px"
    )
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
