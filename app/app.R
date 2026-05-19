# Shiny app launcher.
# global.R is auto-sourced by Shiny before this file runs.
# UI and server are kept in separate files for readability.

<<<<<<< Updated upstream
ui <- source("ui.R")$value
server <- source("server.R")$value
=======
setwd("~/2026_startup_goats")
source('R/udder_curve.R')
source('R/leg_curve.R')
source('R/pelvic_curve.R')
source('R/medial_curve.R')
source('R/teats_curve.R')
>>>>>>> Stashed changes

shinyApp(ui = ui, server = server)
