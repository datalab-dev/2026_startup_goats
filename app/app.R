# Shiny app launcher.
# global.R is auto-sourced by Shiny before this file runs.
# UI and server are kept in separate files for readability.

ui <- source("ui.R")$value
server <- source("server.R")$value

shinyApp(ui = ui, server = server)