# Shiny app launcher.
# global.R is auto-sourced by Shiny before this file runs.
# UI and server are kept in separate files for readability.

ui     <- source("ui.R",     local = TRUE)$value
server <- source("server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)
