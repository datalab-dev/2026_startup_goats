# Page 6 - Export confirmation (success / failure variants).  HCI #1, #9
page6_ui <- function() {
  div(class = "page-enter",
    div(class = "card-panel",
      uiOutput("export_confirm")
    )
  )
}
