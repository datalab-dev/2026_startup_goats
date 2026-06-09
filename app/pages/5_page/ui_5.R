# Page 5 - observation Added and Review Page
page5_ui <- function() {
  div(class = "page-enter",
    div(class = "card-panel",
      span(class = "blob blob-tr"),
      h2(class = "page-title", "Observation added"),
      uiOutput("obs_summary"),
      uiOutput("obs_review"),

      div(class = "btn-row between", style = "margin-top:22px;",
        actionButton("add_another", "+ Add Another", class = "btn btn-outline"),
        uiOutput("export_button")
      )
    )
  )
}
