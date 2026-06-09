# Page 3 - Select a Goat  (HCI #1, #6, #7)
page3_ui <- function() {
  div(class = "page-enter",
    div(class = "card-panel",
      h2(class = "page-title", "Select a goat"),
      p(class = "page-sub", "Search your database, then tap a goat to select it."),

      fluidRow(
        # left: search + results
        column(width = 7,
          div(class = "search-controls",
            uiOutput("column_filter"),
            uiOutput("value"),
            actionButton("search_goats", "🔍  Search",
                         class = "btn btn-primary",
                         style = "width:100%; margin-bottom:14px;")
          ),
          uiOutput("goat_results")
        ),
        # right: progressive-disclosure selected-goat panel
        column(width = 5,
          uiOutput("selected_goat_panel")
        )
      )
    )
  )
}
