# Page 2 - Upload goat database (extra step, not in the Figma)

page2_ui <- function() {
  div(class = "page-enter",
    div(class = "card-panel",
      span(class = "blob blob-tr"),
      h2(class = "page-title", "Upload goat database"),
      p(class = "page-sub",
        "Choose the appraisal CSV you'll be working from. Only .csv files are accepted."),

      fileInput("goat_database", labelMandatory("Goat database (.csv)"),
                accept = ".csv", width = "100%",
                placeholder = "No file selected"),

      uiOutput("upload_status"),

      # shown only when the CSV is missing required identity columns
      uiOutput("column_selection")
    )
  )
}
