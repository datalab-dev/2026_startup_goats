# Page 2 logic - read the uploaded CSV into shared state (rv$goat_data) and,
# when the required identity columns are missing, let the user map their own
# columns onto them (rv$goat_card_cols).

logic2 <- function(input, output, session, rv, S) {

  # the three identity columns every downstream page relies on
  REQUIRED_COLS <- c("Animal Id", "Animal Name", "Owner Name")

  # observe the file input for changes, and try to read the CSV when it changes
  observeEvent(input$goat_database, {
    req(input$goat_database)
    tryCatch({
      df <- read.csv(
        input$goat_database$datapath,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      # which required columns are absent? if none, use an identity mapping;
      # otherwise wait for the user to map them via column_selection below.
      missing <- setdiff(REQUIRED_COLS, names(df))
      rv$missing_cols   <- missing
      rv$mapping_error  <- NULL
      rv$goat_card_cols <- if (length(missing) == 0)
        setNames(REQUIRED_COLS, REQUIRED_COLS) else NULL

      rv$goat_data    <- df
      rv$upload_error <- NULL
    }, error = function(e) {
      rv$goat_data      <- NULL
      rv$goat_card_cols <- NULL
      rv$missing_cols   <- character(0)
      rv$upload_error   <- e$message
    })
  })

  # upload status error or success message
  output$upload_status <- renderUI({
    if (!is.null(rv$upload_error)) {
      return(div(class = "empty-state",
        style = "border-color:var(--error); color:var(--error); background:var(--error-bg);",
        span(class = "icon", "⚠"),
        "Couldn't read that file. Check it's a valid .csv and try again."))
    }
    df <- rv$goat_data
    if (is.null(df)) {
      return(div(class = "empty-state",
        span(class = "icon", "\U0001F4C4"),
        "Upload a CSV to continue."))
    }

    # confirmation message with number of rows and columns loaded
    div(style = "margin-top:8px; color:var(--teal); font-weight:600; font-size:14px;",
        sprintf("✓ Loaded %d goat%s · %d columns.",
                nrow(df), if (nrow(df) == 1) "" else "s", ncol(df)))
  })

  # column-mapping UI: only shown when the upload is missing required columns.
  # Lets the user pick which of their columns maps to each required field.
  output$column_selection <- renderUI({
    missing <- rv$missing_cols
    goats   <- rv$goat_data
    if (length(missing) == 0 || is.null(goats)) return(NULL)

    col_choices <- names(goats)

    div(class = "empty-state",
      style = "text-align:left; border-color:var(--error); background:var(--error-bg); margin-top:12px;",
      p(style = "font-weight:600; color:var(--error); margin-bottom:10px;",
        "Some expected columns weren't found. Map your columns to continue:"),
      lapply(missing, function(col) {
        selectInput(
          inputId  = paste0("map_", gsub(" ", "_", col)),
          label    = HTML(paste0("Which column corresponds to <b>", col, "</b>?")),
          choices  = c("— select —" = "", col_choices),
          selected = "",
          width    = "100%"
        )
      }),
      if (!is.null(rv$mapping_error))
        p(class = "help-error", style = "display:block;", rv$mapping_error),
      actionButton("confirm_selection", "Confirm columns",
                   class = "btn btn-primary", style = "width:100%; margin-top:8px;")
    )
  })

  # apply the mapping when confirmed (inline validation, no toast popups)
  observeEvent(input$confirm_selection, {
    missing <- rv$missing_cols
    df      <- rv$goat_data
    if (is.null(df) || length(missing) == 0) return()

    # collect the user's selection for each missing field
    selections <- vapply(missing, function(col) {
      input[[paste0("map_", gsub(" ", "_", col))]] %||% ""
    }, character(1))

    # every field must be chosen, and each must map to a distinct column
    if (any(selections == "")) {
      rv$mapping_error <- "Please select a column for every required field."
      return()
    }
    if (anyDuplicated(selections)) {
      rv$mapping_error <- "Each required field must map to a different column."
      return()
    }

    present <- setdiff(REQUIRED_COLS, missing)
    rv$goat_card_cols <- c(
      setNames(present,    present),     # columns that already existed
      setNames(selections, missing)      # user-mapped columns
    )
    rv$missing_cols  <- character(0)
    rv$mapping_error <- NULL
  })
}
