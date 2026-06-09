# Page 3 logic - search and select a goat
logic3 <- function(input, output, session, rv, S) {

  # resolve a required identity column to its actual header via the page-2 mapping
  gcol <- function(canon) goat_col(rv$goat_card_cols, canon)

  # filtering based controls
  output$column_filter <- renderUI({
    goats <- rv$goat_data
    if (is.null(goats)) {
      return(p(class = "reference-note", "Upload a goat database to search."))
    }
    col_choices <- c("None" = "", names(goats))
    selectInput("filter_column", label = "Filter by",
                choices = col_choices, selected = "", width = "100%")
  })

  # show the value box only when a filter column is chosen
  output$value <- renderUI({
    if (is.null(input$filter_column) || input$filter_column == "") return(NULL)
    textInput("filter_value",
              label = paste("Search in:", input$filter_column),
              placeholder = "Search by name or tag number",
              width = "100%")
  })

  # do the actual filtering when the search button is clicked, and store results in reactive state
  S$filtered_goats <- eventReactive(input$search_goats, {
    goats <- rv$goat_data # calls upon the goats data from page 2
    if (is.null(goats)) return(NULL)

    col   <- isolate(input$filter_column %||% "")
    value <- trimws(isolate(input$filter_value) %||% "")

    if (!nzchar(col) || !nzchar(value)) return(goats) # return full list if no column or value specified
    if (!col %in% names(goats)) return(goats)

    matches <- grepl(tolower(value),
                     tolower(as.character(goats[[col]])), fixed = TRUE)
    goats[matches, , drop = FALSE]
  }, ignoreNULL = FALSE)

  # default to page 1 of results, and reset to page 1 on new search or when selected goat changes
  S$results_page <- reactiveVal(1)

  observeEvent(input$search_goats, {
    S$results_page(1)
    rv$selected_goat <- NULL
  })
  observeEvent(input$results_prev_page, {
    S$results_page(max(1, S$results_page() - 1))
  })
  observeEvent(input$results_next_page, {
    goats <- S$filtered_goats()
    if (is.null(goats)) return()
    max_page <- ceiling(nrow(goats) / 10)
    S$results_page(min(max_page, S$results_page() + 1))
  })

  # input the results
  output$goat_results <- renderUI({
    goats <- S$filtered_goats()
    df    <- rv$goat_data

    if (is.null(df)) {
      return(div(class = "empty-state",
                 span(class = "icon", "\U0001F4C4"),
                 "No goat database loaded. Go back and upload one."))
    }
    if (is.null(goats) || nrow(goats) == 0) {
      return(div(class = "empty-state",
                 span(class = "icon", "\U0001F50D"),
                 "No goats found. Try a different name or ID."))
    }

    items_per_page <- 10
    total_items    <- nrow(goats)
    total_pages    <- ceiling(total_items / items_per_page)
    cur_page       <- min(S$results_page(), total_pages)

    start_i <- (cur_page - 1) * items_per_page + 1
    end_i   <- min(cur_page * items_per_page, total_items)
    page_goats <- goats[start_i:end_i, , drop = FALSE]

    # in line HTML based on the page of the goats we're showing
    goat_cards <- lapply(seq_len(nrow(page_goats)), function(i) {
      goat_id    <- as.character(page_goats[[ gcol("Animal Id") ]][i])
      goat_name  <- page_goats[[ gcol("Animal Name") ]][i]
      owner_name <- page_goats[[ gcol("Owner Name") ]][i]
      is_sel     <- identical(rv$selected_goat, goat_id)

      div(
        class = paste("goat-card", if (is_sel) "selected" else ""),
        onclick = sprintf(
          "Shiny.setInputValue('goat_card_click','%s',{priority:'event'});", goat_id),
        div(class = "g-name", goat_name),
        div(class = "g-meta", sprintf("ID: %s", goat_id)),
        div(class = "g-meta", sprintf("Owner: %s", owner_name))
      )
    })

    prev_btn <- if (cur_page > 1) {
      actionButton("results_prev_page", "← Prev",
                   class = "btn btn-default", style = "min-height:38px;")
    } else {
      tags$button("← Prev", class = "btn btn-default", disabled = NA,
                  style = "min-height:38px;")
    }
    next_btn <- if (cur_page < total_pages) {
      actionButton("results_next_page", "Next →",
                   class = "btn btn-default", style = "min-height:38px;")
    } else {
      tags$button("Next →", class = "btn btn-default", disabled = NA,
                  style = "min-height:38px;")
    }
    pagination <- div(
      style = "display:flex; align-items:center; justify-content:center; gap:10px; margin:12px 0;",
      prev_btn,
      span(class = "summary-line", style = "margin:0;",
           sprintf("Page %d of %d", cur_page, total_pages)),
      next_btn
    )

    div(
      div(class = "summary-line",
          sprintf("Showing %d–%d of %d goat%s",
                  start_i, end_i, total_items,
                  if (total_items == 1) "" else "s")),
      do.call(tagList, goat_cards),
      if (total_pages > 1) pagination
    )
  })

  # handle clicks to the goat cards by storing the selected goat's ID in reactive state
  observeEvent(input$goat_card_click, {
    clicked <- input$goat_card_click
    if (identical(rv$selected_goat, clicked)) {
      rv$selected_goat <- NULL
    } else {
      rv$selected_goat <- clicked
    }
  })

  # progress to next page only if a goat is selected, otherwise show an alert
  output$selected_goat_panel <- renderUI({
    sel <- rv$selected_goat
    if (is.null(sel)) {
      return(div(class = "selected-panel",
        div(class = "section-title", style = "color:var(--muted);", "Your selected goat"),
        div(class = "empty-state", style = "border:none; padding:18px 6px;",
            span(class = "icon", "\U0001F410"),
            "No goat selected yet. Tap a result to choose one.")))
    }
    db  <- rv$goat_data
    row <- db[as.character(db[[ gcol("Animal Id") ]]) == sel, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    r <- row[1, ]

    # get the info fields to show in the selected goat panel, hiding any that are empty or NA
    info <- function(lbl, val) {
      if (is.null(val) || is.na(val) || !nzchar(as.character(val))) return(NULL)
      div(style = "margin-bottom:8px;",
          div(class = "g-meta", style = "font-size:11px; text-transform:uppercase;", lbl),
          div(style = "font-weight:600;", as.character(val)))
    }

    div(class = "selected-panel",
      div(class = "section-title", style = "color:var(--teal);", "Your selected goat"),
      div(class = "g-name", style = "font-size:18px; margin-bottom:10px;",
          r[[ gcol("Animal Name") ]]),
      info("Animal ID", r[[ gcol("Animal Id") ]]),
      info("Owner",     r[[ gcol("Owner Name") ]]),
      info("Breed",     r$Breed),
      info("Size",      r$Size)
    )
  })
}
