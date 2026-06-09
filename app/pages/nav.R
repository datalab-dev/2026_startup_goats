# shared navbar and 6 pages pagination logic

# Pages are as followed:
# 1. Welcome  
# 2. Upload  
# 3. Select  
# 4. Visualizer 
# 5. Review  
# 6. Export

N_PAGES <- 6

# creates a list of page values (p1, p2, ..., p[N]) and labels for the nav bar
PAGE_VALUES <- paste0("p", seq_len(N_PAGES))
PAGE_LABELS <- c("Welcome", "Upload", "Select goat",
                 "Measure", "Review", "Export")

# function for the nav bar
# has the logo and the SOP reference link

nav_bar <- function() {
  div(class = "nav-barr",
    div(class = "brand",
        tags$img(src = "TEXTGOATvznLogo.png", alt = "GOATvzn",
                 class = "brand-logo")
    ),
    tags$a(class = "sop-link", href = "https://adga.org/wp-content/uploads/2025/03/2025Linear-SOPDraft1.pdf",
           target = "_blank", rel = "noopener noreferrer",
           title = "Standard Operating Procedure reference",
           "SOP Reference")
  )
}

# progress dot  bars based on states
dots_ui <- function(current, total = N_PAGES) {
  div(class = "dots",
    lapply(seq_len(total), function(i) {
      state <- if (i == current) "active" else if (i < current) "done" else "todo"
      span(class = paste("dot", state),
           title = paste0("Step ", i, ": ", PAGE_LABELS[i]))
    })
  )
}


# navigation logic: page state management, next/previous button rendering, and
# cross-page contextual actions
logic_nav <- function(input, output, session, rv, S) {

  nav_to <- function(n) {
    n <- max(1, min(N_PAGES, n))
    rv$page <- n
    updateTabsetPanel(session, "page_tabs", selected = PAGE_VALUES[n])
  }
  S$nav_to <- nav_to

  output$page_dots <- renderUI({ dots_ui(rv$page) })


  # previous button, changing based on state
  output$prev_button <- renderUI({
    if (rv$page == 1) {
      tags$button("← Back", class = "btn btn-default", disabled = NA)
    } else {
      actionButton("nav_prev", "← Back", class = "btn btn-default")
    }
  })

  # whether the current linear page (1-3) is allowed to advance. Used to both
  # render the Next button's enabled state AND guard the observer, so progress
  # can't slip through via keyboard/programmatic events.
  can_advance <- function() {
    switch(rv$page,
      `1` = {
        nm <- input$user_name  %||% ""
        em <- input$user_email %||% ""
        nzchar(trimws(nm)) && grepl(".+@.+\\..+", em)
      },
      `2` = !is.null(rv$goat_data) && length(rv$missing_cols) == 0,
      `3` = !is.null(rv$selected_goat),
      FALSE
    )
  }

  # next button but only on the first 3 pages, and disabled if can_advance() is FALSE
  output$next_button <- renderUI({
    if (rv$page >= 4) return(NULL)
    if (can_advance()) actionButton("nav_next", "Next →", class = "btn btn-primary")
    else               tags$button("Next →", class = "btn btn-primary", disabled = NA)
  })

  observeEvent(input$nav_next, { if (can_advance()) nav_to(rv$page + 1) })
  observeEvent(input$nav_prev, { nav_to(rv$page - 1) })

  # map where each action button can take the user, based on the current page and action
  observeEvent(input$add_observation, {nav_to(5)})
  observeEvent(input$add_another,     {nav_to(3)})
  observeEvent(input$did_export,      {nav_to(6)})
  observeEvent(input$edit_again,      {nav_to(5)})
  observeEvent(input$retry_export,    {nav_to(5)})
  observeEvent(input$start_over, {
    rv$selected_goat <- NULL
    rv$observations  <- NULL
    rv$export_status <- "success"
    rv$export_time   <- NULL
    rv$export_count  <- NULL
    nav_to(1)
  })
}
