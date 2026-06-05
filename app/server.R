server <- function(input, output, session) {
  # HILARY"S CODE
  # page state to keep track of the current page
  page <- reactiveVal("user_information")
  
  # enabling/disabling button logic
  # previous button
  output$prev_button <- renderUI({
    # disabled on the first page
    if (page() == "user_information") {
      tags$button("← Previous", class = "btn btn-default", disabled = NA)
    } else {
      actionButton("nav_prev", "← Previous")
    }
  })
  
  # next button
  output$next_button <- renderUI({
    ok <- switch(page(),
                 # only allowed when required inputs are entered
                 "user_information" = {
                   name  <- input$user_name  %||% ""
                   email <- input$user_email %||% ""
                   nzchar(trimws(name)) &&
                     grepl(".+@.+\\..+", email) # regex to check proper email format
                 },
                 "goat_database"  = !is.null(input$goat_database),
                 "goat_selection" = !is.null(selected_goat()),
                 "goat_visualizer" = FALSE   # last page
    )
    
    if (ok) {
      actionButton("nav_next", "Next →")
    } else {
      tags$button("Next →", class = "btn btn-default", disabled = NA)
    }
  })
  
  # next button logic
  observeEvent(input$nav_next, {
    # name and email must be entered
    if (page() == "user_information") {
      req(input$user_name, input$user_email)
      
      validate(
        need(
          grepl(".+@.+\\..+", input$user_email), 
          "Please enter a valid email address"
        )
      )
      
      updateTabsetPanel(session, "page_tabs", selected = "goat_database")
      page("goat_database")
    }
    # goat database must be uploaded
    else if(page() == "goat_database") {
      req(input$goat_database)
      
      updateTabsetPanel(session, "page_tabs", selected = "goat_selection")
      page("goat_selection")
    }
    # goat must be selected
    else if (page() == "goat_selection") {
      req(input$goat_card_click)
      
      updateTabsetPanel(session, "page_tabs", selected = "goat_visualizer")
      page("goat_visualizer")
    }
  })
    
  # previous button logic
  observeEvent(input$nav_prev, {
    if (page() == "goat_database") {
      updateTabsetPanel(session, "page_tabs", selected = "user_information")
      page("user_information")
    }
    else if (page() == "goat_selection") {
      updateTabsetPanel(session, "page_tabs", selected = "goat_database")
      page("goat_database")
    }
    
    else if (page() == "goat_visualizer") {
      updateTabsetPanel(session, "page_tabs", selected = "goat_selection")
      page("goat_selection")
    }
  })
  
  # store the goat database
  goat_data <- reactiveVal(NULL)
  
  # store selected goat
  selected_goat <- reactiveVal(NULL)
  
  # read in the goat database when uploaded
  observeEvent(input$goat_database, {
    req(input$goat_database)
    
    tryCatch({
      df <- read.csv(
        input$goat_database$datapath,
        stringsAsFactors = FALSE,
        check.names      = FALSE  
      )
      goat_data(df)
    }, error = function(e) {
      showNotification(
        paste("Failed to read CSV:", e$message),
        type = "error", duration = 5
      )
      goat_data(NULL)
    })
  })
  
  # filter by column
  output$column_filter <- renderUI({
    goats <- goat_data()
    
    # no data, placeholder message
    if (is.null(goats)) {
      return(p(style = "color:#888; font-style:italic;",
               "Upload a goat database to search."))
    }
    
    # select what to filter by
    col_choices <- c("None" = "", names(goats))
    
    selectInput(
      "filter_column",
      label    = "Filter by",
      choices  = col_choices,
      selected = "",
      width    = "100%"
    )
  })
  
  # show the input text"box only when a filter is selected
  output$value <- renderUI({
    if (is.null(input$filter_column) || input$filter_column == "") {
      return(NULL)
    }
    
    textInput(
      "filter_value",
      label       = paste("Search in:", input$filter_column),
      placeholder = paste0("Type to filter by ", input$filter_column,
                           " (leave blank for all)"),
      width    = "100%"
    )
  })
  
  # run search only when button is clicked
  filtered_goats <- eventReactive(input$search_goats, {
    goats <- goat_data()
    
    if (is.null(goats)) return(NULL)
    
    col   <- isolate(input$filter_column %||% "")
    value <- trimws(isolate(input$filter_value) %||% "")
    
    # return all if searching for all
    if (!nzchar(col) || !nzchar(value)) {
      return(goats)
    }
    
    # column must exist
    if (!col %in% names(goats)) return(goats)
    
    # case-insensitive substring match on the chosen column
    matches <- grepl(
      pattern = tolower(value),
      x       = tolower(as.character(goats[[col]])),
      fixed   = TRUE
    )
    
    goats[matches, , drop = FALSE]
    
  }, ignoreNULL = FALSE)
  
  # saves the goat search page result number
  results_page <- reactiveVal(1)
  
  # reset the page number to 1 when search is ran
  observeEvent(input$search_goats, {
    results_page(1)
    selected_goat(NULL)
  })
  
  # go to the previous search result page
  observeEvent(input$results_prev_page, {
    results_page(max(1, results_page() - 1))
  })
  
  # go to the next search result page
  observeEvent(input$results_next_page, {
    goats <- filtered_goats()
    if (is.null(goats)) return()
    max_page <- ceiling(nrow(goats) / 10)
    results_page(min(max_page, results_page() + 1))
  })
  
  # display goat results after search
  output$goat_results <- renderUI({
    goats <- filtered_goats()
    df    <- goat_data()
    
    if (is.null(df)) {
      return(div(
        style = "padding:20px; background:#f8d7da; border:1px solid #dc3545; border-radius:5px;",
        p(style = "margin:0; color:#721c24;", "Error: no goat database loaded.")
      ))
    }
    
    if (is.null(goats) || nrow(goats) == 0) {
      return(div(
        style = "padding:20px; background:#fff3cd; border:1px solid #ffc107; border-radius:5px;",
        p(style = "margin:0; color:#856404;", "No goats found matching your search criteria.")
      ))
    }
    
    # pagination match
    items_per_page <- 10
    total_items    <- nrow(goats)
    total_pages    <- ceiling(total_items / items_per_page)
    cur_page       <- min(results_page(), total_pages)   
    
    start_i <- (cur_page - 1) * items_per_page + 1
    end_i   <- min(cur_page * items_per_page, total_items)
    
    page_goats <- goats[start_i:end_i, , drop = FALSE]
    
    # build the goat cards
    goat_cards <- lapply(seq_len(nrow(page_goats)), function(i) {
      goat_id    <- as.character(page_goats$`Animal Id`[i])
      goat_name  <- page_goats$`Animal Name`[i]
      owner_name <- page_goats$`Owner Name`[i]
      is_selected <- identical(selected_goat(), goat_id)
      
      card_style <- if (is_selected) {
        "background:#d9d9d9; border:2px solid #888; border-radius:8px;
       padding:12px; margin-bottom:10px; cursor:pointer; opacity:0.7;"
      } else {
        "background:white; border:2px solid #559FD9; border-radius:8px;
       padding:12px; margin-bottom:10px; cursor:pointer;"
      }
      
      div(
        style      = card_style,
        onmouseover = if (!is_selected) "this.style.background='#eef5fc';" else NULL,
        onmouseout  = if (!is_selected) "this.style.background='white';"   else NULL,
        onclick     = sprintf(
          "Shiny.setInputValue('goat_card_click','%s',{priority:'event'});",
          goat_id
        ),
        div(style = "font-weight:700; font-size:16px; color:#559FD9; margin-bottom:4px;",
            goat_name),
        div(style = "font-size:14px; color:#666;",
            sprintf("ID: %s", goat_id)),
        div(style = "font-size:13px; color:#888; margin-top:4px;",
            sprintf("Owner: %s", owner_name))
      )
    })
    
    # pagination controls
    prev_btn <- if (cur_page > 1) {
      actionButton("results_prev_page", "← Prev",
                   style = "margin-right:8px;")
    } else {
      tags$button("← Prev", class = "btn btn-default",
                  disabled = NA, style = "margin-right:8px;")
    }
    
    next_btn <- if (cur_page < total_pages) {
      actionButton("results_next_page", "Next →")
    } else {
      tags$button("Next →", class = "btn btn-default", disabled = NA)
    }
    
    # pagination control
    pagination <- div(
      style = "display:flex; align-items:center; justify-content:center; gap:8px; margin:12px 0;",
      prev_btn,
      span(style = "font-size:13px; color:#555;",
           sprintf("Page %d of %d", cur_page, total_pages)),
      next_btn
    )
    
    # display the controls at the top and bottom of the search results
    div(
      div(style = "margin-bottom:10px; color:#666; font-size:14px;",
          sprintf("Showing %d–%d of %d goat%s",
                  start_i, end_i, total_items,
                  if (total_items == 1) "" else "s")),
      
      pagination,              
      do.call(tagList, goat_cards),
      pagination              
    )
    
  })
  
  # handle goat card clicks
  observeEvent(input$goat_card_click, {
    clicked_goat_id <- input$goat_card_click
    
    # look up the goat name
    database <- goat_data()
    goat <- if (!is.null(database)) {
      match_row <- database[as.character(database$`Animal Id`) == clicked_goat_id, ]
      if (nrow(match_row) > 0) match_row$`Animal Name`[1] else clicked_goat_id
    } else {
      clicked_goat_id
    }
    
    # toggle selection
    if (identical(selected_goat(), clicked_goat_id)) {
      selected_goat(NULL)
      
      showNotification(
        sprintf("Deselected: %s", goat),
        type = "warning",
        duration = 2
      )
      
    } else {
      
      selected_goat(clicked_goat_id)
      
      showNotification(
        sprintf("Selected: %s", goat),
        type = "message",
        duration = 2
      )
    }
  })
  
  # clear the selected goat when a new search occurs
  observeEvent(input$search_goats, {
    selected_goat(NULL)
  })


  # RASH'S CODE

  # +/- / reset / min / max badge wiring for every input in PARAM_DEFAULTS.
  # The +/- buttons always nudge by 0.5 per the Ag-GOAT notes; numericInput's
  # own `step` is the keyboard-arrow granularity. local() captures pid so the
  # closures don't all see the last value of the loop variable.
  for (param_id in names(PARAM_DEFAULTS)) {
    local({
      pid         <- param_id
      default_val <- PARAM_DEFAULTS[[pid]]
      bounds      <- PARAM_BOUNDS[[pid]]

      bump <- function(delta) {
        new_val <- clamp_numeric(input[[pid]] + delta, bounds$min, bounds$max)
        if (is.null(new_val)) return(invisible())
        updateNumericInput(session, pid, value = new_val)
      }

      observeEvent(input[[paste0(pid, "_plus")]],  { bump(1) })
      observeEvent(input[[paste0(pid, "_minus")]], { bump(-1) })
      observeEvent(input[[paste0(pid, "_reset")]], {
        updateNumericInput(session, pid, value = default_val)
      })
      observeEvent(input[[paste0(pid, "_min_badge")]], {
        updateNumericInput(session, pid, value = bounds$min)
      })
      observeEvent(input[[paste0(pid, "_max_badge")]], {
        updateNumericInput(session, pid, value = bounds$max)
      })

      # clamp out-of-range typed values back into the allowed window and cap
      # them at 2-dp. Only write back when the cleaned value actually differs,
      # so we don't reset the cursor on every valid keystroke.
      observeEvent(input[[pid]], {
        cleaned <- clamp_numeric(input[[pid]], bounds$min, bounds$max)
        if (is.null(cleaned)) return()
        if (!isTRUE(all.equal(cleaned, input[[pid]]))) {
          updateNumericInput(session, pid, value = cleaned)
        }
      }, ignoreInit = TRUE)
    })
  }

  # validation is split so the plot only invalidates on adjustable-param
  # changes or a new lock-in. Reading score inputs here would re-trigger the
  # renderPlot (and the white-out animation) on every score keystroke even
  # though the scores aren't applied until Create Visual is clicked.
  # is_valid_number() lives in R/utils.R.
  adjustable_params_filled <- reactive({
    all(vapply(names(ADJUSTABLE_DEFAULTS),
               function(pid) is_valid_number(input[[pid]]),
               logical(1)))
  })

  locked_scores_filled <- reactive({
    all(vapply(locked_scores(), is_valid_number, logical(1)))
  })

  observeEvent(input$reset_all, {
    for (pid in names(PARAM_DEFAULTS)) {
      updateNumericInput(session, pid, value = PARAM_DEFAULTS[[pid]])
    }
  })

  # lock the score inputs in when "Create Visual" is clicked. ignoreNULL = FALSE
  # together with default ignoreInit = FALSE fires this once at startup using
  # the param_row defaults, so the plot renders without requiring a click.
  locked_scores <- eventReactive(input$create_visual, {
    list(
      udder_depth_score       = input$udder_depth_score,
      rear_udder_height_score = input$rear_udder_height_score,
      medial_score            = input$medial_score,
      teat_length_score       = input$teat_length_score,
      teat_diameter_score     = input$teat_diameter_score,
      teat_placement_score    = input$teat_placement_score
    )
  }, ignoreNULL = FALSE)

  # geometry = locked scores + live adjustable params. The validate() here
  # catches the only score-independent failure mode: arch <-> leg intersection
  # placed at or above the rear-udder-height-determined arch vertex, which
  # would flip the inverse-sqrt arch.
  geometry <- reactive({
    s <- locked_scores()
    arch_vertex_y <- -input$hock_height *
                     (1 - score_to_rear_udder_height_pct(s$rear_udder_height_score))
    validate(need(
      input$arch_leg_y < arch_vertex_y,
      sprintf("Arch leg y (%.1f) must sit below the udder arch vertex (%.2f). Lower the slider, raise the hock, or bump the rear udder height score.",
              input$arch_leg_y, arch_vertex_y)
    ))
    scores_to_geometry(
      scores         = s,
      hock_height    = input$hock_height,
      leg_width      = input$leg_width,
      arch_leg_y     = input$arch_leg_y,
      arch_shape_pad = input$arch_shape_pad
    )
  })


  # NEW FUCNTION, takes geometrical inch measurement from the linear scores
  teats_poly <- reactive({
    g <- geometry()
    teats_polygon_from_measurements(
      teat_x_center       = g$teat_x_center,
      teat_diameter_in    = g$teat_diameter_in,
      teat_length_in      = g$teat_length_in,
      udder_floor_height  = g$udder_floor_height,
      closeness_of_halves = g$closeness_of_halves,
      depth_of_medial     = g$depth_of_medial
    )
  })

  legs_poly <- reactive({
    legs_polygon_df(leg_width = input$leg_width,
                    hock_height = input$hock_height,
                    top_y = view_top, bot_y = view_bottom)
  })

  hocks_poly <- reactive({
    hocks(hock_height = input$hock_height, leg_width = input$leg_width)
  })

  hock_midline <- reactive({
    hockmidline(hock_height = input$hock_height, leg_width = input$leg_width)
  })

  pelvic_poly <- reactive({
    pelvic_polygon_df(leg_width = input$leg_width, top_y = view_top)
  })

  body_poly <- reactive({
    g <- geometry()
    body_polygon_df(
      udder_floor_height  = g$udder_floor_height,
      closeness_of_halves = g$closeness_of_halves,
      depth_of_medial     = g$depth_of_medial,
      arch_roundness      = g$arch_roundness,
      arch_height         = g$arch_height,
      arch_shape          = g$arch_shape,
      leg_width           = g$leg_width
    )
  })

  # Image overlay handler (unchanged)
  goat_raster <- reactive({
    req(input$goat_image)
    img <- magick::image_read(input$goat_image$datapath)
    img <- magick::image_colorize(img,
              opacity = 100 - input$img_opacity, color = "white")
    img <- magick::image_rotate(img, input$rotation)
    as.raster(img)
  })

  output$goat_plot <- renderPlot({
    validate(need(adjustable_params_filled() && locked_scores_filled(),
                  "Please fill out every input with a number."))

    g <- ggplot() +
      coord_fixed(xlim = c(-8, 8),
                  ylim = c(view_bottom, view_top), expand = FALSE) +
      theme_minimal() +
      labs(title = NULL,
           x = "Horizontal position",
           y = "Vertical position")

    if (!is.null(input$goat_image)) {
      raster <- goat_raster()
      g <- g + annotation_raster(
        raster,
        xmin = (-8 - input$zoom)          + input$shift_x,
        xmax = ( 8 + input$zoom)          + input$shift_x,
        ymin = (view_bottom - input$zoom) + input$shift_y,
        ymax = (view_top    + input$zoom) + input$shift_y
      )
    }

    g +
      geom_polygon(data = teats_poly(),  aes(x, y, group = group),
                   fill = "mediumpurple", color = "mediumpurple3",
                   linewidth = 1, alpha = 0.31) +
      geom_polygon(data = pelvic_poly(), aes(x, y, group = group),
                   fill = "steelblue", color = "steelblue",
                   linewidth = 1, alpha = 0.45) +
      geom_polygon(data = body_poly(),   aes(x, y, group = group),
                   fill = "salmon", color = "firebrick",
                   linewidth = 1, alpha = 0.5) +
      geom_polygon(data = legs_poly(),   aes(x, y, group = group),
                   fill = "gray60", color = "black",
                   linewidth = 0.4, alpha = 0.5) +
      geom_segment(data = hock_midline(),
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", linewidth = 0.6) +
      geom_polygon(data = hocks_poly(), aes(x, y, group = side),
                   fill = "gray40", color = "black",
                   linewidth = 0.5) +
      geom_point(aes(x = 0, y = 0), color = "steelblue", size = 4)
  })

  output$scale_indicator <- renderText({
    sprintf("scale: image visibility %d%%, zoom %s",
            input$img_opacity, format(input$zoom, nsmall = 1))
  })

 # notification system for now, which will eventually be deleted/commented out 
  observeEvent(input$calc_score, {
    showNotification(
      "Linear appraisal score calculation will be wired up to the trained model.",
      type = "message", duration = 4)
  })

  # expporting score data in the UCD Goat Lab Approved Format.
  # CSV export uses the score inputs the user is currently looking at
  # (whether or not they've been locked in via Create Visual).
  # "Rear Udder Arch" stays in the schema (UCD Goat Lab format) but is
  # left blank since it isn't one of the 6 scores driving the visual.
  output$export_data <- downloadHandler(
    filename = function() {
      sprintf("goat_traits-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    content = function(file) {
      export_df <- data.frame(
        "UdderDepth"                 = input$udder_depth_score,
        "Rear Udder Height"          = input$rear_udder_height_score,
        "Rear Udder Arch"            = NA,
        "Medial Suspensory Ligament" = input$medial_score,
        "Teat Placement"             = input$teat_placement_score,
        "Teat Diameter"              = input$teat_diameter_score,
        "Teat Length"                = input$teat_length_score,
        check.names = FALSE
      )
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  # exporting function for png 
  output$export_png <- downloadHandler(
    filename = function() {
      sprintf("goat-plot-%s.png", format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    content = function(file) {
      png(file, width = 1200, height = 900, res = 150)
      
      g <- ggplot() +
        coord_fixed(xlim = c(-8, 8),
                    ylim = c(view_bottom, view_top), expand = FALSE) +
        theme_minimal() +
        labs(x = "Horizontal position", y = "Vertical position")
      
      if (!is.null(input$goat_image)) {
        raster <- goat_raster()
        g <- g + annotation_raster(
          raster,
          xmin = (-8 - input$zoom)         + input$shift_x,
          xmax = ( 8 + input$zoom)         + input$shift_x,
          ymin = (view_bottom - input$zoom) + input$shift_y,
          ymax = (view_top    + input$zoom) + input$shift_y
        )
      }
      
      print(g +
              geom_polygon(data = teats_poly(),  aes(x, y, group = group),
                           fill = "mediumpurple", color = "mediumpurple3",
                           linewidth = 1, alpha = 0.31) +
              geom_polygon(data = pelvic_poly(), aes(x, y, group = group),
                           fill = "steelblue", color = "steelblue",
                           linewidth = 1, alpha = 0.45) +
              geom_polygon(data = body_poly(),   aes(x, y, group = group),
                           fill = "salmon", color = "firebrick",
                           linewidth = 1, alpha = 0.5) +
              geom_polygon(data = legs_poly(),   aes(x, y, group = group),
                           fill = "gray60", color = "black",
                           linewidth = 0.4, alpha = 0.5) +
              geom_segment(data = hock_midline(),
                           aes(x = x, y = y, xend = xend, yend = yend),
                           color = "black", linewidth = 0.6) +
              geom_polygon(data = hocks_poly(), aes(x, y, group = side),
                           fill = "gray40", color = "black",
                           linewidth = 0.5) +
              geom_point(aes(x = 0, y = 0), color = "steelblue", size = 4))
      
      dev.off()
    }
  )
}

server
