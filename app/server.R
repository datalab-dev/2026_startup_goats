server <- function(input, output, session) {
  # page states to keep track of the pages
  page <- reactiveVal("user_information")
  
  # next button logic
  observeEvent(input$nav_next, {
    if (page() == "user_information") {
      req(input$user_name, input$user_email)
      
      validate(
        need(
          grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
                input$user_email),
          "Please enter a valid email address"
        )
      )
      
      updateTabsetPanel(session, "page_tabs", selected = "goat_selection")
      page("goat_selection")
    }
    else if (page() == "goat_selection") {
      # Now check for goat_card_click instead of goat_select
      req(input$goat_card_click)
      
      updateTabsetPanel(session, "page_tabs", selected = "goat_visualizer")
      page("goat_visualizer")
    }
  })
    
  # previous button logic
  observeEvent(input$nav_prev, {
    if (page() == "goat_selection") {
      updateTabsetPanel(session, "page_tabs", selected = "user_information")
      page("user_information")
    }
    
    else if (page() == "goat_visualizer") {
      updateTabsetPanel(session, "page_tabs", selected = "goat_selection")
      page("goat_selection")
    }
  })
  
  # filter goats shown based off the owner name/id
  filtered_goats <- reactive({
    # only filter after search button is pressed
    req(input$search_goats)
    
    if (!exists("goat_database")) return(NULL)
    goats <- goat_database
    
    # Check for required columns immediately
    if (!all(c("Animal Id", "Animal Name") %in% names(goats))) {
      return(NULL)
    }
    
    name_filter <- trimws(input$owner_name_filter %||% "")
    id_filter   <- trimws(input$owner_id_filter %||% "")
    
    # If both filters are empty, return all goats
    if (!nzchar(name_filter) && !nzchar(id_filter)) {
      return(goats)
    }
    
    # get the first/last name of the owner
    owner_names <- goats$`Owner Name`
    owner_last  <- sub(",.*", "", owner_names)     # Last name (before comma)
    owner_first <- sub(".*,\\s*", "", owner_names) # First name (after comma)
    
    match_name <- rep(TRUE, nrow(goats))
    match_id   <- rep(TRUE, nrow(goats))
    
    if (nzchar(name_filter)) {
      match_name <- grepl(name_filter, owner_first, ignore.case = TRUE) |
        grepl(name_filter, owner_last,  ignore.case = TRUE)
    }
    
    if (nzchar(id_filter)) {
      match_id <- grepl(id_filter, as.character(goats$OwnerID), ignore.case = TRUE)
    }
    
    goats[match_name & match_id, , drop = FALSE]
  })
  
  # display goat results after search
  output$goat_results <- renderUI({
    # only show results after search button is pressed
    req(input$search_goats)
    
    goats <- filtered_goats()
    
    # Check for data loading issues first
    if (!exists("goat_database")) {
      return(
        div(style = "padding: 20px; background: #f8d7da; border: 1px solid #dc3545; border-radius: 5px;",
            p(style = "margin: 0; color: #721c24;", 
              "Error: Goat database not loaded.")
        )
      )
    }
    
    # Check for required columns
    if (!all(c("Animal Id", "Animal Name") %in% names(goat_database))) {
      return(
        div(style = "padding: 20px; background: #f8d7da; border: 1px solid #dc3545; border-radius: 5px;",
            p(style = "margin: 0; color: #721c24;", 
              sprintf("Error: Required columns not found. Available columns: %s", 
                      paste(names(goat_database), collapse = ", ")))
        )
      )
    }
    
    # no goats found
    if (is.null(goats) || nrow(goats) == 0) {
      return(
        div(style = "padding: 20px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;",
            p(style = "margin: 0; color: #856404;", 
              "No goats found matching your search criteria.")
        )
      )
    }
    
    # clickable cards to select a goat
    goat_cards <- lapply(1:nrow(goats), function(i) {
      goat_id <- as.character(goats$`Animal Id`[i])
      goat_name <- goats$`Animal Name`[i]
      owner_name <- goats$`Owner Name`[i]
      
      div(
        style = "background: white; border: 2px solid #559FD9; border-radius: 8px; 
             padding: 12px; margin-bottom: 10px; cursor: pointer;
             transition: all 0.2s;",
        onmouseover = "this.style.background='#eef5fc'; this.style.borderColor='#3d7fb8';",
        onmouseout = "this.style.background='white'; this.style.borderColor='#559FD9';",
        onclick = sprintf("Shiny.setInputValue('goat_card_click', '%s', {priority: 'event'});", 
                          goat_id),
        
        div(style = "font-weight: 700; font-size: 16px; color: #559FD9; margin-bottom: 4px;",
            goat_name),
        div(style = "font-size: 14px; color: #666;",
            sprintf("ID: %s", goat_id)),
        div(style = "font-size: 13px; color: #888; margin-top: 4px;",
            sprintf("Owner: %s", owner_name))
      )
    })
    
    div(
      div(style = "margin-bottom: 10px; color: #666; font-size: 14px;",
          sprintf("Found %d goat%s:", nrow(goats), if(nrow(goats) == 1) "" else "s")),
      do.call(tagList, goat_cards)
    )
  })
  
  # handle goat card clicks
  observeEvent(input$goat_card_click, {
    selected_goat_id <- input$goat_card_click

    showNotification(
      sprintf("Selected goat ID: %s", selected_goat_id),
      type = "message",
      duration = 2
    )
  })
  
  # incremental buttons for each param
  # the +/- buttons always move by ±0.5 per the Ag-GOAT notes, regardless of
  # the numericInput's own step (which is the granularity for keyboard arrows).
  # local() captures each loop variable so the closures don't all see the
  # last value of pid.
  for (param_id in names(PARAM_DEFAULTS)) {
    local({
      pid         <- param_id
      default_val <- PARAM_DEFAULTS[[pid]]
      bounds      <- PARAM_BOUNDS[[pid]]

      bump <- function(delta) {
        cur <- input[[pid]]
        if (is.null(cur) || is.na(cur)) return(invisible())
        new_val <- max(bounds$min, min(bounds$max, cur + delta))
        updateNumericInput(session, pid, value = new_val)
      }

      observeEvent(input[[paste0(pid, "_plus")]],  { bump( 0.5) })
      observeEvent(input[[paste0(pid, "_minus")]], { bump(-0.5) })
      observeEvent(input[[paste0(pid, "_reset")]], {
        updateNumericInput(session, pid, value = default_val)
      })

    
      observeEvent(input[[paste0(pid, "_min_badge")]], {
        updateNumericInput(session, pid, value = bounds$min)
      })
      observeEvent(input[[paste0(pid, "_max_badge")]], {
        updateNumericInput(session, pid, value = bounds$max)
      })

      # if the user types a value outside the range, it snaps to the closest bound
      # `ignoreInit` so the initial default doesn't fire this.
      observeEvent(input[[pid]], {
        v <- input[[pid]]
        if (is.null(v) || is.na(v)) return()
        if (v < bounds$min) {
          updateNumericInput(session, pid, value = bounds$min)
        } else if (v > bounds$max) {
          updateNumericInput(session, pid, value = bounds$max)
        }
      }, ignoreInit = TRUE)
    })
  }

  # check if all goat params are numbers 
  all_params_filled <- reactive({
    all(vapply(names(PARAM_DEFAULTS), function(pid) {
      v <- input[[pid]]
      !is.null(v) && length(v) == 1 && !is.na(v) && is.numeric(v)
    }, logical(1)))
  })

  # reset everything to defaul
  observeEvent(input$reset_all, {
    for (pid in names(PARAM_DEFAULTS)) {
      updateNumericInput(session, pid, value = PARAM_DEFAULTS[[pid]])
    }
  })

  # define react-like polygons
  teats_poly <- reactive({
    teats_polygon_df(
      teat_placement  = input$teat_placement,
      teat_roundness = input$depth_of_medial,
      udder_floor_height  = input$udder_floor_height,
      teat_length = input$teat_length,
      teat_diameter = input$teat_diameter,
      leg_width = input$leg_width,
      closeness_of_halves = input$closeness_of_halves,
      depth_of_medial = input$depth_of_medial
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
    validate(need(input$arch_shape > input$leg_width,
                  "Arch shape must be greater than leg width."))
    body_polygon_df(
      udder_floor_height  = input$udder_floor_height,
      closeness_of_halves = input$closeness_of_halves,
      depth_of_medial = input$depth_of_medial,
      arch_roundness = input$arch_roundness,
      arch_height = input$arch_height,
      arch_shape = input$arch_shape,
      leg_width = input$leg_width
    )
  })

  # image overlay handler
  goat_raster <- reactive({
    req(input$goat_image)
    img <- magick::image_read(input$goat_image$datapath)
    img <- magick::image_colorize(img,
              opacity = 100 - input$img_opacity, color = "white")
    img <- magick::image_rotate(img, input$rotation)
    as.raster(img)
  })

  # plotting the graph 
  output$goat_plot <- renderPlot({
    validate(need(all_params_filled(),
                  "please fill out all values with numbers"))

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
        xmin = (-8 - input$zoom)         + input$shift_x,
        xmax = ( 8 + input$zoom)         + input$shift_x,
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
      # Hock midline (drawn before the knee circle so the circle outline
      # covers the segment edges).
      geom_segment(data = hock_midline(),
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", linewidth = 0.6) +
      # Knee circles on top of the legs polygon.
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
  output$export_data <- downloadHandler(
    filename = function() {
      sprintf("goat_traits-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    # REPLACE LATER !!!!!!!!
    content = function(file) {
      export_df <- data.frame(
        "UdderDepth"                 = 0,
        "Rear Udder Height"          = 0,
        "Rear Udder Arch"            = 0,
        "Medial Suspensory Ligament" = 0,
        "Teat Placement"             = 0,
        "Teat Diameter"              = 0,
        "Teat Length"                = 0
      )
      write.csv(export_df, file, row.names = FALSE)
    }
  )
}

server
