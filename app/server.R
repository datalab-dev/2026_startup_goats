server <- function(input, output, session) {

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
