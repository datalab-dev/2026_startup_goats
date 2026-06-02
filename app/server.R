server <- function(input, output, session) {

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
}

server
