# Page 4 - Measurement Input / Visualizer (the graph).  
# The graph and the input controls are kept from the previous weeks of work, with some slight differences
page4_ui <- function() {
  div(class = "page-enter",
    div(class = "card-panel",
      fluidRow(

        column(width = 4,
          h2(class = "page-title", style = "font-size:22px;", "Enter measurements"),
          p(class = "page-sub", style = "margin-bottom:14px;",
            "Enter the 6 linear appraisal scores, hit ", tags$b("Create Visual"),
            ", then fine-tune hock, legs, and udder-arch attachment."),

          actionButton("reset_all", HTML("&#x21bb; Reset all inputs"),
                       class = "btn master-reset"),

          section_card("Linear Appraisal Scores", "scores",
            param_row("udder_depth_score",       "udder depth"),
            param_row("rear_udder_height_score", "rear udder height"),
            param_row("medial_score",            "medial ligament"),
            param_row("teat_length_score",       "teat length"),
            param_row("teat_diameter_score",     "teat diameter"),
            param_row("teat_placement_score",    "teat placement")
          ),

          # secondary action (outlined) - recompute the model from the scores
          actionButton("create_visual", "Create Visual",
                       class = "btn btn-outline calc-btn"),

          br(), br(), # used for spacing

          section_card("Adjustable Parts", "adjustable",
            param_row("hock_height",    "hock height"),
            param_row("leg_width",      "leg width"),
            param_row("arch_leg_y",     "arch leg y"),
            param_row("arch_shape_pad", "arch roundness")
          )
        ),

        # graph and potential image overlay 
        column(width = 8,
          div(class = "agheader",
            div(class = "blurb",
                "Live udder model. Overlay a photo and translate, scale, or rotate it."),
            downloadButton("export_png", HTML("&#x2B07; Save image"),
                           class = "btn btn-default", style = "min-height:38px;",
                           icon = NULL)
          ),

          plotOutput("goat_plot", height = "560px"),
          div(class = "reference-note",
              "Reference model - the blue dot marks the hock origin (0,0)."),

          div(class = "image-card",
            div(class = "section-title", style = "color:var(--muted);", "Image Overlay"),
            fluidRow(
              column(6,
                fileInput("goat_image", "Upload or take a photo of the udder",
                          accept = "image/*", width = "100%")),
              column(6,
                sliderInput("img_opacity", "image visibility",
                            min = 0, max = 100, value = 70, step = 5, post = "%"))
            ),
            fluidRow(
              column(3, sliderInput("zoom",     "zoom",
                                    min = -3, max = 5, value = 1.5, step = 0.1)),
              column(3, sliderInput("shift_x",  "shift x",
                                    min = -10, max = 10, value = -0.5, step = 0.1)),
              column(3, sliderInput("shift_y",  "shift y",
                                    min = -15, max = 15, value = -0.5, step = 0.1)),
              column(3, sliderInput("rotation", "rotation (deg)",
                                    min = -180, max = 180, value = 0, step = 1))
            ),
            div(class = "scale", textOutput("scale_indicator", inline = TRUE))
          ),

          # primary action - record this goat's scores into the review table
          div(class = "btn-row end", style = "margin-top:18px;",
              uiOutput("add_obs_button"))
        )
      )
    )
  )
}
