# document colors:
  # glossy light green - #A6ED9F
  # azul mystic - #559FD9
  # trendy turquoise - #61D3B8

ui <- fluidPage(

  tags$head(
    # Inject iPad / PWA meta tags into the OUTER shinylive document so
    # "Add to Home Screen" treats this as a standalone app. Doing this from
    # JS (rather than editing site/index.html post-export) keeps the build
    # step a single command: shinylive::export("app", "site").
    tags$script(HTML("
      (function () {
        let doc;
        // adding a meta section in conjunction to the document
        try { doc = window.parent.document; } catch (e) { doc = document; }
        if (!doc) doc = document;
        function meta(n, c) {
          if (doc.querySelector('meta[name=\"' + n + '\"]')) return;
          const m = doc.createElement('meta');
          m.name = n; m.content = c;
          doc.head.appendChild(m);
        }
        // adding meta headers to support apple device usage
        meta('apple-mobile-web-app-capable',         'yes');
        meta('mobile-web-app-capable',               'yes');
        meta('apple-mobile-web-app-status-bar-style','default');
        meta('apple-mobile-web-app-title',           'Ag-GOAT');
        meta('theme-color',                          '#559FD9');
        meta('viewport', 'width=device-width, initial-scale=1, viewport-fit=cover');
        doc.title = 'Ag-GOAT';
      })();
    ")),

    # when the document loads in, all the numeric inputs are given a required attribute. 
    # This allows us to use the :invalid CSS selector to style empty inputs with a red border

    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('.num-wrap input[type=\"number\"]').attr('required', 'required');
      });
    ")),

    # style definitions
    tags$style(HTML("
      body { background: #fafafa; }

      .agheader { display: flex; justify-content: space-between;
                  align-items: flex-start; margin-bottom: 10px; }

      .agtitle  { font-size: 32px; font-weight: 700; color: #559FD9;
                  line-height: 1; margin: 0; }

      .agsub    { color: #666; font-size: 12px; max-width: 260px;
                  margin-top: 4px; }

      .blurb    { color: #555; font-style: italic; font-size: 13px;
                  margin-bottom: 6px; }

  /* master reset */
      .master-reset {
        width: 100%;
        margin-bottom: 10px;
        background: #fff;
        border: 1px solid #559FD9;
        color: #559FD9;
        font-weight: 600;
        font-size: 13px;
      }
      .master-reset:hover { background: #eef5fc; color: #3d7fb8; }

  /* section cards */
      .section-card {
        background: #f3f3f3;
        border: 2px solid #999;
        border-radius: 8px;
        padding: 8px 10px 6px 10px;
        margin-bottom: 8px;
      }
      .section-title {
        font-weight: 700;
        text-transform: uppercase;
        font-size: 12px;
        letter-spacing: 0.5px;
        margin-bottom: 6px;
        color: var(--part-dark, #333);
      }

  /* param definition */
      .param-row {
        display: flex;
        align-items: center;
        gap: 4px;
        margin-bottom: 4px;
      }
      .param-row:last-child { margin-bottom: 0; }
      .param-label {
        flex: 1 1 auto;
        font-size: 12px;
        color: #333;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        min-width: 0;
      }

  /* badges are <button>s so they can snap the input to min/max on click */
      .range-badge {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 26px;
        height: 22px;
        min-height: 22px !important;
        padding: 0 6px !important;
        border-radius: 11px;
        font-size: 10px;
        font-weight: 600;
        flex-shrink: 0;
        border: none;
        line-height: 1;
        cursor: pointer;
      }
      .range-badge:hover  { opacity: 0.85; }
      .range-badge:active { transform: scale(0.95); }
      .range-light { background: var(--part-light); color: #222; }
      .range-dark  { background: var(--part-dark);  color: #fff; }

      .tiny-btn {
        min-width: 26px;
        height: 28px;
        padding: 0 4px;
        font-size: 14px;
        border: 1px solid #bbb;
        background: #fff;
        border-radius: 4px;
        line-height: 1;
        flex-shrink: 0;
        color: #333;
      }
      .tiny-btn:hover { background: #eee; }

      .num-wrap { flex-shrink: 0; }
      .num-wrap .form-group { margin-bottom: 0; }
      .num-wrap .shiny-input-container { width: auto !important; }
      .num-wrap input.form-control {
        height: 28px;
        padding: 2px 4px;
        text-align: center;
        font-size: 16px;
        width: 62px;
      }

  /* Empty number inputs are :invalid (because required is set via JS) —
         drive the red border off of that. Also catches out-of-range values
         briefly before the server clamps them. */
      .num-wrap input.form-control:invalid {
        border-color: #d9534f;
        background: #fff0f0;
      }

      .reset-btn {
        width: 24px;
        height: 24px;
        border-radius: 50%;
        background: #fff;
        border: 1px solid #aaa;
        font-size: 12px;
        padding: 0;
        line-height: 1;
        flex-shrink: 0;
        color: #555;
      }
      .reset-btn:hover { background: #eee; }

  /* get score button */
      .calc-btn {
        width: 100%;
        min-height: 44px;
        font-size: 15px;
        font-weight: 600;
        background: #61D3B8;
        border-color: #4cbfa3;
        color: #fff;
      }
      .calc-btn:hover, .calc-btn:focus {
        background: #4cbfa3;
        border-color: #3aa68b;
        color: #fff;
      }

  /* ---- right column ---- */
      .image-card {
        background: #f3f3f3;
        border: 2px solid #bbb;
        border-radius: 8px;
        padding: 10px 12px;
        margin-top: 14px;
      }
      .image-card .form-group { margin-bottom: 6px; }
      .scale    { color: #888; font-size: 12px; margin-top: 6px; }

  /* iPad / touch-friendly */
      .btn { min-height: 38px; }
      .agheader, .section-title { -webkit-user-select: none; user-select: none; }
      .irs-handle { width: 28px !important; height: 28px !important;
                    top: 22px !important; }

  /* Stop Safari from auto-zooming text inputs under 16px */
      select, textarea, .form-control { font-size: 16px; }
    "))
  ),

  # navigation buttons
  fluidRow(
    column(12,
           div(style = "display:flex; justify-content:space-between; margin-bottom:10px;",
               actionButton("nav_prev", "← Previous"),
               actionButton("nav_next", "Next →")
           )
    )
  ),
  
  # page system
  tabsetPanel(
    id = "page_tabs",
    type = "hidden",
    
    # input user information page
    tabPanel("user_information",
             
             fluidRow(
               column(12,
                      h2("Enter your information"),
                      textInput("user_name", "Name"),
                      textInput("user_email", "Email")
               )
             )
    ),
    
    # goat selection page    
    tabPanel("goat_selection",
             fluidRow(
               column(12,
                      h2("Select a Goat"),
                      
                      fluidRow(
                        column(6,
                               textInput("owner_name_filter", "Owner Name filter")
                        ),
                        column(6,
                               textInput("owner_id_filter", "Owner ID filter")
                        )
                      ),
                      
                      actionButton("search_goats", "Search", 
                                   class = "btn btn-primary",
                                   style = "margin-bottom: 15px; width: 100%;"),
                      
                      # display area for goat results
                      uiOutput("goat_results")
               )
             )
    ),
    
    # goat visualizer page
    tabPanel("goat_visualizer",
      fluidRow(
        # left side is the params control
        column(width = 4,
    
          div(class = "agheader",
            div(
              h1("Ag-GOAT", class = "agtitle"),
              div(class = "agsub",
                  "Tap +/- to nudge by 0.5, type for a precise value, ",
                  "or hit ↻ to reset.")
            )
          ),
    
          actionButton("reset_all",
                       HTML("&#x21bb; Reset all goat parts"),
                       class = "btn master-reset"),
    
          section_card("Legs", "legs",
            param_row("leg_width",   "leg width"),
            param_row("hock_height", "hock height")
          ),
    
          section_card("Udder Arch", "udder",
            param_row("arch_height",    "arch height"),
            param_row("arch_roundness", "arch roundness"),
            param_row("arch_shape",     "arch shape")
          ),
    
          section_card("Medial", "medial",
            param_row("udder_floor_height",  "udder floor height"),
            param_row("closeness_of_halves", "closeness of halves"),
            param_row("depth_of_medial",     "medial cleft depth")
          ),
    
          section_card("Teats", "teats",
            param_row("teat_placement", "teat placement"),
            param_row("teat_length",    "teat length"),
            param_row("teat_diameter",  "teat diameter")
          ),
    
          br(),
          actionButton("calc_score", "Calculate Linear Appraisal Score",
                       class = "btn calc-btn")
        ),
    
        # right side has the graph and image overlay controls 
        column(width = 8,
          div(class = "agheader",
            div(class = "blurb",
                "ggplot2-based graph that lets you put an image on the graph,",
                br(),
                "and translate, scale, or rotate the image."),
            downloadButton("export_data", "Export Data", class = "btn-default")
          ),
    
          plotOutput("goat_plot", height = "640px"),
    
          div(class = "image-card",
            div(class = "section-title",
                style = "color: #555;", "Image Overlay"),
            fluidRow(
              column(6,
                fileInput("goat_image", "Upload or take a photo of the udder",
                          accept = "image/*")
              ),
              column(6,
                sliderInput("img_opacity", "image visibility",
                            min = 0, max = 100, value = 70, step = 5,
                            post = "%")
              )
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
          )
        )
      )
    )
  )
)

ui
