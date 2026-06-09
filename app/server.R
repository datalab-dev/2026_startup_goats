# server bundles all the logic for the app from the various pages

server <- function(input, output, session) {

  rv <- reactiveValues(
    page          = 1,
    goat_data     = NULL,
    goat_card_cols = NULL,          # canonical->actual column mapping (page 2)
    missing_cols   = character(0),  # required cols not found in the upload
    mapping_error  = NULL,          # inline validation msg for the mapping UI
    selected_goat = NULL,
    observations  = NULL,
    upload_error  = NULL,
    export_status = "success",
    export_time   = NULL,
    export_count  = NULL,
    pending_delete = NULL
  )
  S <- new.env(parent = emptyenv())
  
  # call all of the logic functions for the different pages, passing in the reactive values and session info as needed
  logic_nav(input, output, session, rv, S)
  logic1(input, output, session, rv, S)
  logic2(input, output, session, rv, S)
  logic3(input, output, session, rv, S)
  logic4(input, output, session, rv, S)
  logic5(input, output, session, rv, S)
  logic6(input, output, session, rv, S)
}

server
