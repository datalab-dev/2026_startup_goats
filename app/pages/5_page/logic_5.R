# Page 5 logic - the appended observation table
# with options to edit/delete rows
# and export the data as a CSV file.

logic5 <- function(input, output, session, rv, S) {

  # column order matches the UCD Goat Lab export schema (+ goat identity)
  empty_obs <- function() {
    data.frame(
      Timestamp                  = character(),
      `Animal Id`                = character(),
      `Animal Name`              = character(),
      `Owner Name`               = character(),
      UdderDepth                 = numeric(),
      `Rear Udder Height`        = numeric(),
      `Rear Udder Arch`          = numeric(),
      `Medial Suspensory Ligament` = numeric(),
      `Teat Placement`           = numeric(),
      `Teat Diameter`            = numeric(),
      `Teat Length`              = numeric(),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  # append the goats scores as a new row in the observations table
  observeEvent(input$add_observation, {
    sel <- rv$selected_goat
    db  <- rv$goat_data
    name <- owner <- ""
    if (!is.null(db) && !is.null(sel)) {
      # resolve identity columns through the page-2 mapping
      m   <- rv$goat_card_cols
      row <- db[as.character(db[[ goat_col(m, "Animal Id") ]]) == sel, , drop = FALSE]
      if (nrow(row) > 0) {
        name  <- as.character(row[[ goat_col(m, "Animal Name") ]][1])
        owner <- as.character(row[[ goat_col(m, "Owner Name") ]][1])
      }
    }

    new_row <- data.frame(
      Timestamp                  = format(Sys.time(), "%Y-%m-%d %H:%M"),
      `Animal Id`                = sel %||% "",
      `Animal Name`              = name,
      `Owner Name`               = owner,
      UdderDepth                 = input$udder_depth_score,
      `Rear Udder Height`        = input$rear_udder_height_score,
      `Rear Udder Arch`          = NA_real_,
      `Medial Suspensory Ligament` = input$medial_score,
      `Teat Placement`           = input$teat_placement_score,
      `Teat Diameter`            = input$teat_diameter_score,
      `Teat Length`              = input$teat_length_score,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    cur <- rv$observations
    if (is.null(cur)) cur <- empty_obs()
    rv$observations <- rbind(cur, new_row)
  })

  # delete a row and intiate a "are you sure?" confirmation modal
  observeEvent(input$del_obs_request, {
    idx <- as.integer(input$del_obs_request)
    cur <- rv$observations
    if (is.null(cur) || idx < 1 || idx > nrow(cur)) return()
    rv$pending_delete <- idx

    r    <- cur[idx, ]
    who  <- if (nzchar(r$`Animal Name`)) r$`Animal Name` else r$`Animal Id`
    showModal(modalDialog(
      title = "Delete observation?",
      sprintf("Are you sure you want to delete this observation%s? This can't be undone.",
              if (nzchar(who)) paste0(" for ", who) else ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_del_obs", "Delete", class = "btn btn-primary",
                     style = "background:var(--error);border-color:var(--error);")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_del_obs, {
    removeModal()
    idx <- rv$pending_delete
    cur <- rv$observations
    rv$pending_delete <- NULL
    if (is.null(idx) || is.null(cur) || idx < 1 || idx > nrow(cur)) return()
    rv$observations <- cur[-idx, , drop = FALSE]
  })

  # edit a row, going back to the input page with the scores pre-filled and the row removed (so it can be re-added on save)
  observeEvent(input$edit_obs, {
    idx <- as.integer(input$edit_obs)
    cur <- rv$observations
    if (is.null(cur) || idx < 1 || idx > nrow(cur)) return()
    r <- cur[idx, ]
    updateNumericInput(session, "udder_depth_score",       value = r$UdderDepth)
    updateNumericInput(session, "rear_udder_height_score", value = r$`Rear Udder Height`)
    updateNumericInput(session, "medial_score",            value = r$`Medial Suspensory Ligament`)
    updateNumericInput(session, "teat_placement_score",    value = r$`Teat Placement`)
    updateNumericInput(session, "teat_diameter_score",     value = r$`Teat Diameter`)
    updateNumericInput(session, "teat_length_score",       value = r$`Teat Length`)
    rv$selected_goat <- as.character(r$`Animal Id`)

    # drop the row being edited so re-adding doesn't duplicate it
    rv$observations <- cur[-idx, , drop = FALSE]
    S$nav_to(4)
  })

  # summary of text
  output$obs_summary <- renderUI({
    n <- if (is.null(rv$observations)) 0 else nrow(rv$observations)
    p(class = "page-sub", style = "margin-bottom:16px;",
      if (n == 0) "No observations recorded yet."
      else HTML(sprintf("<b>%d goat%s recorded.</b> Review below, add another, or export.",
                        n, if (n == 1) "" else "s")))
  })

  # review table
  output$obs_review <- renderUI({
    obs <- rv$observations
    if (is.null(obs) || nrow(obs) == 0) {
      return(div(class = "empty-state",
                 span(class = "icon", "\U0001F4DD"),
                 "Nothing here yet. Go back to add a goat's measurements."))
    }

    head <- tags$tr(
      tags$th("#"), tags$th("Goat"), tags$th("ID"),
      tags$th("Udder Dpt"), tags$th("Rear Ht"), tags$th("Medial"),
      tags$th("Teat Plc"), tags$th("Teat Dia"), tags$th("Teat Len"),
      tags$th(style = "text-align:right;", "Edit")
    )

    rows <- lapply(seq_len(nrow(obs)), function(i) {
      r <- obs[i, ]
      tags$tr(
        tags$td(i),
        tags$td(tags$b(if (nzchar(r$`Animal Name`)) r$`Animal Name` else "-")),
        tags$td(r$`Animal Id`),
        tags$td(r$UdderDepth), tags$td(r$`Rear Udder Height`),
        tags$td(r$`Medial Suspensory Ligament`), tags$td(r$`Teat Placement`),
        tags$td(r$`Teat Diameter`), tags$td(r$`Teat Length`),
        tags$td(style = "text-align:right; white-space:nowrap;",
          tags$button(class = "row-action", title = "Edit",
            `aria-label` = "Edit observation",
            onclick = sprintf("Shiny.setInputValue('edit_obs','%d',{priority:'event'});", i),
            HTML("&#9998;")),
          tags$button(class = "row-action del", title = "Delete",
            `aria-label` = "Delete observation",
            onclick = sprintf("Shiny.setInputValue('del_obs_request','%d',{priority:'event'});", i),
            HTML("&#128465;"))
        )
      )
    })

    div(class = "obs-scroll",
        tags$table(class = "obs-table",
          tags$thead(head),
          tags$tbody(do.call(tagList, rows))))
  })

  # export button 
  output$export_button <- renderUI({
    n <- if (is.null(rv$observations)) 0 else nrow(rv$observations)
    if (n == 0) {
      tags$button("Export Data", class = "btn btn-primary", disabled = NA,
                  title = "Add at least one observation first")
    } else {
      downloadButton("export_data", "Export Data", class = "btn btn-primary",
        onclick = "Shiny.setInputValue('did_export', Math.random(), {priority:'event'});")
    }
  })

  # csv download handler
  output$export_data <- downloadHandler(
    filename = function() sprintf("goat_observations-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      obs <- rv$observations
      if (is.null(obs)) obs <- empty_obs()
      write.csv(obs, file, row.names = FALSE)
    }
  )
}
