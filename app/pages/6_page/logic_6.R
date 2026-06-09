# Page 6 logic - export confirmation
logic6 <- function(input, output, session, rv, S) {

  observeEvent(input$did_export, {
    rv$export_status <- "success"
    rv$export_time   <- Sys.time()
    rv$export_count  <- if (is.null(rv$observations)) 0 else nrow(rv$observations)
  })

  output$export_confirm <- renderUI({
    status <- rv$export_status %||% "success"

    if (identical(status, "fail")) {
      return(div(class = "confirm show",
        div(class = "blobs", span(class = "g"), span(class = "t"), span(class = "b")),
        h2(class = "c-title fail", "Export failed"),
        p(class = "c-meta", "Something went wrong. Check your storage and try again."),
        div(class = "btn-row", style = "justify-content:center;",
          actionButton("retry_export", "Retry Export", class = "btn btn-primary"),
          actionButton("start_over", "Go Back", class = "btn btn-default"))
      ))
    }

  
    n    <- rv$export_count %||% (if (is.null(rv$observations)) 0 else nrow(rv$observations))
    when <- if (!is.null(rv$export_time)) format(rv$export_time, "%b %d, %Y · %H:%M") else "" # adds timestamp if available, would be bad if it wasn't available

    div(class = "confirm show",
      div(class = "blobs", span(class = "g"), span(class = "t"), span(class = "b")),
      h2(class = "c-title ok", "Successfully Exported!"),
      p(class = "c-meta",
        sprintf("%d observation%s · CSV · Saved to Files%s",
                n, if (n == 1) "" else "s",
                if (nzchar(when)) paste0(" · ", when) else "")),
      div(class = "btn-row", style = "justify-content:center;",
        actionButton("edit_again", "Edit Again", class = "btn btn-outline"),
        actionButton("start_over", "Start New Session", class = "btn btn-default"))
    )
  })
}
