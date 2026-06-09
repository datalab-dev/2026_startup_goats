# Page 1 logic - inline email validation (HCI #9).

# email validation using simple regex
# common email pattern: 
  # one or more characters, followed by @, 
  # followed by one or more characters, followed by ., 
  # followed by one or more characters


logic1 <- function(input, output, session, rv, S) {
  output$email_error <- renderUI({
    em <- input$user_email %||% ""
    if (nzchar(trimws(em)) && !grepl(".+@.+\\..+", em)) {
      div(class = "help-error", style = "display:block;",
          "Please enter a valid email address.")
    }
  })
}