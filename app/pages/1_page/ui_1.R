# Page 1 - Wlecome/Personal Info

page1_ui <- function() {
  div(class = "page-enter",
    div(class = "card-panel",
      span(class = "blob blob-tr"),
      span(class = "blob blob-bl"),
      h2(class = "page-title", "Welcome to GOATvzn"),
      p(class = "page-sub", "Let's start with your information."),

      div(class = "field", id = "user_name_field",
        textInput("user_name", labelMandatory("Name"),
                  placeholder = "First Last", width = "100%")
      ),
      div(class = "field", id = "user_email_field",
        textInput("user_email", labelMandatory("Email"),
                  placeholder = "you@example.com", width = "100%"),
        uiOutput("email_error")
      )
    )
  )
}
