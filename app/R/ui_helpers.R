# UI helpers for the compact, dashboard-style parameter cards
# these functions are used repeatedly in ui.R to create the parameter controls and section cards

library(shiny)

# a field label with a red required asterisk (HCI #5: error prevention)
labelMandatory <- function(label) {
  tagList(label, span(class = "req", style = "color:#E24B4A;", " *"))
}

param_row <- function(id, label) {
  bounds <- PARAM_BOUNDS[[id]]
  value <- PARAM_DEFAULTS[[id]]

  div(class = "param-row",
    span(class = "param-label", label),
    actionButton(paste0(id, "_min_badge"), bounds$min,
                 class = "range-badge range-light",
                 title = paste("set to", bounds$min)),
    actionButton(paste0(id, "_minus"), HTML("&minus;"), class = "tiny-btn"),
    div(class = "num-wrap",
      numericInput(id, label = NULL,
                   value = value,
                   min = bounds$min,
                   max = bounds$max,
                   step = bounds$step,
                   width = "62px")
    ),
    actionButton(paste0(id, "_plus"), HTML("+"), class = "tiny-btn"),
    actionButton(paste0(id, "_max_badge"), bounds$max,
                 class = "range-badge range-dark",
                 title = paste("set to", bounds$max)),
    actionButton(paste0(id, "_reset"), HTML("&#x21bb;"),
                 class = "reset-btn", title = "reset")
  )
}

section_card <- function(title, palette_key, ...) {
  pal <- SECTION_PALETTE[[palette_key]]
  div(
    class = paste0("section-card part-", palette_key),
    style = sprintf(
      "border-color: %s; --part-light: %s; --part-main: %s; --part-dark: %s;",
      pal$border, pal$light, pal$main, pal$dark
    ),
    div(class = "section-title", title),
    ... # this is where the parameter rows get inserted when we call section_card() in ui.R
  )
}
