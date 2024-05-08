#' key_findings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_key_findings_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'key_findings_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          h4("Key Findings"),
          openMenu(),
          p("This is the Key findings page")
        ),
        right = div(
          div(
            class = "row color-block",
            div(
              class = "col-sm-4",
              br(),
              actionLink(
                inputId = "about",
                p("About", style = "font-size: 16px; font-weight: 700;"),
                icon = icon("circle-info"),
                class = "link-no-underline"
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              actionLink(
                inputId = "context",
                p("Context", style = "font-size: 16px; font-weight: 700;"),
                icon = tags$i(class = "fa-solid fa-list"),
                class = "link-no-underline"
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              actionLink(
                inputId = "more",
                p("More", style = "font-size: 16px; font-weight: 700;"),
                icon = tags$i(class = "fa-solid fa-key"),
                class = "link-no-underline"
              )
            )
          ),
          div(
            style = "padding-left: 12%;",
            shinyWidgets::materialSwitch(
              inputId = "color_palette",
              p(
                HTML("<b>Enable Color Patters?</b> <br>
                   This allows you to distinguish columns using a pattern fill."
                ), style = "margin-bottom: -20px;"
              )
            )
          )
        )
      )
    )
  )
}

#' key_findings Server Functions
#'
#' @noRd
mod_key_findings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_key_findings_ui("key_findings_1")

## To be copied in the server
# mod_key_findings_server("key_findings_1")
