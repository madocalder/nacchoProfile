#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'home_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          h4("Test"),
          openMenu(),
          p("This is the home page")
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

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
