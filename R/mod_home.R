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
<<<<<<< HEAD
      div(
        class = "col-pad row home-content",
        div(
          class = "col-sm-8 col-pad-right",
          p("welcome to the", style = "font-size: 30px; font-weight: 400 !important;"),
          h1(HTML("NACCHO Profile <br> Study Dashboard"), style = "font-size: 60px; text-transform: none !important;"),
          br(),
          bslib::accordion(
            id = "cord",
            open = FALSE,
            multiple = TRUE,
            style = "padding-top: 0px;",

            bslib::accordion_panel(
              title = h2("About the Survey"),
              value = "cord_about"
            ),

            bslib::accordion_panel(
              title = h2("Methods"),
              value = "cord_methods"
            ),

            bslib::accordion_panel(
              title = h2("Access the Data"),
              value = "cord_access"
            ),

            bslib::accordion_panel(
              title = h2("Acknowledgments"),
              value = "cord_ack"
            ),

            bslib::accordion_panel(
              title = h2("Contact and Questions"),
              value = "cord_contact"
            )
          )
        ),
        div(
          class = "col-sm-4 col-pad-left"
=======
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
>>>>>>> a279d5c4edcf9f7324add51ae188a2aa50a8248a
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
