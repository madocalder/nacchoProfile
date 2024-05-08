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
          right = div(
            class = "col-sm-4 col-pad-left"
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
