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
      div(
        class = "col-pad",
        p("This is the Key Findings Page")
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
