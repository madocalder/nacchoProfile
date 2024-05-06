#' finance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_finance_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' finance Server Functions
#'
#' @noRd 
mod_finance_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_finance_ui("finance_1")
    
## To be copied in the server
# mod_finance_server("finance_1")
