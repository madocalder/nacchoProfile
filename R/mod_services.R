#' services UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_services_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' services Server Functions
#'
#' @noRd 
mod_services_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_services_ui("services_1")
    
## To be copied in the server
# mod_services_server("services_1")
