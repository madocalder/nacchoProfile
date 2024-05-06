#' workforce UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_workforce_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' workforce Server Functions
#'
#' @noRd 
mod_workforce_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_workforce_ui("workforce_1")
    
## To be copied in the server
# mod_workforce_server("workforce_1")
