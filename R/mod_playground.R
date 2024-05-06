#' playground UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_playground_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' playground Server Functions
#'
#' @noRd 
mod_playground_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_playground_ui("playground_1")
    
## To be copied in the server
# mod_playground_server("playground_1")
