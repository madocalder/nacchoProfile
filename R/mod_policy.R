#' policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_policy_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' policy Server Functions
#'
#' @noRd 
mod_policy_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_policy_ui("policy_1")
    
## To be copied in the server
# mod_policy_server("policy_1")
