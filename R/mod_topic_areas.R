#' topic_areas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_topic_areas_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' topic_areas Server Functions
#'
#' @noRd 
mod_topic_areas_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_topic_areas_ui("topic_areas_1")
    
## To be copied in the server
# mod_topic_areas_server("topic_areas_1")
