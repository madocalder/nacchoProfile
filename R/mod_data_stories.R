#' data_stories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_stories_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'data_stories_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          class = "key-findings-content",
          h1("Data Stories"),
          openMenu(),
          p("")
        ),
        right = NULL
      )
    )

  )
}

#' data_stories Server Functions
#'
#' @noRd
mod_data_stories_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_data_stories_ui("data_stories_1")

## To be copied in the server
# mod_data_stories_server("data_stories_1")
