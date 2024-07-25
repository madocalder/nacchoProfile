#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # stories server
  mod_data_stories_server("data_stories_1")


  # add urls for the pages
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if(is.null(query$tab))
      return()

    updateTabsetPanel(session,
                      "tabs",
                      selected = query$tab)
  })


  # footer modal
  # modal
  observeEvent(
    input$citation,
    bsutils::modal_show("modal")
  )

  # key findings
  mod_key_findings_server("key_findings_1")
  mod_landscape_server("landscape_1")
  mod_workforce_server("workforce_1")

}
