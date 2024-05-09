#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # footer modal
  # modal
  observeEvent(
    input$citation,
    bsutils::modal_show("modal")
  )

  # key findings
  mod_key_findings_server("key_findings_1")

}
