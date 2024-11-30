#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

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
  observeEvent(
    input$citation,
    bsutils::modal_show("modal")
  )


  # modules
  mod_overview_server("overview_1")
  mod_key_findings_server("key_findings_1")
  mod_landscape_server("landscape_1")
  mod_workforce_server("workforce_1")
  mod_finance_server("finance_1")
  mod_services_server("services_1")
  mod_data_stories_server("data_stories_1")
  mod_topic_areas_server("topic_areas_1")



  #overview modal pop up

  # Reactive value to track if the modal has been shown
  modal_shown <- reactiveVal(FALSE)


  observe({
    if (!is.null(input$tabs) && input$tabs == "overview_tab" && !modal_shown()) {
      showModal(modalDialog(
        title = "About the Denominator of LHDs",
        HTML("<p>For the purposes of surveying and developing national estimates of
              the LHD landscape, NACCHO designed and utilizes a methodology to account
              for the most unique individuals in the U.S. population without \"double counting.\"
              Therefore, the denominator of LHDs used for surveying and national
              estimation purposes will always be smaller than the actual number of LHDs in the U.S.,
              while still covering all local jurisdictions uniquely.
              NACCHO is in frequent contact and consultation with state and local
              officials to update the denominator. There were 2,512 LHDs included
              in the 2022 Profile study population.</p>"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))

      modal_shown(TRUE)
    }
  })



}
