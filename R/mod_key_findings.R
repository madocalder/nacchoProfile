#' key_findings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_key_findings_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'key_findings_tab'",
      div(
        div(
          class = "key-findings-head",
          h1("Key Findings")
        ),
        openMenu(),
        div(
          class = "key-findings-content",

          # carousel
          bsutils::carousel(
            indicators = TRUE,
            controls = FALSE,
            class = "carousel-dark",
            bsutils::carouselItem(
              div(
                class = "row",
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              div(
                class = "row",
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              div(
                class = "row",
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-4",
                  align = "center",
                  bslib::card(
                    height = 350,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                )
              ),
              interval = 60000L
            )
          ),

          # download key findings
          div(
            align = "center",
            br(),
            br(),
            actionLink(
              inputId = ns("download"),
              p("Download our key findings", style = "padding-top: 20px;"),
              icon = tags$i(class = "fa-solid fa-circle-down"),
              class = "link-no-underline"
            )
          ),

          # modal for key findings download
          bsutils::modal(
            id = ns("modal"),
            bsutils::modalHeader(
              bsutils::modalTitle(
                p("What do you want to download?")
              )
            ),
            bsutils::modalBody(
              p("Some information about how you can use this information
                  for example you can add more visualizations or use it to
                  do this and that."),
              br(),
              br(),
              downloadLink(
                outputId = ns("download_slides"),
                label = p("Download our key findings slide deck")
              ),
              br(),
              downloadLink(
                outputId = ns("download_pdf"),
                label = p("Download our key findings brief")
              )
            )
          )
        )
      )
    )
  )
}

#' key_findings Server Functions
#'
#' @noRd
mod_key_findings_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # modal
    observeEvent(
      input$download,
      bsutils::modal_show(ns("modal"))
    )

    # download slides
    output$download_slides <-
      downloadHandler(
        filename = "key_findings_slides.pdf",
        content = function(file) {
          file.copy("", file) # add content www/
        }
      )

    # download pdf
    output$download_pdf <-
      downloadHandler(
        filename = "key_findings_report.pdf",
        content = function(file) {
          file.copy("", file) # add content www/
        }
      )

  })
}

## To be copied in the UI
# mod_key_findings_ui("key_findings_1")

## To be copied in the server
# mod_key_findings_server("key_findings_1")
