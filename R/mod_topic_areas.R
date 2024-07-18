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
    conditionalPanel(
      condition = "input.tabs == 'topics_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          class = "row",
          openMenu(),
          div(
            class = "col-xl-1 vertical-layout",
            div(
              class = "hover-text",
              actionButton(ns("jurisdictions_tab"),
                           "",
                           icon = tags$i(class = "fa-light fa-building-columns",
                                         style = "font-size: 26px; text-align: center;"),
                           class = "icon-button"),
              span(class = "hover-text-content", "Jurisdiction & Governance")
            ),
            div(
              class = "hover-text",
              actionButton(ns("partnerships_tab"),
                           "",
                           icon = tags$i(class = "fa-light fa-handshake",
                                         style = "font-size: 26px; text-align: center;"),
                           class = "icon-button"),
              span(class = "hover-text-content", "Partnerships")
            ),
            div(
              class = "hover-text",
              actionButton(ns("leadership_tab"),
                           "",
                           icon = tags$i(class = "fa-light fa-user-tie-hair-long",
                                         style = "font-size: 26px; text-align: center;"),
                           class = "icon-button"),
              span(class = "hover-text-content", "Agency Leadership")
            ),
            div(
              class = "hover-text",
              actionButton(ns("planning_tab"),
                           "",
                           icon = tags$i(class = "fa-light fa-memo-circle-check",
                                         style = "font-size: 26px; text-align: center;"),
                           class = "icon-button"),
              span(class = "hover-text-content", "Planning, Assessment, and Accreditation")
            ),
            div(
              class = "hover-text",
              actionButton(ns("policy_tab"),
                           "",
                           icon = tags$i(class = "fa-light fa-bullhorn",
                                         style = "font-size: 26px; text-align: center;"),
                           class = "icon-button"),
              span(class = "hover-text-content", "Policy")
            ),
            div(
              class = "hover-text",
              actionButton(ns("informatics_tab"),
                           "",
                           icon = tags$i(class = "fa-light fa-gear-code",
                                         style = "font-size: 26px; text-align: center;"),
                           class = "icon-button"),
              span(class = "hover-text-content", "Informatics")
            )
          ),
          div(
            class = "col-xl-11",
            div(
              class = "row",
              align = "center",
              div(
                class = "col-sm-4",
                br(),
                selectInput(
                  inputId = ns("topic"),
                  strong("Select Topic"),
                  choices = c(
                    "Expenditures",
                    "Funding",
                    "Other lightg"
                  ),
                  selected = "Expenditures"
                )
              ),
              div(
                class = "col-sm-4",
                br(),
                selectInput(
                  inputId = ns("view"),
                  strong("Select View"),
                  choices = c(
                    "Bar Graph",
                    "Map",
                    "Table"
                  ),
                  selected = "Bar Graph"
                )
              ),
              div(
                class = "col-sm-4",
                br(),
                selectInput(
                  inputId = ns("filter"),
                  strong("Filter"),
                  choices = c(
                    "Rurality",
                    "Governance",
                    "LHD Size"
                  ),
                  selected = "Rurality"
                )
              )
            ),

            hr(),
            br(),
            br(),

            # add main page content here ----------------------------------------------

            p("This is the workforce Page"),

            # end of main content -----------------------------------------------------

          )
        ),
        right = div(
          div(
            class = "row color-block",
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("about"),
                "",
                icon = tags$i(class = "fa-light fa-circle-info",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline"),
              span(class = "hover-text-content-side", "About")
            ),
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("context"),
                "",
                icon = tags$i(class = "fa-light fa-list",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline"),
              span(class = "hover-text-content-side", "Definitions and Notes")
            ),
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("more"),
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline"),
              span(class = "hover-text-content-side", "Key Takeaways")
            )
          ),
          br(),
          div(
            style = "padding-left: 12%;",
            shinyWidgets::materialSwitch(
              inputId = ns("color_palette"),
              p(
                HTML("<b>Enable Color Patterns?</b> <br>
                   Distinguish bars with patterns."
                ), style = "margin-bottom: -20px;"
              )
            )
          )
        )
      )
    )
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
