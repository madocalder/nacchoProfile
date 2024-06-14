#' landscape UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landscape_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'landscape_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          class = "col-pad",
          openMenu(),
          div(
            class = "row",
            align = "center",
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Select Topic"),
                choices = list(
                  "Population Served" = "totpop",
                  "Geographies Served",
                  "Governance",
                  "Structure",
                  "Board of Health"
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
                  "Line Graph",
                  "Pie Chart",
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

          # highcharter::highchartOutput(ns("figure"))

          # end of main content -----------------------------------------------------

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
                   This allows you to distinguish columns using a pattern fill."
                ), style = "margin-bottom: -20px;"
              )
            )
          )
        )
      )
    )
  )
}

#' landscape Server Functions
#'
#' @noRd
mod_landscape_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # selected by user from input$filter
    data_filter <- reactive({
      get(input$filter)
    })

    var <- reactive ({
      get(input$topic)
    })

    data <- reactive({
      master_data %>%
        dplyr::group_by(filter) %>%
        dplyr::summarise(value = sum(vay())) %>%
        dplyr::mutate(perc = paste0(round(value/sum(value)*100,1), "%"))

    })

    output$figure <- highcharter::renderHighchart({

    })


  })
}

## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
