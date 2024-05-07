#' workforce UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_workforce_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'workforce_tab'",

      div(
        class = "row",
        div(
          class = "col-sm-9 col-pad",
          div(
            class = "row",
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Select Topic"),
                choices = c(
                  "Expenditures",
                  "Funding",
                  "Other thing"
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

        ),
        div(
          class = "col-sm-3 learn-more",
          div(
            class = "row color-block",
            div(
              class = "col-sm-4",
              align = "center",
              br(),
              actionLink(
                inputId = ns("about"),
                "About",
                icon = icon("circle-info"),
                class = "link-no-underline"
              )
            ),
            div(
              class = "col-sm-4",
              align = "center",
              br(),
              actionLink(
                inputId = ns("context"),
                "Context",
                icon = tags$i(class = "fa-solid fa-list"),
                class = "link-no-underline"
              )
            ),
            div(
              class = "col-sm-4",
              align = "center",
              br(),
              actionLink(
                inputId = ns("more"),
                "More",
                icon = tags$i(class = "fa-solid fa-key"),
                class = "link-no-underline"
              )
            )
          ),
          br(),
          div(
            style = "padding-left: 12%;",
            shinyWidgets::materialSwitch(
              inputId = ns("color_palette"),
              p(
                HTML("<b>Enable Color Patters?</b> <br>
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

#' workforce Server Functions
#'
#' @noRd
mod_workforce_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
