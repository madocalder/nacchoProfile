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
                strong("Topic"),
                choices = workforce_topics,
                selectize = TRUE
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("view"),
                strong("Visualization Type"),
                choices = c(
                  "Bar Graph",
                  "Line Graph",
                  "Pie Chart",
                  "Table"
                )
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters
              )
            )
          ),

          hr(),

          # add main page content here ----------------------------------------------


          # display occupational classifications is selected
          conditionalPanel(
            condition = paste0('input[\'', ns('topic'), "\'] == \'occupation_class\'"),
            div(
              class = "viz-content row",
              div(
                class = "col-sm-3",
                br(),
                strong("Select Classification"),
                div(
                  class = "checkboxes",
                  checkboxGroupInput(
                    ns("classifications"),
                    "",
                    workforce_classifications,
                  )
                )
              ),
              div(
                class = "col-sm-9"
              )
            )
          ),

          # display if occupational classifications is not selected
          conditionalPanel(
            condition = paste0('input[\'', ns('topic'), "\'] != \'occupation_class\'"),
            div(
              class = "viz-content-buttons",
              div(
                radioButtons(
                  ns("type"),
                  "",
                  choices = list("FTE Grouping" = "FTE_numcat",
                                 "Total FTEs" = "c5q37",
                                 "Per Capita FTEs" = "fte_bypop"),
                  inline = TRUE
                ),
                br()
              ),
              br(),

              conditionalPanel(
                condition = paste0('input[\'', ns('type'), "\'] == \'FTE_numcat\'"),
                highcharter::highchartOutput(ns("figure"), height = 575)
              ),

              conditionalPanel(
                condition = paste0('input[\'', ns('type'), "\'] != \'FTE_numcat\'"),
                highcharter::highchartOutput(ns("histogram"), height = 575)
              )
            )
          )

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

#' workforce Server Functions
#'
#' @noRd
mod_workforce_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      meta %>%
        dplyr::filter(field == input$type)
    })



    # figure output -----------------------------------------------------------
    # used for bar and line graphs
    output$figure <- highcharter::renderHighchart({

      data <- return_survey_weighted_counts("FTE_numcat")

      set_column(data, y_label = "% of LHDs") %>%
        viz_options(
          viz_title = "Percent of LHDs within FTE Categories",
          viz_caption = "<b> Source: </b> 2022 National Profile of Local Health
          Departments. National Association of County and City Health Officials.
          Washington, DC. (2024).",
          tooltip = "{point.y}% of LHDs employ {point.category} FTEs at their agency."
        ) %>%
        viz_export()
    })

    # save the figure as a proxy to redraw below
    proxy <- highcharter::highchartProxy(ns("figure"))


    # used for first histogram
    output$histogram <- highcharter::renderHighchart({
      data <- create_hist("c5q37")

      set_hist(data) %>%
        viz_options(
          viz_title = paste("Histogram Showing the Number of LHDs by the Number of Total FTEs"),
          viz_caption = "<b> Source: </b> 2022 National Profile of Local Health
                          Departments. National Association of County and City Health Officials.
                          Washington, DC. (2024).",
          tooltip = "{point.y} LHDs have between {point.low_bin} and {point.high_bin} total FTEs"
        ) %>%
        viz_export()

    })


    # save the figure as a proxy to redraw below
    hist_proxy <- highcharter::highchartProxy(ns("histogram"))



    # this observe is needed to redraw the figure based on the input selected
    # by the user. It also filters and stores tables based on the inputs
    # combinations selected by the user.
    observe ({

      if (input$type == "fte_bypop") {
        data <- create_hist("fte_bypop")

        hist_proxy %>%
          proxy_to_hist(data, filter_meta())


      } else if (input$type == "c5q37") {
        data <- create_hist("c5q37")

        hist_proxy %>%
          proxy_to_hist(data, filter_meta())
      }
    })

  })
}

## To be copied in the UI
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
