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
                choices = landscape_topics,
                selectize = TRUE
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
                choices = landscape_filters,
                selected = "LHD Size"
              )
            )
          ),

          hr(),
          br(),
          br(),

          # add main page content here ----------------------------------------------

          div(
            class = "viz_content",
            highcharter::highchartOutput(ns("figure"), height = 500)
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


    filter_data <- reactive({
      master_data %>%
        haven::as_factor() %>%
        dplyr::mutate(fig_filter = stringr::str_to_title(get(input$filter))) %>%
        dplyr::mutate(value = get(input$topic))
    })


    output$figure <- highcharter::renderHighchart({

      master_data <-
        filter_data() %>%
        dplyr::group_by(fig_filter) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(perc = round(value/sum(value)*100,1))

      if (input$view == "Bar Graph") {
        naccho.viz::create_single_bar(
          data = master_data,
          x_variable = "fig_filter",
          y_variable = "perc",
          title_text = "Percent of U.S. Population Served by LHDs",
          subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
          caption_text = "<b> Source: </b> 2022 National Profile of Local Health
          Departments. National Association of County and City Health Officials.
          Washington, DC. (2024).",
          y_label = "Percent of LHDs",
          y_max = 100,
          y_format = "{value}%",
          my_colors = naccho.viz::naccho_orange,
          legend_enable = FALSE,
          legend_title = "",
          tooltip = "{point.perc}% of LHDs serve {point.fig_filter} populations",
          allow_export = FALSE,
          select_theme = naccho.viz::naccho_minimal_theme
        )
      } else if (input$view == "Pie Chart") {
        naccho.viz::create_pie(
          data = master_data,
          variable = "fig_filter",
          value = "perc",
          my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green, naccho.viz::naccho_blue, naccho.viz::naccho_pink, naccho.viz::naccho_teal),
          legend_enable = TRUE,
          title_text = "Percent of U.S. Population Served by LHDs",
          subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
          caption_text = "<b> Source: </b> 2022 National Profile of Local Health
          Departments. National Association of County and City Health Officials."
        )
      }
    })

  })
}

## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
