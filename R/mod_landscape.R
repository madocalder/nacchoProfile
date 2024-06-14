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
                selected = "Population Served"
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
                choices = list(
                  "Rurality" = "urbanrural2022",
                  "Governance" = "c0govcat",
                  "LHD Size" = "c0popcat3",
                  "Region" = "censusregion"
                ),
                selected = "LHD Size"
              )
            )
          ),

          hr(),
          br(),
          br(),

          # add main page content here ----------------------------------------------

          highcharter::highchartOutput(ns("figure"))

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


      naccho.viz::create_single_bar(
        data = master_data,
        x_variable = "fig_filter",
        y_variable = "perc",
        title_text = "Test",
        subtitle_text = "Test",
        caption_text = "<b> Source: </b> 2022 National Profile of Local Health
        Departments. National Association of County and City Health Officials.
        Washington, DC. (2024).",
        y_label = "",
        y_max = 100,
        y_format = "{value}%",
        legend_enable = TRUE,
        legend_title = "",
        tooltip = "Topic: {point.fig_filter} <br> Percent: {point.perc}%",
        allow_export = FALSE,
        select_theme = naccho.viz::naccho_minimal_theme
      )
    })

  })
}

## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
