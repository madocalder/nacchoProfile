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
              class = "col-sm-4 min-height-400",
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
                choices = landscape_filters[-1],
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


    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("totpop", "tenure")) {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[-1]
        )
      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters
        )
      }
    })


    # filter data -------------------------------------------------------------
    filter_data <- reactive({

      if (input$topic == "totpop") {
        req(input$filter != "No Filter") # prevent error message from popping
        master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(fig_value = get(input$topic)) %>%
          dplyr::group_by(fig_filter) %>%
          dplyr::summarise(fig_value = sum(fig_value)) %>%
          dplyr::mutate(n = round(fig_value/sum(fig_value)*100,1)) %>%
          dplyr::mutate(fig_filter = factor(fig_filter)) %>% # factor for correct ordering
          dplyr::arrange(fig_filter) %>%
          dplyr::mutate(subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup()


      } else if (input$topic == "tenure") {
        req(input$filter != "No Filter") # prevent error message from popping
        master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(fig_value = get(input$topic)) %>%
          dplyr::group_by(fig_filter) %>%
          dplyr::summarise(n = mean(fig_value, na.rm = TRUE)) %>%
          dplyr::mutate(subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup()


      } else if (input$filter == "No Filter") {
        master_data %>%
          dplyr::mutate(fig_filter = get(input$topic)) %>%
          tidyr::drop_na(fig_filter) %>%
          haven::as_factor() %>%
          dplyr::group_by(fig_filter) %>%
          dplyr::summarise(value = dplyr::n()) %>%
          dplyr::mutate(n = round(value/sum(value)*100,1)) %>%
          dplyr::mutate(subtitle = "Across All LHDs") %>%
          dplyr::ungroup()


        # nested if/else: if any other filter is selected...
      } else {
        req(input$filter != "No Filter") # prevent error message from popping
        master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(fig_cat = get(input$topic)) %>%
          tidyr::drop_na(fig_cat) %>%
          dplyr::group_by(fig_filter, fig_cat) %>%
          dplyr::summarise(fig_value = dplyr::n()) %>%
          dplyr::mutate(n = round(fig_value/sum(fig_value)*100,1)) %>%
          dplyr::mutate(fig_filter = factor(fig_filter)) %>%
          dplyr::arrange(fig_filter) %>%
          dplyr::ungroup()

      }  # close topic class if/else
    })


    # filter meta data --------------------------------------------------------
    filter_meta <- reactive({
      meta %>%
        dplyr::filter(field == input$topic)
    })


    # figure output -----------------------------------------------------------
    output$figure <- highcharter::renderHighchart({

      req(filter_data()) # prevents error message from popping

      if (input$view == "Bar Graph" & (input$topic %in% c("totpop", "tenure") | input$filter == "No Filter")) {
        naccho.viz::create_single_bar(
          data = filter_data(),
          x_variable = "fig_filter",
          y_variable = "n",
          title_text = filter_meta()$title,
          subtitle_text = filter_data()$subtitle[[1]],
          caption_text = filter_meta()$caption,
          y_label = filter_meta()$y_label,
          y_max = filter_meta()$y_max,
          y_format = filter_meta()$y_format,
          my_colors = naccho.viz::naccho_orange,
          legend_enable = FALSE,
          legend_title = "",
          tooltip = "",
          allow_export = FALSE,
          select_theme = naccho.viz::naccho_minimal_theme
        )
      } else if (input$view == "Bar Graph" & input$filter != "No Filter") {
        naccho.viz::create_grouped_bar(
          data = filter_data(),
          x_variable = "fig_cat",
          y_variable = "n",
          x_group = "fig_filter",
          title_text = filter_meta()$title,
          subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
          caption_text = filter_meta()$caption,
          y_label = filter_meta()$y_label,
          y_max = filter_meta()$y_max,
          y_format = filter_meta()$y_format,
          my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green, naccho.viz::naccho_blue, naccho.viz::naccho_pink, naccho.viz::naccho_teal),
          legend_enable = TRUE,
          legend_title = "",
          tooltip = "",
          allow_export = FALSE,
          select_theme = naccho.viz::naccho_minimal_theme
        )
      } else if (input$view == "Pie Chart") {
        naccho.viz::create_pie(
          data = filter_data(),
          variable = "fig_filter",
          value = "n",
          my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green, naccho.viz::naccho_blue, naccho.viz::naccho_pink, naccho.viz::naccho_teal),
          legend_enable = TRUE,
          title_text = filter_meta()$title,
          subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
          caption_text = filter_meta()$caption
        ) %>%
          naccho.viz::add_logo(
            "https://www.naccho.org/img/footerlog.svg",
            width = 240L,
            height = 80L
          )
      }
    }) %>% debounce(100)
  })
}

## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
