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
    tags$head(
      tags$script(src = "www/script.js") # Correct path if the file is in inst/app/www/
    ),
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
            highcharter::highchartOutput(ns("figure"), height = 575)
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
                inputId = ns("more"),
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link"),
              span(class = "hover-text-content-side", "Key Takeaways")
            ),
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("about"),
                "",
                icon = tags$i(class = "fa-light fa-circle-info",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link"),
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
                class = "link-no-underline action-link"),
              span(class = "hover-text-content-side", "Definitions and Notes")
            ),
            br(),

            # toggle side content -----------------------------------------------------
            shinyjs::hidden(
              div(id = ns("toggle_takeaway"),
                  class = "context-content",
                  br(),
                  br(),
                  uiOutput(ns("takeaway"))
              )
            ),
            shinyjs::hidden(
              div(id = ns("toggle_about"),
                  class = "context-content",
                  br(),
                  br(),
                  uiOutput(ns("context_about"))
              )
            ),

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

#' landscape Server Functions
#'
#' @noRd
mod_landscape_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # update inputs based on selections ---------------------------------------
    observeEvent(c(input$topic, input$view, input$filter), {
      if (input$topic %in% c("totpop", "tenure")) {
        new_filter = landscape_filters[-1]
        new_view = c("Bar Graph", "Line Graph", "Table")

      } else if (input$topic == "c0govcatn" & input$filter != "No Filter") {
        new_filter = landscape_filters[-3]
        new_view = c("Bar Graph", "Line Graph", "Table")

      } else if (input$topic == "c0govcatn" & input$filter == "No Filter") {
        new_filter = landscape_filters[-3]
        new_view = c("Map", "Bar Graph", "Line Graph", "Table")

      } else {
        new_filter = landscape_filters
        new_view = c("Bar Graph", "Line Graph", "Table")

      }

      updateSelectInput(session, "filter", choices = new_filter)
      updateSelectInput(session, "view", choices = new_view)
    })


    # filter meta data --------------------------------------------------------
    filter_meta <- reactive({
      meta %>%
        dplyr::filter(field == input$topic)
    })


    # Function to generate data based on dataset and range
    generate_data <- function() {

      if (input$view == "Map") {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$topic))) %>%
          dplyr::distinct(fips, .keep_all = TRUE) %>%
          dplyr::select(fips, state_name, fig_filter)


      } else if (input$topic == "field" & input$filter == "No Filter") {
        data <- master_data %>%
          dplyr::select(tidyselect::starts_with("c4q5")) %>%
          reshape2::melt() %>%
          tidyr::drop_na(value) %>%
          dplyr::mutate(fig_filter = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                                      variable == "c4q502b" ~ "Nursing",
                                                      variable == "c4q502c" ~ "Public Health",
                                                      variable == "c4q502d" ~ "Other")) %>%
          dplyr::group_by(fig_filter) %>%
          dplyr::summarise(fig_value = sum(value)) %>%
          dplyr::mutate(n = round(fig_value/935*100,1)) %>%
          dplyr::mutate(subtitle = "Across All LHDs")


      } else if (input$topic == "field" & input$filter != "No Filter") {
        data <- spec_data %>%
          dplyr::filter(filter == input$filter)

      } else if (input$filter == "No Filter") {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$topic))) %>%
          tidyr::drop_na(fig_filter) %>%
          dplyr::group_by(fig_filter) %>%
          dplyr::summarise(value = dplyr::n()) %>%
          dplyr::mutate(n = round(value/sum(value)*100,1)) %>%
          dplyr::mutate(subtitle = "Across All LHDs") %>%
          dplyr::ungroup()

      } else {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(fig_cat = stringr::str_to_title(get(input$topic))) %>%
          tidyr::drop_na(fig_cat) %>%
          dplyr::group_by(fig_filter, fig_cat) %>%
          dplyr::summarise(fig_value = dplyr::n()) %>%
          dplyr::mutate(n = round(fig_value/sum(fig_value)*100,1)) %>%
          dplyr::ungroup()
      }  # close topic class if/else
      return(data)
    }


    # figure output -----------------------------------------------------------
    output$figure <- highcharter::renderHighchart({

      data <- data.frame(
        x = c(0, 1),
        y = c(10.1, 89.9)
      )

      highcharter::hchart(data,
                          "column",
                          highcharter::hcaes(x = x, y = y)) %>%
        highcharter::hc_xAxis(
          categories = c("Urban", "Rural")
        )
    })

    proxy <- highcharter::highchartProxy(ns("figure"))

    observe({
      if (input$topic == "totpop") {
        data <- master_data %>%
          dplyr::mutate(x = stringr::str_to_title(get(input$filter)),
                        fig_value = get(input$topic)) %>%
          dplyr::group_by(x) %>%
          dplyr::summarise(fig_value = sum(fig_value)) %>%
          dplyr::mutate(y = round(fig_value/sum(fig_value)*100,1),
                        x = as.numeric(x),
                        subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup()

        proxy |>
          highcharter::hcpxy_set_data(
            "column",
            data = data
          ) |>
          highcharter::hcpxy_redraw()

      } else
        if (input$topic == "tenure") {
        data <- master_data %>%
          dplyr::mutate(x = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(fig_value = get(input$topic)) %>%
          dplyr::group_by(x) %>%
          dplyr::summarise(y = mean(fig_value, na.rm = TRUE)) %>%
          dplyr::mutate(x = as.numeric(x)) %>%
          dplyr::mutate(subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup()

        proxy |>
          highcharter::hcpxy_set_data(
            "column",
            data = data
          ) |>
          highcharter::hcpxy_redraw()
      }
    })



    #   } else if (input$view == "Bar Graph" & input$filter != "No Filter") {
    #     naccho.viz::create_grouped_bar(
    #       data = filter_data(),
    #       x_variable = "fig_cat",
    #       y_variable = "n",
    #       x_group = "fig_filter",
    #       title_text = filter_meta()$title,
    #       subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
    #       caption_text = filter_meta()$caption,
    #       y_label = filter_meta()$y_label,
    #       y_max = filter_meta()$y_max,
    #       y_format = filter_meta()$y_format,
    #       my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_pink, naccho.viz::naccho_teal, naccho.viz::naccho_blue, naccho.viz::naccho_green),
    #       legend_enable = TRUE,
    #       legend_title = "",
    #       tooltip = filter_meta()$tooltip,
    #       allow_export = TRUE,
    #       select_theme = naccho.viz::naccho_minimal_theme
    #     ) %>%
    #       naccho.viz::add_logo(
    #         "https://toolbox.naccho.org/pages/img/logo-header.png",
    #         width = 150L,
    #         height = 40L
    #       )
    #
    #   } else if (input$view == "Pie Chart") {
    #     naccho.viz::create_pie(
    #       data = filter_data(),
    #       variable = "fig_filter",
    #       value = "n",
    #       my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_pink, naccho.viz::naccho_teal, naccho.viz::naccho_blue, naccho.viz::naccho_green),
    #       legend_enable = TRUE,
    #       title_text = filter_meta()$title,
    #       subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
    #       caption_text = filter_meta()$caption
    #     ) %>%
    #       naccho.viz::add_logo(
    #         "https://toolbox.naccho.org/pages/img/logo-header.png",
    #         width = 150L,
    #         height = 40L
    #       )
    #
    #   } else if (input$view == "Map") {
    #     naccho.viz::create_cat_map(
    #       data = filter_data(),
    #       fips_column = "fips",
    #       selected_value = "fig_filter",
    #       title_text = filter_meta()$title,
    #       subtitle_text = "",
    #       caption_text = filter_meta()$caption,
    #       accessible_desc = ""
    #     )
    #   }
    # })



    # context and definitions -------------------------------------------------
    output$about <- renderText({
    })

    output$definitions <- renderText({
    })

    observeEvent(input$about, {
      shinyjs::show("toggle_about")
      shinyjs::hide("toggle_takeaway")
      output$context_about <- renderUI({
        context <- context %>%
          dplyr::filter(field == input$topic)
        p(strong("Survey Question:"))
        p(htmltools::HTML(
          "<b>Survey Question</b>: <br>",
          context$survey_question,
          "<br> <br> <b> Survey Response Options: </b>",
          context$response_options))
      })
    })

    observeEvent(input$more, {
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_about")
      output$takeaway <- renderUI({
        context <- context %>%
          dplyr::filter(field == input$topic)
        p(htmltools::HTML(context$takeaway))
      })
    })

    # keep so the correct action links are selection upon shiny connection
    session$sendCustomMessage(type = 'setNamespace', message = 'landscape_1')
  })
}

## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
