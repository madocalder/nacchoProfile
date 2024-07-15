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
                )
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters[-1]
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
    observe({
      if (input$topic == "c0govcatn" & input$filter != "No Filter") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[-3])
        updateSelectInput(session, "view",
                          choices = c("Bar Graph", "Line Graph", "Table"))
      }
    })
    observe({
      if (input$topic == "c0govcatn" & input$filter == "No Filter") {
        updateSelectInput(session, "view",
                          choices = c("Map", "Bar Graph", "Line Graph", "Table"))
      }
    })

    # filter meta data --------------------------------------------------------
    filter_meta <- reactive({
      meta %>%
        dplyr::filter(field == input$topic)
    })


    # Function to generate data based on dataset and range
    generate_data <- function() {

      # if (input$view == "Map")
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(fig_filter = stringr::str_to_title(get(input$topic))) %>%
          dplyr::distinct(fips, .keep_all = TRUE) %>%
          dplyr::select(fips, state_name, fig_filter)

      return(data)
    }


    # figure output -----------------------------------------------------------
    output$figure <- highcharter::renderHighchart({

      data <- data.frame(
        x = c(0, 1),
        y = c(10.1, 89.9),
        category = c("Rural", "Urban"),
        group = "All LHDs"
      )

      highcharter::hchart(
        data,
        "column",
        id = "All",
        name = "All LHDs",
        highcharter::hcaes(
          x = x,
          y = y,
          group = group
        ),
        colorByPoint = FALSE
      ) %>%
        highcharter::hc_xAxis(
          title = list(text = ""), # x-axis label
          categories = data$category
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "% of Population"), # y-axis label
          min = 0
        ) %>%
        highcharter::hc_colors(
          colors = naccho.viz::naccho_colors
        ) %>%
        highcharter::hc_add_theme(
          naccho.viz::basic_theme
        ) %>%
        highcharter::hc_exporting(
          enabled = TRUE,
          accessibility = list(
            enabled = TRUE
          ),
          formAttributes = list(
            target = "_blank" # opens in new window
          ),
          sourceHeight = 700, # sets height of download
          sourceWidth = 1200, # sets width of download
          allowHTML = TRUE,
          buttons = list(
            contextButton = list(
              symbol = "menu",
              symbolStrokeWidth = 2,
              symbolFill = naccho.viz::naccho_teal,
              symbolStroke = naccho.viz::naccho_teal,
              menuItems = c("printChart",
                            "separator",
                            "downloadPNG",
                            "downloadJPEG",
                            "downloadPDF",
                            "downloadSVG",
                            "separator",
                            "downloadCSV",
                            "downloadXLS",
                            "openInCloud")
            )
          )
        )
    })

    proxy <- highcharter::highchartProxy(ns("figure"))
    series_id <- reactiveVal(NULL)


    # this observe is needed to redraw the figure based on the input selected
    # by the user. Requirements are determined by how the data need to be
    # wrangled and filtered for a specific selection.
    observeEvent(input$topic, {
      if (input$topic == "totpop" & input$filter != "No Filter") {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(category = stringr::str_to_title(get(input$filter)),
                        fig_value = get(input$topic)) %>%
          dplyr::group_by(category) %>%
          dplyr::summarise(fig_value = sum(fig_value)) %>%
          dplyr::mutate(y = round(fig_value/sum(fig_value)*100,1),
                        x = as.numeric(as.factor(category)) -1, # -1 is required so the numbers start at 0 for the proxy to correctly draw
                        subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup()

        print(series_id)


        proxy %>%
          highcharter::hcpxy_remove_series(
            id = series_id()
          ) %>%
          highcharter::hcpxy_set_data(
            "column",
            data = data
          ) %>%
          highcharter::hcpxy_update(
            yAxis = list(
              title = list(
                text = "% of Population"
              )
            ),
            xAxis = list(
              categories = data$category
            )
          ) %>%
          highcharter::hcpxy_redraw()

      } else if (input$topic == "tenure" & input$filter != "No Filter") {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(category = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(fig_value = get(input$topic)) %>%
          dplyr::group_by(category) %>%
          dplyr::summarise(y = mean(fig_value, na.rm = TRUE)) %>%
          dplyr::mutate(x = as.numeric(as.factor(category)) -1) %>%
          dplyr::mutate(subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup()

        print(series_id)

        proxy %>%
          highcharter::hcpxy_remove_series(
            id = series_id()
          ) %>%
          highcharter::hcpxy_set_data(
            "column",
            data = data
          ) %>%
          highcharter::hcpxy_update(
            yAxis = list(
              title = list(
                text = "% of Population"
              )
            ),
            xAxis = list(
              categories = data$category
            )
          ) %>%
          highcharter::hcpxy_redraw()

      } else if (input$topic == "field" & input$filter == "No Filter") {
        req(input$topic %not_in% c("totpop", "tenure"))
        data <- master_data %>%
          dplyr::select(tidyselect::starts_with("c4q5")) %>%
          dplyr::mutate_if(is.double, as.numeric) %>% # this helps prevent a warning
          reshape2::melt(id.vars = NULL) %>%
          tidyr::drop_na(value) %>%
          dplyr::mutate(category = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                                      variable == "c4q502b" ~ "Nursing",
                                                      variable == "c4q502c" ~ "Public Health",
                                                      variable == "c4q502d" ~ "Other")) %>%
          dplyr::group_by(category) %>%
          dplyr::summarise(fig_value = sum(value)) %>%
          dplyr::mutate(y = round(fig_value/935*100,1),
                        subtitle = "Across All LHDs",
                        x = as.numeric(as.factor(category)) -1)

        print(series_id)

        proxy %>%
          highcharter::hcpxy_remove_series(
            id = series_id()
          ) %>%
          highcharter::hcpxy_set_data(
            "column",
            data = data
          ) %>%
          highcharter::hcpxy_update(
            yAxis = list(
              title = list(
                text = "% of Population"
              )
            ),
            xAxis = list(
              categories = data$category
            )
          ) %>%
          highcharter::hcpxy_redraw()


      } else if (input$topic == "field" & input$filter != "No Filter") {

        data <- spec_data %>%
          dplyr::filter(filter == input$filter) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(x = as.numeric(as.factor(category)) -1,
                        group = stringr::str_to_title(group))

        series_id(unique(data$group))

        proxy %>%
          highcharter::hcpxy_remove_series(
            id = series_id()
          ) %>%
          highcharter::hcpxy_add_series(
            data = data,
            "column",
            highcharter::hcaes(x, y, group = group),
            id = series_id()
          ) %>%
          highcharter::hcpxy_update(
            yAxis = list(
              title = list(
                text = "% of LHD Directors*"
              )
            )
          ) %>%
          highcharter::hcpxy_redraw()

      } else if (input$filter == "No Filter") {
        req(input$topic %not_in% c("totpop", "tenure"))
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(category = stringr::str_to_title(get(input$topic))) %>%
          tidyr::drop_na(category) %>%
          dplyr::group_by(category) %>%
          dplyr::summarise(value = dplyr::n()) %>%
          dplyr::mutate(y = round(value/sum(value)*100,1),
                        subtitle = "Across All LHDs",
                        x = as.numeric(as.factor(category)) -1) %>%
          dplyr::ungroup()

        proxy %>%
          highcharter::hcpxy_remove_series(
            id = series_id()
          ) %>%
          highcharter::hcpxy_set_data(
            "column",
            data = data
          ) %>%
          highcharter::hcpxy_update(
            yAxis = list(
              title = list(
                text = "% of Population"
              )
            ),
            xAxis = list(
              categories = data$category
            )
          ) %>%
          highcharter::hcpxy_redraw()


      } else {

        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(group = stringr::str_to_title(get(input$filter))) %>%
          dplyr::mutate(category = stringr::str_to_title(get(input$topic))) %>%
          tidyr::drop_na(category) %>%
          dplyr::group_by(group, category) %>%
          dplyr::summarise(fig_value = dplyr::n()) %>%
          dplyr::mutate(y = round(fig_value/sum(fig_value)*100,1),
                        x = as.numeric(as.factor(category)) -1) %>%
          dplyr::ungroup()

        proxy %>%
          highcharter::hcpxy_add_series(
            data = data,
            "column",
            highcharter::hcaes(x, y, group = group),
            id = c("urban", "rural")
          ) %>%
          highcharter::hcpxy_update(
            yAxis = list(
              title = list(
                text = "% of LHD Directors*"
              )
            )
          ) %>%
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
