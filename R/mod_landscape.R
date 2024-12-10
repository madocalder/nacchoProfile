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
              class = "col-sm-6 min-height-400",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = landscape_topics,
                selectize = TRUE,
                selected = landscape_topics[2]
              )
            ),
            div(
              class = "col-sm-6",
              br(),
              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters
              )
            )
          ),

          hr(),

          # viz toggles here --------------------------------------------------
          div(
            class = "viz-view",
            shinyWidgets::radioGroupButtons(
              ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-simple"></i>'),  # Chart icon
                HTML('<i class="fas fa-chart-line"></i>'), # Line graph icon
                HTML('<i class="fas fa-table"></i>')         # Map icon
              ),
              choiceValues = c('Bar Graph', 'Line Graph', 'Table'),
              selected = 'Bar Graph'
            )
          ),


          # add main page content here ----------------------------------------
          div(
            class = "viz-content",

            # display when Map is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Map\'"),
              div(
                highcharter::highchartOutput(ns("map"), height = "100%"),
                style = "height: 60vh; width: auto;"
              )
            ),

            # display when Bar is selected and not Map, Table, or Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), '\'] != \'Map\' && input[\'', ns('view'), '\'] != \'Table\' && input[\'', ns('view'), "\'] != \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("figure"), height = "100%"),
                style = "height: 58vh; width: auto;"
              )
            ),

            # display when Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              div(
                uiOutput(ns("table_title")),
                reactable::reactableOutput(ns("data_table"), height = "auto", width = "auto"),
                br(),
                HTML('<div style="font-size: 1.2em;">', source_line,'
         <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),
                style = "margin-left: auto !important;
                        margin-right: auto !important;"
              )
            ),

            # view output Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("line"), height = "60vh"),
                br(),
                # HTML('<div style="font-size: 2vmin;">', source_line,'</div>'),
                style = "height: 60vh; width: auto;"
              )
            )
          )
        ),

          # end of main content -----------------------------------------------------

        right = div(
            div(
              class = "row color-block-header",
              div(
                class = "col-sm-4 hover-text-side",
                align = "center",
                br(),
                actionLink(
                  inputId = ns("takeaway"), # key takeaway
                  "",
                  icon = tags$i(class = "fa-light fa-key",
                                style = "font-size: 4vmin; text-align: center;"),
                  class = "link-no-underline action-link"),
                span(class = "hover-text-content-side", "Takeaways")
              ),
              div(
                class = "col-sm-4 hover-text-side",
                align = "center",
                br(),
                actionLink(
                  inputId = ns("context"), # survey questions and responses
                  "",
                  icon = tags$i(class = "fa-light fa-circle-info",
                                style = "font-size: 4vmin; text-align: center;"),
                  class = "link-no-underline action-link"),
                span(class = "hover-text-content-side", "Notes")
              ),
              div(
                class = "col-sm-4 hover-text-side",
                align = "center",
                br(),
                actionLink(
                  inputId = ns("definitions"), # related definitions
                  "",
                  icon = tags$i(class = "fa-light fa-list",
                                style = "font-size: 4vmin; text-align: center;"),
                  class = "link-no-underline action-link"),
                span(class = "hover-text-content-side", "Definitions")
              ),
              hr(class = "right-hr")
            ),


          div(
            class = "row color-block-content",

            # toggle side content -----------------------------------------------------
            shinyjs::hidden(
              div(id = ns("toggle_takeaway"),
                  class = "context-content",
                  uiOutput(ns("show_takeaway"))
              )
            ),
            shinyjs::hidden(
              div(id = ns("toggle_context"),
                  class = "context-content",
                  uiOutput(ns("show_context"))
              )
            ),
            shinyjs::hidden(
              div(id = ns("toggle_definitions"),
                  class = "context-content",
                  uiOutput(ns("show_definitions"))
              )
            )
          ),
          div(
            class = "row color-block-footer"
          )
        ),

        right_button = right_button,
        left_width = 25,
        right_width = 25,
        center_width = 50
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
      if (input$topic %in% c("c2q501")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"))


      } else {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Line Graph", "Table"))
      }
    })

    # observe({
    #   req(input$topic, input$filter, input$view)
    #
    #   # If the selected view is "Line Graph", ensure the topic is valid
    #   if (input$view == "Line Graph" && input$topic == "c2q501") {
    #     # If the combination is invalid, reset the view to "Bar Graph" or another valid view
    #     shinyWidgets::updateRadioGroupButtons(session, "view", selected = "Bar Graph")
    #
    #     # Optionally, show a message to the user about invalid combination
    #     showNotification("This topic cannot display a Line Graph.", type = "error")
    #   }
    #
    # })

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      meta %>%
        dplyr::filter(field == input$topic,
                      stringr::str_detect(view, input$view),
                      stringr::str_detect(filter, input$filter))


    })







    #table titles-----------------
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      req(input$topic)

      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(filter_meta()$title)

      # Render the HTML
      p(title_values, style = "font-size: 2em; font-weight: bold !important; text-align: center;")
    })

    # Dynamic color definitions ---------------------------------------------------
    colors <- reactive({
      if (input$filter == "urbanrural2022") {
        c(naccho.viz::navy, naccho.viz::teal)
      } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
        c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta)
      } else if (input$filter == "censusregion") {
        c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta, naccho.viz::pink)
      } else {
        c(naccho.viz::navy)
      }
    })



    #Tables output ------
    output$data_table <- reactable::renderReactable({
      req(input$view == "Table")

      #add topic value for function
      topic = input$topic

      filtering = input$filter

      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL  # If input$classifications does not exist, set it to NULL
      }

      # #debug
      # message("input$topic =")
      # print(input$topic)
      # message("topic variable =")
      # print(topic)
      #
      # #debug
      # message("input$filter =")
      # print(input$filter)
      # message("filtering variable =")
      # print(filtering)



      data <- process_table_data(topic, filtering, classifications)

      data <- data %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
        )


      # Define colors reactively based on the filter
      colors <- colors()


      # Determine the category based on the input filter
      category <- case_when(
        input$filter == "urbanrural2022" ~ "Rurality",
        input$filter == "c0govcat" ~ "Governance",
        input$filter == "c0popcat3" ~ "LHD size",
        input$filter == "censusregion" ~ "Region",
        input$filter == "All LHDs" ~ "All LHDs"
      )

      # print(data)

      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic)

    })



    # Bar figure output -----------------------------------------------------------
    # Bar Graph output with proxy
    output$figure <- highcharter::renderHighchart({
      req(input$view == "Bar Graph", input$topic, input$filter)

      # Create an empty bar chart for setup
      bar_chart <- highcharter::highchart()

      # Combine data for "All LHDs" and filtered LHDs
      combined_data <- if (input$filter == "All LHDs") {
        if (input$topic == "field") {
          spec_data_all
        } else {
          return_survey_weighted_counts(input$topic)
        }
      } else {
        all_data <- if (input$topic == "field") {
          spec_data_all
        } else {
          return_survey_weighted_counts(input$topic)
        }

        filtered_data <- if (input$topic == "field") {
          spec_data %>% dplyr::filter(filter == input$filter)
        } else {
          return_survey_weighted_counts(input$topic, input$filter)
        }

        dplyr::bind_rows(
          all_data %>% dplyr::mutate(filter = "All LHDs"),
          filtered_data
        )
      }

      # Generate the bar chart (no series yet)
      bar_chart <- set_column(combined_data, y_format = filter_meta()$y_format) %>%
        viz_options(
          viz_title = filter_meta()$title,
          viz_caption = paste(
            "<b> N: </b>", sum(unique(combined_data$Responses), na.rm = TRUE),
            "<br>", source_line
          ),
          tooltip = filter_meta()$tooltip
        )

      bar_chart
    })

    # Proxy for the Bar Chart
    proxy_bar <- highcharter::highchartProxy(ns("figure"))

    # Update the bar chart with new data when inputs change
    observe({
      req(input$topic, input$filter)  # Ensure inputs are available

      # Remove all old series (to prevent overlap)
      proxy_bar %>%
        highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series")

      # Combine the updated data based on input$topic and input$filter
      combined_data <- if (input$filter == "All LHDs") {
        if (input$topic == "field") {
          spec_data_all
        } else {
          return_survey_weighted_counts(input$topic)
        }
      } else {
        all_data <- if (input$topic == "field") {
          spec_data_all
        } else {
          return_survey_weighted_counts(input$topic)
        }

        filtered_data <- if (input$topic == "field") {
          spec_data %>% dplyr::filter(filter == input$filter)
        } else {
          return_survey_weighted_counts(input$topic, input$filter)
        }

        dplyr::bind_rows(
          all_data %>% dplyr::mutate(filter = "All LHDs"),
          filtered_data
        )
      }

      # Dynamically update the bar chart with new data
      proxy_bar %>%
        highcharter::hcpxy_add_series(
          id = "filtered_bar_series",
          name = input$filter,
          data = combined_data,
          type = "column",  # Ensure this is a column chart (bar graph)
          selected = TRUE
        )
    })


    # Line Output-------------------
    # Line Graph output with proxy
    output$line <- highcharter::renderHighchart({
      req(input$view == "Line Graph", input$topic != "c2q501", input$filter)

      # Create an empty line chart for setup
      line_chart <- highcharter::highchart()

      # Process data for the line graph
      data <- process_line_data(input$topic, input$filter)

      # If data is empty, return an empty chart
      if (nrow(data) > 0) {
        line_chart <- naccho.viz::create_grouped_line(
          data = data,
          x_variable = "year",
          x_group = "legend_label",
          y_variable = "y",
          y_label = filter_meta()$y_label,
          y_max = max(data$y, na.rm = TRUE) * 1.1,
          x_label = "Year",
          select_theme = naccho.viz::basic,
          title = filter_meta()$title,
          tooltip = filter_meta()$tooltip
        )
      } else {
        line_chart <- highcharter::highchart()  # Empty chart for no data
      }

      line_chart
    })

    # Proxy for the Line Chart
    proxy_line <- highcharter::highchartProxy(ns("line"))

    # Update the line chart with new data when inputs change
    observe({
      req(input$topic, input$filter)  # Ensure inputs are available

      # Remove old series to prevent overlap
      proxy_line %>%
        highcharter::hcpxy_remove_series(id = "filtered_line_series")

      # Process and generate the updated data for the line graph
      data <- process_line_data(input$topic, input$filter)

      # Dynamically update the line chart with new data
      proxy_line %>%
        highcharter::hcpxy_add_series(
          id = "filtered_line_series",
          name = input$filter,
          data = data,
          type = "line",  # Ensure this is a line chart
          selected = TRUE
        )
    })


    # # Update bar graph and line graph when topic or filter changes
    # observe({
    #   req(input$topic, input$filter)  # Ensure inputs are available
    #
    #   # Update the bar graph
    #   if (input$view == "Bar Graph") {
    #     output$figure <- highcharter::renderHighchart({
    #       req(input$topic, input$filter)
    #
    #       # Combine data for "All LHDs" and filtered LHDs
    #       combined_data <- if (input$filter == "All LHDs") {
    #         if (input$topic == "field") {
    #           spec_data_all
    #         } else {
    #           return_survey_weighted_counts(input$topic)
    #         }
    #       } else {
    #         all_data <- if (input$topic == "field") {
    #           spec_data_all
    #         } else {
    #           return_survey_weighted_counts(input$topic)
    #         }
    #
    #         filtered_data <- if (input$topic == "field") {
    #           spec_data %>% dplyr::filter(filter == input$filter)
    #         } else {
    #           return_survey_weighted_counts(input$topic, input$filter)
    #         }
    #
    #         dplyr::bind_rows(
    #           all_data %>% dplyr::mutate(filter = "All LHDs"),
    #           filtered_data
    #         )
    #       }
    #
    #
    #       bar_chart <- set_column(combined_data, y_format = filter_meta()$y_format) %>%
    #         viz_options(
    #           viz_title = filter_meta()$title,
    #           viz_caption = paste("<b> N: </b>", sum(unique(combined_data$Responses), na.rm = TRUE), "<br>", source_line),
    #           tooltip = filter_meta()$tooltip
    #         )
    #
    #       bar_chart
    #     })
    #   }
    #
    #   # Update the line graph
    #   if (input$view == "Line Graph") {
    #     output$line <- highcharter::renderHighchart({
    #       req(input$topic, input$filter)
    #
    #       # Process and generate line graph data
    #       data <- process_line_data(input$topic, input$filter)
    #
    #       # Make sure there is data before rendering
    #       if (nrow(data) > 0) {
    #         naccho.viz::create_grouped_line(
    #           data = data,
    #           x_variable = "year",
    #           x_group = "legend_label",
    #           y_variable = "y",
    #           y_label = filter_meta()$y_label,
    #           y_max = max(data$y, na.rm = TRUE) * 1.1,
    #           x_label = "Year",
    #           select_theme = naccho.viz::basic,
    #           title = filter_meta()$title,
    #           tooltip = filter_meta()$tooltip
    #         )
    #       } else {
    #         return(NULL)  # Return nothing if there's no data for the line graph
    #       }
    #     })
    #   }
    # })
    #
    #


    # right content ===========================================================

    # takeaway ----------------------------------------------------------------
    # takeaway ----------------------------------------------------------------
    observeEvent(input$takeaway, {

      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")



      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view, # fix
                        filter == input$filter)

        p(htmltools::HTML(
          "<b>Key Takeaway(s)</b>: <br><br>",
          notes$takeaway))
      })
    }, ignoreNULL = FALSE)


    # survey questions, responses, and notes ----------------------------------
    observeEvent(input$context, {

      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view, # fix
                        filter == input$filter)

        if (is.na(notes$notes)) {
          p(htmltools::HTML(
            "<b>Survey Question(s)</b>: <br>",
            notes$survey_question,
            "<br> <br> <b> Survey Response Options: </b>",
            notes$survey_response))

        } else {
          p(htmltools::HTML(
            "<b>Survey Question(s)</b>: <br>",
            notes$survey_question,
            "<br> <br> <b> Survey Response Options: </b>",
            notes$survey_response,
            "<br> <br> <b> Notes: </b>",
            notes$notes
))
        }
      })
    })


    # definitions -------------------------------------------------------------
    observeEvent(input$definitions, {

      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::show("toggle_definitions")

      output$show_definitions <- renderUI({

        definitions <- definitions %>%
          dplyr::filter(field %in% c(input$topic, input$filter))

        links <- lapply(seq_along(definitions$item), function(i) {
          div(
            tags$a(
              class = "def-link",
              `data-bs-toggle` = "collapse",
              href = paste0("#collapseExample", i),
              tags$i(class = "fa fa-plus"),
              paste(definitions$item[[i]])
            ),
            div(
              class= "collapse",
              id= paste0("collapseExample", i),
              HTML(definitions$definition[[i]], "<br> <br>")
            )
          )
        })
        div(
          p(htmltools::HTML("<b>Definitions</b>: <br>")),

# side right --------------------------------------------------------------


          if (length(links) == 0) {
            paste("No relevant definitions.")
          } else {
            links
          }
        )
      })
    })
  })
}
## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
