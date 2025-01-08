#' services UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_services_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'services_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          class = "main-content",
          openMenu(),

            div(
              class = "topic-select",

              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = service_topics,
                selectize = TRUE
              )
            ),
            div(
              class = "filter-select",

              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters,
                selectize = TRUE
              )
            ),


          # viz toggles here --------------------------------------------------
          div(
            class = "viz-view",
            shinyWidgets::radioGroupButtons(
              ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-simple"></i>'),
                HTML('<i class="fas fa-table"></i>')
              ),
              choiceValues = c("Bar Graph", "Table")
            )
          ),


          # main page content ----------------------------------------------
          div(
            class = "viz-content",

            # display when Service Provision and Bar Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] != 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Bar Graph'"),
              # div(
                highcharter::highchartOutput(ns("figure_bar"), height = "100%")

              #   , #675
              # style = "height: 60vh; width: auto; margin-left: auto !important;
              #           margin-right: auto !important;")
            ),


            # # display when Service Provision and Line Graph are selected
            # conditionalPanel(
            #   condition = paste0('input[\'', ns('topic'), "\'] != 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Line Graph'"),
            #   # highcharter::highchartOutput(ns("figure_line"), height = 575)
            # ),

            # display when Service Changes and Scatter Plot are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Scatter Plot'"),
              # div(
              highcharter::highchartOutput(ns("figure_scatter"), height = "100%")
              # , #775
              # style = "height: 70vh; width: auto;")
            ),

            # display when Service Changes and Butterfly Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Butterfly Graph'"),
              # div(
                highcharter::highchartOutput(ns("figure_butterfly"), height = "100%")

                # , #675
                # style = "height: 70vh; margin-left: auto !important; margin-right: auto !important;")
            ),

            # display when view Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              div(
                uiOutput(ns("table_title")),
              reactable::reactableOutput(ns("data_table"), height = "auto", width = "auto"),
              br(),
              HTML('<div style="font-size: 1.2em;">', source_line,'
                   <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),

            style = "min-width: 50vw; min-height: 45vh; margin-left: auto !important;
                        margin-right: auto !important;"
            ),

            # br(),
)
)

        ),

     # end of main content -----------------------------------------------------
# right pane begins--------
right = div(
  div(
    class = "row color-block-header",
    id = ns("dynamic_row"),
    # Define the default state with all panels hidden
    div(
      class = "col hover-text-side",
      align = "center",
      actionLink(
        ns("get_services"),
        "",
        icon = tags$i(class = "fa-light fa-filter", style = "font-size: calc(30px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Add Services")
    ),
    div(
      class = "col hover-text-side",
      align = "center",
      actionLink(
        ns("takeaway"),
        "",
        icon = tags$i(class = "fa-light fa-key", style = "font-size: calc(30px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Takeaways")
    ),
    div(
      class = "col hover-text-side",
      align = "center",
      actionLink(
        ns("context"),
        "",
        icon = tags$i(class = "fa-light fa-circle-info", style = "font-size: calc(30px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Notes")
    ),
    div(
      class = "col hover-text-side",
      align = "center",
      actionLink(
        ns("definitions"),
        "",
        icon = tags$i(class = "fa-light fa-list", style = "font-size: calc(30px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Definitions")
    ),
    hr(class = "right-hr")
  ),

  # Add placeholders for the toggled content
  div(
    class = "row color-block-content",
    # Right panel setup in the UI
    # Services panel (filter) visible by default
    div(
      id = ns("toggle_get_services"),
      class = "context-content",
      p(htmltools::HTML("<b>Select Service(s):</b>")),
      uiOutput(ns("services_input"))
    ),
    shinyjs::hidden(
      div(id = ns("toggle_takeaway"), class = "context-content", uiOutput(ns("show_takeaway")))
    ),
    shinyjs::hidden(
      div(id = ns("toggle_context"), class = "context-content", uiOutput(ns("show_context")))
    ),
    shinyjs::hidden(
      div(id = ns("toggle_definitions"), class = "context-content", uiOutput(ns("show_definitions")))
    )
  ),
  div(class = "row color-block-footer")
),

        right_button = right_button,
        left_width = 25,
        right_width = 25,
        center_width = 50
      )
    )
  )
}



#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Input Updates ==========================================================
    observe({
      req(input$topic)
      if (input$topic != "Reduced/Expanded Services") {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-simple"></i>'),
            HTML('<i class="fas fa-table"></i>')
          ),
          choiceValues = c("Bar Graph", "Table")
        )
      } else {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-scatter"></i>'),
            HTML('<i class="fas fa-chart-bar"></i>'),
            HTML('<i class="fas fa-table"></i>')
          ),
          choiceValues = c("Scatter Plot", "Butterfly Graph", "Table")
        )
      }
    })

    observe({
      req(input$topic, input$view)
      if (input$topic == "Reduced/Expanded Services" && input$view == "Butterfly Graph") {
        updateSelectInput(
          session, "filter",
          choices = landscape_filters[1]
        )
      }
    })

    # Dynamically switch between single-select and multi-select
    observe({
      req(input$topic)

      if (input$topic == "Reduced/Expanded Services") {
        # Multi-select using checkboxes
        output$services_input <- renderUI({
          checkboxGroupInput(
            inputId = ns("services"),
            label = "",
            choices = service_cats,  # List of services
            selected = service_cats[1]
          )
        })
      } else {
        # Single-select using radio buttons
        output$services_input <- renderUI({
          radioButtons(
            inputId = ns("services"),
            label = "",
            choices = service_classifications,  # List of services
            selected = service_classifications[1]  # Default first service
          )
        })
      }
    })





    observe({
      updateSelectInput(session, "view", selected = "Table")
      updateSelectInput(session, "filter", selected = landscape_filters[1])
      updateSelectInput(session, "services", selected = service_classifications[1])
      updateSelectInput(session, "topic", selected = service_topics[[1]][1])
    })

    # Reactive Color Definitions =============================================
    colors <- reactive({
      if (input$filter == "urbanrural2022") {
        c(naccho.viz::navy, naccho.viz::teal)
      } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
        c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta)
      } else if (input$filter == "censusregion") {
        c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta, naccho.viz::pink)
      } else {
        naccho.viz::green
      }
    })

    # Debounced Inputs =======================================================
    updated_inputs <- reactive({
      input$topic
      input$filter
      input$services
    }) %>% debounce(100)

    # service_change_inputs <- reactive({
    #   input$topic
    #   input$filter
    #   input$services
    # }) %>% debounce(100)


    # Metadata Filtering =====================================================
    filter_meta <- reactive({
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      meta %>%
        dplyr::filter(field == input$topic,
                      stringr::str_detect(view, paste0("'", input$view, "'")),
                      stringr::str_detect(filter, paste0("'", input$filter, "'")))
    })

    # Outputs ================================================================

    output$table_title <- renderUI({
      req(updated_inputs())
      req(input$topic)
      title_values <- as.character(filter_meta()$title)
      p(title_values, style = "font-size: 2em; font-weight: bold !important; text-align: center;")
    })


    # Data Table Output
    output$data_table <- reactable::renderReactable({
      req(updated_inputs())
      req(input$view == "Table", input$topic, input$filter) # Ensure the table is only rendered for the "Table" view

      # Define topic and filtering based on input
      topic <- input$topic
      filtering <- input$filter

      # Determine classifications based on service changes or services inputs
      classifications <- input$services

      # Debug messages (optional for troubleshooting)
      # message("input$topic =", input$topic)
      # message("topic variable =", topic)
      # message("input$filter =", input$filter)
      # message("filtering variable =", filtering)
      # message("classifications variable =", classifications)

      # Process the data for the table based on the topic, filter, and classifications
      data <- process_table_data(topic, filtering, classifications)
      # %>%
      #   dplyr::mutate(
      #     dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
      #   )

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

      # Generate the table using a custom table creation function
      create_non_time_table(data, category, colors, topic)
    })

    output$figure_bar <- highcharter::renderHighchart({
      req(updated_inputs())
      req(input$topic, input$filter, input$services)



      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      topic <- input$topic
      services <- input$services
      filtering <- input$filter

      data <- services_data %>%
        dplyr::filter(
          service_classification %in% services,
          filter == filtering,
          service_provision == topic,
          Answer == "checked"
        ) %>%
        dplyr::mutate(category = service_type) %>%
        dplyr::group_by(Group) %>%
        dplyr::mutate(x = seq_along(category) - 1) %>%
        dplyr::ungroup()

      data_n <- services_data %>%
        dplyr::filter(
          service_classification %in% services,
          filter == filtering,
          service_provision == topic
        ) %>%
        dplyr::group_by(service_type) %>%
        dplyr::summarize(Responses = sum(Responses, na.rm = TRUE)) %>%
        dplyr::ungroup()

      set_column(data,
                 y_label = filter_meta()$y_label,
                 y_format = filter_meta()$y_format,
                 data_label = filter_meta()$data_label) %>%
        viz_options(
          viz_title = "",
          viz_subtitle = "",
          viz_caption = paste0("<b> N: </b>", show_n(data_n$Responses), "<br>", source_line),
          caption_align = "center",
          colors = colors(),
          theme = naccho.viz::basic,
          tooltip = filter_meta()$tooltip
        )
    })

    output$figure_scatter <- highcharter::renderHighchart({
      req(updated_inputs())
      req(input$topic, input$filter, input$services)

      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      services <- input$services
      filtering <- input$filter


      data <- scatter_data(set_filter = filtering, set_service = services)

      scatter_plot(data, colors = colors())
    })

    output$figure_butterfly <- highcharter::renderHighchart({
      req(updated_inputs())
      req(input$services)


      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      services <- input$services

      data <- scatter_data(set_filter = landscape_filters,
                           set_service = services) %>%
        dplyr::mutate(
          left = x,
          right = y,
          x = seq_along(unique(group)) - 1
        ) %>%
        dplyr::mutate(group = ifelse(group == 13, "All LHDs", group))

      data_n <- data %>%
        dplyr::filter(filter == "All LHDs")

      max_one <- max(data$left)
      max_two <- max(data$right)

      set_bar(data,
              y_label = "",
              max_one = max_one,
              max_two = max_two,
              name_one = "Shrunk Service",
              name_two = "Grew Service",
              y_format = "{value}%") %>%
        viz_options(
          colors = c(naccho.viz::terracotta, naccho.viz::green),
          legend_enable = TRUE,
          legend_title = "% of LHDs that...",
          viz_title = "Percent of LHDs that Shrunk or Grew Selected Service",
          viz_caption = paste0("<b> N: </b>", show_n(data_n$Responses), "<br>", source_line),
          viz_subtitle = paste(unique(data$service_type)),
          tooltip = "<b> {series.name}: </b>{point.y:.0f}% <br>",
          theme = naccho.viz::butterfly
        )
    })

    # Event Handling =========================================================

    # Show Filter Services
    observeEvent(input$get_services, {
      shinyjs::show("toggle_get_services")  # Show the Filter Services UI
      shinyjs::hide("toggle_takeaway")      # Hide Takeaways
      shinyjs::hide("toggle_context")       # Hide Notes
      shinyjs::hide("toggle_definitions")   # Hide Definitions

    })


    #takeaway ----
    observeEvent(input$takeaway, {
      shinyjs::hide("toggle_get_services")
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_takeaway <- renderUI({
        req(input$topic, input$filter)  # Ensure inputs are not NULL
        req(notes)  # Ensure `notes` is not NULL

        # Filter notes for key takeaways
        filtered_notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        filter == input$filter)

        if (nrow(filtered_notes) == 0) {
          p("No key takeaways available for the selected filters.")
        } else {
          # Check if takeaway is available
          if (is.na(filtered_notes$takeaway)) {
            p("No specific takeaways for this selection.")
          } else {
            p(htmltools::HTML(
              "<b>Key Takeaway(s)</b>: <br><br>",
              filtered_notes$takeaway
            ))
          }
        }
      })
    })

    #context ----

    observeEvent(input$context, {
      shinyjs::hide("toggle_get_services")
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        req(input$topic, input$filter)  # Ensure inputs are not NULL
        req(notes)  # Ensure `notes` is not NULL

        filtered_notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        filter == input$filter)

        if (nrow(filtered_notes) == 0) {
          p("No relevant notes available.")
        } else {
          if (is.na(filtered_notes$notes)) {
            p(htmltools::HTML(
              "<b>Survey Question(s)</b>: <br>",
              filtered_notes$survey_question,
              "<br> <br> <b> Survey Response Options: </b>",
              filtered_notes$survey_response
            ))
          } else {
            p(htmltools::HTML(
              "<b>Survey Question(s)</b>: <br>",
              filtered_notes$survey_question,
              "<br> <br> <b> Survey Response Options: </b>",
              filtered_notes$survey_response,
              "<br> <br> <b> Notes: </b>",
              filtered_notes$notes
            ))
          }
        }
      })
    })


    #definitions------

    observeEvent(input$definitions, {
      shinyjs::hide("toggle_get_services")
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
