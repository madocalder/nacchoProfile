#' Workforce UI Function
#'
#' @description A Shiny module for rendering the UI for workforce data visualization and exploration.
#'
#' @param id A character string representing the namespace ID for the module.
#'
#' @return A `tagList` containing the UI components for the module.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom highcharter highchartOutput
#' @importFrom reactable reactableOutput
#' @noRd
mod_workforce_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Main layout
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
            # Topic and Filter selectors
            div(
              class = "col-sm-6 min-height-400",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = workforce_topics,
                selectize = TRUE
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
          # Visualization view toggle buttons
          div(
            class = "viz-view",
            shinyWidgets::radioGroupButtons(
              inputId = ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-simple"></i>'),  # Bar Graph icon
                HTML('<i class="fas fa-chart-line"></i>'),    # Line Graph icon
                HTML('<i class="fas fa-table"></i>')          # Table icon
              ),
              choiceValues = c("Bar Graph", "Line Graph", "Table")
            )
          ),
          # Visualization content area
          div(
            class = "viz-content",
            conditionalPanel(
              condition = paste0("input['", ns('view'), "'] == 'Bar Graph'"),
              highcharter::highchartOutput(ns("figure"), height = "100%")
            ),
            conditionalPanel(
              condition = paste0("input['", ns('view'), "'] == 'Line Graph'"),
              highcharter::highchartOutput(ns("line"), height = "auto")
            ),
            conditionalPanel(
              condition = paste0("input['", ns('view'), "'] == 'Table'"),
              div(
                uiOutput(ns("table_title")),
                reactable::reactableOutput(ns("data_table"), height = "auto"),
                br(),
                HTML('<div style="font-size: 1.8vmin;">', source_line, '<br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>')
              )
            )
          )
        ),
        # Sidebar layout for additional information
        right = div(
          div(
            class = "row color-block-header",
            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("takeaway"),
                "",
                icon = tags$i(class = "fa-light fa-key", style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"
              ),
              span(class = "hover-text-content-side", "Takeaways")
            ),
            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("context"),
                "",
                icon = tags$i(class = "fa-light fa-circle-info", style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"
              ),
              span(class = "hover-text-content-side", "Context")
            ),
            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("definitions"),
                "",
                icon = tags$i(class = "fa-light fa-list", style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"
              ),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),
          # Toggling side content
          div(
            class = "row color-block-content",
            shinyjs::hidden(div(id = ns("toggle_takeaway"), class = "context-content", uiOutput(ns("show_takeaway")))),
            shinyjs::hidden(div(id = ns("toggle_context"), class = "context-content", uiOutput(ns("show_context")))),
            shinyjs::hidden(div(id = ns("toggle_definitions"), class = "context-content", uiOutput(ns("show_definitions"))))
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




#' Workforce Server Function
#'
#' @description A Shiny module server logic for rendering and managing workforce data visualizations.
#'
#' @param id A character string representing the namespace ID for the module.
#'
#' @return None (used for side effects).
#'
#' @importFrom shiny moduleServer observeEvent req
#' @importFrom shinyWidgets updateRadioGroupButtons
#' @importFrom highcharter highchartProxy
#' @importFrom reactable renderReactable
#' @noRd
mod_workforce_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(input$topic)
      topic <- input$topic

      # Define views based on the topic
      if (topic %in% c("fte_numcat", "employee_numcat", "jobs_total", "a", "b")) {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-simple"></i>'),  # Bar Graph icon
            HTML('<i class="fas fa-table"></i>')         # Table icon
          ),
          choiceValues = c("Bar Graph", "Table"),
          selected = "Bar Graph"
        )
      } else if (topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop")) {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-line"></i>'),  # Line Graph icon
            HTML('<i class="fas fa-table"></i>')       # Table icon
          ),
          choiceValues = c("Line Graph", "Table"),
          selected = "Line Graph"
        )
      } else {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-simple"></i>'),  # Bar Graph icon
            HTML('<i class="fas fa-chart-line"></i>'),    # Line Graph icon
            HTML('<i class="fas fa-table"></i>')          # Table icon
          ),
          choiceValues = c("Bar Graph", "Line Graph", "Table"),
          selected = "Bar Graph"
        )
      }
    })

    # Reactive to filter metadata based on the selected topic
    filter_meta <- reactive({
      req(input$topic)
      meta %>% dplyr::filter(field == input$topic)
    })

    # Reactive for colors based on the filter
    colors <- reactive({
      req(input$filter)
      switch(input$filter,
             "urbanrural2022" = c(naccho.viz::navy, naccho.viz::teal),
             "c0govcat" = c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta),
             "censusregion" = c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta, naccho.viz::pink),
             "All LHDs" = c(naccho.viz::navy, naccho.viz::terracotta),
             naccho.viz::basic_palette)
    })

    # Generalized data processing for charts
    process_chart_data <- reactive({
      req(input$topic, input$filter)
      data <- if (input$topic %in% c("a", "b")) {
        classification_data <- process_classification_data(
          topic = input$topic,
          classifications = input$classifications,
          filtering = input$filter,
          q_remap = q_remap
        )
        classification_data
      } else if (input$topic == "jobs_total") {
        filtered_butterfly("jobs_data") %>%
          dplyr::mutate(x_label = NA)
      } else if (input$filter == "All LHDs"){
        return_survey_weighted_counts(input$topic) %>%
          dplyr::mutate(x_label = NA)
      } else {
        return_survey_weighted_counts(input$topic, input$filter) %>%
          dplyr::mutate(x_label = NA)
      }
      data
    })

    # Generalized data processing for tables
    process_table_data <- reactive({
      req(input$topic, input$filter)
      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL
      }

      data <- process_data(input$topic, input$filter, classifications)
      data %>% dplyr::mutate(
        dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
      )
    })

    # Generalized data processing for line charts
    process_line_chart_data <- reactive({

      req(input$topic, input$filter)
      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL
      }

      process_line_data(input$topic, input$filter, classifications)

    })

    #table title output
    output$table_title <- renderUI({
      req(input$topic)  # Ensure `input$topic` exists before proceeding

      # Extract the title associated with the selected topic
      title_values <- as.character(subset(meta, field == input$topic)$title)

      # Render the title as HTML
      p(title_values, style = "font-size: 3vmin; font-weight: bold !important; text-align: center;")
    })


    # Bar Graph Rendering
    output$figure <- highcharter::renderHighchart({
      req(input$view == "Bar Graph")
      data <- process_chart_data()
      if (input$topic == "jobs_total") {
        max_one <- max(data$left, na.rm = TRUE)
        max_two <- max(data$right, na.rm = TRUE)
        set_bar(
          data,
          y_label = "# of Jobs",
          max_one = max_one,
          max_two = max_two,
          name_one = "Added",
          name_two = "Lost",
          set_format = "{value}"
        ) %>%
          viz_options(
            colors = c(naccho.viz::terracotta, naccho.viz::green),
            legend_enable = TRUE,
            legend_title = "# of Jobs...",
            viz_title = "Jobs Lost and Added - Estimated Total",
            viz_caption = "<b> N: </b>902",
            tooltip = "<b> {series.name}: </b>{point.y:.0f} <br>",
            theme = naccho.viz::butterfly
          ) %>%
          viz_export()
      } else {
        set_column(data) %>%
          viz_options(
            viz_title = filter_meta()$title,
            viz_caption = paste("<b> N: </b>", sum(data$Responses), "<br>", source_line),
            tooltip = "<b>{point.category}: </b>{point.y:.1f}%"
          ) %>%
          viz_export()
      }
    })

    # Line Graph Rendering
    output$line <- highcharter::renderHighchart({
      req(input$view == "Line Graph")

      data <- process_line_chart_data() %>%
        dplyr::mutate(x_label = NA)

      naccho.viz::create_grouped_line(
        data = data,
        x_variable = "year",
        x_group = "legend_label",
        y_variable = "y",
        x_label = NULL,
        y_label = filter_meta()$y_label,
        title = paste(filter_meta()$title, "Over Time"),
        tooltip = "In {point.year}, {point.one}{point.two}."
      )
    })

    # Table Rendering
    output$data_table <- reactable::renderReactable({
      req(input$view == "Table")
      data <- process_table_data()
      create_non_time_table(data, input$filter, colors(), input$topic)
    })

    # Sidebar content remains unchanged from the earlier refactor
    observeEvent(input$takeaway, {
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")
      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic) %>%
          pull(takeaway)
        p(HTML("<b>Key Takeaway(s):</b><br><br>", notes))
      })
    })

    observeEvent(input$context, {
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")
      output$show_context <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic)
        p(HTML("<b>Survey Question(s):</b><br>", notes$survey_question, "<br><br>",
               "<b>Survey Response Options:</b>", notes$survey_response))
      })
    })

    observeEvent(input$definitions, {
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::show("toggle_definitions")
      output$show_definitions <- renderUI({
        definitions <- definitions %>% dplyr::filter(field == input$topic)
        if (nrow(definitions) > 0) {
          lapply(seq_len(nrow(definitions)), function(i) {
            div(
              tags$a(class = "def-link", href = paste0("#collapseExample", i),
                     tags$i(class = "fa fa-plus"), definitions$item[i]),
              div(class = "collapse", id = paste0("collapseExample", i), HTML(definitions$definition[i]))
            )
          })
        } else {
          p("No relevant definitions available.")
        }
      })
    })
  })
}
