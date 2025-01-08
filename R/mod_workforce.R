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
          class = "main-content",
          style = "display: grid !important;
    grid-template-rows: minmax(75px, 8%) minmax(40px, 4%) minmax(350px, 75vh);
    grid-template-columns: minmax(40%, auto) minmax(40%, auto);
    gap: clamp(3px, 2.5vh, 30px) 5%;
    width: clamp(50vw, 100%, 95vw);
    height: calc(100vh - 4rem);
    padding-left: 4vw;
   /*  padding-bottom: 3em; */
    padding-right: 1vw;
    min-width: 70%;
    max-width: 95%;
    align-items: start;
    /* justify-content: center !important;  */
    /* padding-top: 1em; */
    max-height: calc(100vh - 4rem);
    /* margin-top: -10px; */;
          ",

          openMenu(),

            div(
              # class = "col-sm-6",
              # style = "max-height: 10vh",
              style = "grid-area: 1 / 1 / 2 / 2;
              justify-self: center;
              display: block;
              /* align-self: end; */
              /* margin-bottom: clamp(2px, 2.5%, 8px);*/",
              # br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = workforce_topics,
                selectize = TRUE,
                selected = workforce_topics[[1]][3]
              )
            ),
            div(
              # class = "col-sm-6",
              # style = "max-height: 10vh",
              style = "grid-area: 1 / 2 / 2 / 3;
              justify-self: center;
              display: block;
              /* align-self: end; */
              /* margin-bottom: clamp(2px, 2.5%, 8px);*/",

              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters,
                selectize = TRUE
              )

           ),

            div(
             #  class = "viz-view",
             #  style = "grid-column: 1 / -1; display: grid;

              style = "grid-area: 2 / 1 / 3 / 3;
    /* justify-self: center; */
    /* align-self: start; */
    display: unset !important;
    position: relative;
    background: linear-gradient(to right, #008A96 100%, transparent 0%)
             no-repeat center / 100% 1.5px;
    padding-right: 30%;
    padding-left: 30%;
    justify-self: center;
    align-self: center;
    /* margin-bottom: clamp(5px, 3%, 30px); */
    margin-top: clamp(-2px, -2.5%, -8px);",

              shinyWidgets::radioGroupButtons(
                inputId = ns("view"),
                "",
                choiceNames = list(
                  HTML('<i class="fas fa-chart-simple"></i>'),  # Bar Graph icon
                  HTML('<i class="fas fa-chart-line"></i>'),    # Line Graph icon
                  HTML('<i class="fas fa-table"></i>')          # Table icon
                ),
                choiceValues = c("Bar Graph", "Line Graph", "Table"),
                selected = "Bar Graph",
                # size = "lg", # Optional: Adjust button size
                direction = "horizontal" # Ensure buttons are in a single row
              )

          ),
          # Visualization content area
          div(
            # class = "viz-content",
          style = "grid-area: 3 / 1 / 3 / 3;
    justify-self: center;
    align-self: start;
    overflow-y: visible;
    overflow-x: clip;
    width: clamp(50vw, 100%, 90vw);
    height: clamp(375px, 100%, 70vh);
    padding-bottom: 1em;
    /* margin-top: 1%; */",

            conditionalPanel(
              condition = paste0("input['", ns('view'), "'] == 'Bar Graph'"),
              highcharter::highchartOutput(ns("figure"), height = "100%")
              # ,
              #                              style = "height: 65vh; width: auto;"
            ),
            conditionalPanel(
              condition = paste0("input['", ns('view'), "'] == 'Line Graph'"),
              highcharter::highchartOutput(ns("line"), height = "100%")
              # ,
              #                              style = "height: 65vh; width: auto;"
            ),
            conditionalPanel(
              condition = paste0("input['", ns('view'), "'] == 'Table'"),
              div(
                uiOutput(ns("table_title")),
                csvDownloadButton(ns("data_table"), filename = "workforceNACCHO.csv"),
                reactable::reactableOutput(ns("data_table"), height = "auto"),
                br(),
                HTML('<div style="font-size: 1.8vmin;">', source_line, '<br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),
                     style = "margin-left: auto !important;
                        margin-right: auto !important;"
              )
            )
          )
        ),
        # Sidebar layout for additional information
        right = div(
          div(
            class = "row color-block-header",
            uiOutput(ns("class_icon_container")),
            div(
              class = "col hover-text-side",
              align = "center",
              br(),
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
              br(),
              actionLink(
                ns("context"),
                "",
                icon = tags$i(class = "fa-light fa-circle-info", style = "font-size: calc(30px + .2em); text-align: center;"),
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
                icon = tags$i(class = "fa-light fa-list", style = "font-size: calc(30px + .2em); text-align: center;"),
                class = "link-no-underline action-link-2"
              ),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),
          # Toggling side content
          div(
            class = "row color-block-content",

            shinyjs::hidden(div( id = ns("toggle_get_class"),
              p(htmltools::HTML("<b>Select Occupation Classification:</b>")),
              uiOutput(ns("class_input"))
              )
              ),

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

    updated_inputs <- reactive({
      input$topic
      input$filter
      input$view
      # input$classifications
    }) %>% debounce(100)

    output$class_icon_container <- renderUI({
      if (input$topic %in% c("a", "b")) {
        div(
          class = "col hover-text-side",
          align = "center",
          br(),
          actionLink(
            ns("get_class"),
            "",
            icon = tags$i(class = "fa-light fa-filter", style = "font-size: 1.85em; margin-top: 2vmin; text-align: center;"),
            class = "link-no-underline action-link-2"
          ),
          span(class = "hover-text-content-side", "Select Occupation Classification")
        )
      } else {
        NULL  # Remove the entire container
      }
    })


    observe({
      req(updated_inputs())

      topic <- input$topic

      # Define views based on the topic
      if (topic %in% c("FTE_numcat", "employee_numcat", "jobs_total", "a", "b")) {
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

    observe({
      # req(updated_inputs())

      if (input$topic %in% c("c5q37", "c5q36", "fte_bypop")) {
        updateSelectInput(session, "filter",
                          choices = c(landscape_filters[1], landscape_filters[4]))


      } else if (input$topic %in% c("FTE_numcat", "employee_numcat")) {

        updateSelectInput(session, "filter",
                          choices = landscape_filters[1])

      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)
      }
    })

    # Reactive to filter metadata based on the selected topic
    filter_meta <- reactive({
      # req(updated_inputs())
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      meta %>%
        dplyr::filter(field == input$topic,
                      stringr::str_detect(view, input$view),
                      stringr::str_detect(filter, input$filter))

    })


    # Reactive for colors based on the filter
    colors <- reactive({
      # req(updated_inputs())
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

    # Generalized data processing for charts
    process_chart_data <- reactive({
      # req(updated_inputs())

      data <- if (input$topic %in% c("a", "b")) {
        classification_data <- process_classification_data(
          topic = input$topic,
          classifications = input$classifications,
          filtering = input$filter
          # ,
          # q_remap = q_remap
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
   table_data <- reactive({
     # req(updated_inputs())

      topic <- input$topic
      filtering <- input$filter
      classifications <- NULL

      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL
      }

      table_data <- process_table_data(topic, filtering, classifications)


      # table_data %>% dplyr::mutate(
      #   dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
      # )

      message("topic = ", topic)
      message("filtering = ", filtering)
      # View(table_data)

      return(table_data)


    })

    # Generalized data processing for line charts
    process_line_chart_data <- reactive({

      # req(updated_inputs())
      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL
      }

      process_line_data(input$topic, input$filter, classifications)

    })

    #table title output
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      # req(updated_inputs())
      req(filter_meta())
      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(filter_meta()$title)

      # Render the HTML
      p(title_values, style = "font-size: 3vmin; font-weight: bold !important; text-align: center;")
    })


    # Bar Graph Rendering
    output$figure <- highcharter::renderHighchart({
      req(updated_inputs())
      req(filter_meta())
      req(input$view == "Bar Graph", input$filter, input$topic)

      data <- process_chart_data()

      if (input$topic == "jobs_total") {
        max_one <- max(data$left, na.rm = TRUE)
        max_two <- max(data$right, na.rm = TRUE)
        set_bar(
          data,
          y_label = filter_meta()$y_label,
          max_one = max_one,
          max_two = max_two,
          name_one = "Added",
          name_two = "Lost",
          y_format = "{value}"
        ) %>%
          viz_options(
            colors = c(naccho.viz::terracotta, naccho.viz::green),
            legend_enable = TRUE,
            legend_title = "# of Jobs...",
            viz_title = "Jobs Lost and Added - Estimated Total",
            viz_caption = paste("<b> N: </b>902 <br>", source_line),
            tooltip = filter_meta()$tooltip,
            theme = naccho.viz::butterfly
          ) %>%
          viz_export()


      } else {

        set_column(data) %>%
          viz_options(
            viz_title = filter_meta()$title,
            viz_caption = paste("<b> N: </b>", sum(data$Responses), "<br>", source_line),
            tooltip = filter_meta()$tooltip
          ) %>%
          viz_export()%>%
          highcharter::hc_yAxis(
            maxPadding = .09,
            max = filter_meta()$y_max,
            endOnTick = FALSE
          )
      }
    })

    # Line Graph Rendering
    output$line <- highcharter::renderHighchart({
      # req(updated_inputs())
      req(filter_meta())
      req(input$view == "Line Graph", !(input$topic %in% c("FTE_numcat", "employee_numcat")))

      data <- process_line_chart_data() %>%
        dplyr::mutate(x_label = NA)

      naccho.viz::create_grouped_line(
        data = data,
        x_variable = "year",
        x_group = "legend_label",
        y_variable = "y",
        x_label = NULL,
        select_theme = naccho.viz::basic,
        y_max = filter_meta()$y_max,
        y_label = filter_meta()$y_label,
        title = filter_meta()$title,
        tooltip = filter_meta()$tooltip
      )%>%
        viz_options(viz_caption = paste(
          "<b> N: </b>", show_n(data$Responses),
          "<br>", source_line
        ),
        tooltip = filter_meta()$tooltip,
        )%>%
        highcharter::hc_title(text = filter_meta()$title)%>%
        highcharter::hc_yAxis(
          maxPadding = .09,
          max = filter_meta()$y_max,
          endOnTick = FALSE
        )%>%
        highcharter::hc_legend(reversed = TRUE)
      # %>%
      #   highcharter::hc_chart(style = list(minHeight = "00px"))
    })

    # Table Rendering
    output$data_table <- reactable::renderReactable({
      req(updated_inputs())
      req(input$view == "Table")
      data <- table_data()
      create_non_time_table(data, input$filter, colors(), input$topic)
    })

    # Show Occupation Classifications

    observeEvent(input$get_class, {
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")
      shinyjs::show("toggle_get_class")

      output$class_input <- renderUI({
        renderUI({
          checkboxGroupInput(
            inputId = ns("classifications"),
            label = "",
            choices = workforce_classifications,  # List of services
            selected = workforce_classifications[1]
          )

      })

    })
    })


    observeEvent(input$takeaway, {
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")
      shinyjs::hide("toggle_get_class")
      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        filter == input$filter) %>%
          dplyr::filter(stringr::str_detect(view, input$view)) %>%
          pull(takeaway)
        p(HTML("<b>Key Takeaway(s):</b><br><br>", notes))
      })
    })

    observeEvent(input$context, {
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")
      shinyjs::hide("toggle_get_class")
      output$show_context <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        filter == input$filter)
        p(HTML("<b>Survey Question(s):</b><br>", notes$survey_question, "<br><br>",
               "<b>Survey Response Options:</b>", notes$survey_response))
      })
    })

    observeEvent(input$definitions, {
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::show("toggle_definitions")
      shinyjs::hide("toggle_get_class")
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
