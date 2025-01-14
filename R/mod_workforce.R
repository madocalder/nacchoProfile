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
   #        style = "display: grid !important;
   #  grid-template-rows: minmax(75px, 8%) minmax(40px, 4%) minmax(350px, 75vh);
   #  grid-template-columns: minmax(40%, auto) minmax(40%, auto);
   #  gap: clamp(3px, 2.5vh, 30px) 5%;
   #  width: clamp(50vw, 100%, 95vw);
   #  height: calc(100vh - 4rem);
   #  padding-left: 4vw;
   # /*  padding-bottom: 3em; */
   #  padding-right: 1vw;
   #  min-width: 70%;
   #  max-width: 95%;
   #  align-items: start;
   #  /* justify-content: center !important;  */
   #  /* padding-top: 1em; */
   #  max-height: calc(100vh - 4rem);
   #  /* margin-top: -10px; */;
   #        ",

          openMenu(),

            div(
              class = "topic-select",
              # style = "max-height: 10vh",
              # style = "grid-area: 1 / 1 / 2 / 2;
              # justify-self: center;
              # display: block;
              # /* align-self: end; */
              # /* margin-bottom: clamp(2px, 2.5%, 8px);*/",
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
              class = "filter-select",


              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters,
                selectize = TRUE
              )

           ),

            div(
              class = "viz-view",
             #  style = "grid-column: 1 / -1; display: grid;

    #           style = "grid-area: 2 / 1 / 3 / 3;
    # /* justify-self: center; */
    # /* align-self: start; */
    # display: unset !important;
    # position: relative;
    # background: linear-gradient(to right, #008A96 100%, transparent 0%)
    #          no-repeat center / 100% 1.5px;
    # padding-right: 30%;
    # padding-left: 30%;
    # justify-self: center;
    # align-self: center;
    # /* margin-bottom: clamp(5px, 3%, 30px); */
    # margin-top: clamp(-2px, -2.5%, -8px);",

              shinyWidgets::radioGroupButtons(
                inputId = ns("view"),
                "",
                choiceNames = list(
                  HTML('<i class="fas fa-chart-simple"></i>'),  # Bar Graph icon
                  HTML('<i class="fas fa-chart-line"></i>'),    # Line Graph icon
                  HTML('<i class="fas fa-table"></i>')          # Table icon
                ),
                choiceValues = c("Bar Graph", "Line Graph", "Table"),
                selected = "Table",
                # size = "lg", # Optional: Adjust button size
                direction = "horizontal" # Ensure buttons are in a single row
              )

          ),
          # Visualization content area
          div(
            class = "viz-content",
    #       style = "grid-area: 3 / 1 / 3 / 3;
    # justify-self: center;
    # align-self: start;
    # overflow-y: visible;
    # overflow-x: clip;
    # width: clamp(50vw, 100%, 90vw);
    # height: clamp(50vh, 100%, 70vh);
    # padding-bottom: 1em;
    # /* margin-top: 1%; */",



            conditionalPanel(
              condition =paste0('input[\'', ns('view'), '\'] != \'Table\'
                                && input[\'', ns('view'), '\'] != \'Line Graph\'
                                && input[\'', ns('view'), '\'] == \'Bar Graph\'
                                && input[\'', ns('topic'), '\'] != \'jobs_total\''),

              highcharter::highchartOutput(ns("figure"), height = "100%"),
              style = "height: 100%"

            ),

          conditionalPanel(
            condition =paste0('input[\'', ns('view'), '\'] != \'Table\'
                                && input[\'', ns('view'), '\'] != \'Line Graph\'
                                && input[\'', ns('view'), '\'] == \'Bar Graph\'
                                && input[\'', ns('topic'), '\'] == \'jobs_total\''),

            highcharter::highchartOutput(ns("butterfly"), height = "100%"),
            style = "height: 100%"
            ),

            conditionalPanel(
              condition =  paste0('input[\'', ns('view'), '\'] == \'Line Graph\'
                                  && input[\'', ns('view'), '\'] != \'Bar Graph\'
                                && input[\'', ns('view'), '\'] != \'Table\''),

              highcharter::highchartOutput(ns("line"), height = "100%"),
              style = "height: 100%"
            ),
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), '\'] == \'Table\'
                                  && input[\'', ns('view'), '\'] != \'Bar Graph\'
                                && input[\'', ns('view'), '\'] != \'Line Graph\''),
              div( class = "table-grid",


                div(class = "table-title-grid",


                uiOutput(ns("table_title")),
                csvDownloadButton(ns("data_table"), filename = "workforceNACCHO.csv")
                ),

                reactable::reactableOutput(ns("data_table"),
                                           height = "100%",
                                           width = "auto"
                                           ),

                HTML('<div style="font-size: calc(5px + .2em + .8vmin);">', source_line, '<br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>')

              )
            )
          )
        ),
        # Sidebar layout for additional information
        right = div(
          div(
            class = "row color-block-header",

            # uiOutput(ns("class_icon_container")),
           conditionalPanel(condition = paste0('input[\'', ns('topic'), '\'] == \'a\'
                                | input[\'', ns('topic'), '\'] == \'b\''),

                              class = "col hover-text-side",
                              align = "center",
                              br(),
                              actionLink(
                                ns("get_class"),
                                "",
                                icon = tags$i(class = "fa-light fa-filter",
                                              style = "font-size: calc(28px + .2em); text-align: center;"),
                                class = "link-no-underline action-link-2"
                              ),
                              span(class = "hover-text-content-side", "Select Occupation Classification")

                            ),
            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("takeaway"),
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: calc(28px + .2em); text-align: center;"),
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
                icon = tags$i(class = "fa-light fa-circle-info", style = "font-size: calc(28px + .2em); text-align: center;"),
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
                icon = tags$i(class = "fa-light fa-list", style = "font-size: calc(28px + .2em); text-align: center;"),
                class = "link-no-underline action-link-2"
              ),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),
          # Toggling side content
          div(
            class = "row color-block-content",

            shinyjs::hidden(div(id = ns("toggle_get_class"),
                                class = "context-content",
              uiOutput(ns("show_class_input"))
              )
              ),

            shinyjs::hidden(div(id = ns("toggle_takeaway"),
                                class = "context-content",
                                uiOutput(ns("show_takeaway")))),

            shinyjs::hidden(div(id = ns("toggle_context"),
                                class = "context-content",
                                uiOutput(ns("show_context")))),

            shinyjs::hidden(div(id = ns("toggle_definitions"),
                                class = "context-content",
                                uiOutput(ns("show_definitions"))))
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

    # output$class_icon_container <- renderUI({
    #   if (input$topic %in% c("a", "b")) {
    #     div(
    #       class = "col hover-text-side",
    #       align = "center",
    #       br(),
    #       actionLink(
    #         ns("get_class"),
    #         "",
    #         icon = tags$i(class = "fa-light fa-filter",
    #                       style = "font-size: calc(30px + .2em); text-align: center;"),
    #         class = "link-no-underline action-link-2"
    #       ),
    #       span(class = "hover-text-content-side", "Select Occupation Classification")
    #     )
    #   } else {
    #     NULL  # Remove the entire container
    #   }
    # })


    observe({
      req(input$topic, input$view)

      topic <- input$topic
      selected_view <- input$view


      # Define views based on the topic
      if (topic %in% c("FTE_numcat", "employee_numcat", "jobs_total", "a", "b")) {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-simple"></i>'),  # Bar Graph icon
            HTML('<i class="fas fa-table"></i>')         # Table icon
          ),
          choiceValues = c("Bar Graph", "Table"),
          selected = dplyr::case_when(
            selected_view %in% c("Bar Graph", "Table") ~ selected_view,
            TRUE ~ "Bar Graph")
        )
      } else if (topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop")) {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-line"></i>'),  # Line Graph icon
            HTML('<i class="fas fa-table"></i>')       # Table icon
          ),
          choiceValues = c("Line Graph", "Table") ,
          selected = dplyr::case_when(
            selected_view %in% c("Line Graph", "Table") ~ selected_view,
            TRUE ~ "Line Graph")
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
          selected = selected_view
        )
      }
    })

    observe({
      req(input$topic)
      # req(updated_inputs())

      if (input$topic %in% c("c5q37", "c5q36", "fte_bypop")) {
        updateSelectInput(session, "filter",
                          choices = c(landscape_filters[1], landscape_filters[4]))


      } else if (input$topic %in% c("FTE_numcat", "employee_numcat", "employee_bypop")) {

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
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
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
          dplyr::mutate(x_label = NA,
                        Responses = sum(Responses))
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
      req(input$topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop"))

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
      p(title_values, class = "table-title"

      )
    })


    # Bar Graph Rendering
    output$figure <- highcharter::renderHighchart({
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      req(filter_meta())


      req(input$view == "Bar Graph",
          input$filter,
          input$topic != "jobs_total")


      data <- process_chart_data()

      bar_chart <- highcharter::highchart()

        bar_chart <- set_column(data,
                                y_label = filter_meta()$y_label,
                                y_format = filter_meta()$y_format,
                                data_label = filter_meta()$data_label) %>%
          viz_options(
            viz_title = filter_meta()$title,
            viz_caption = paste("<b> N: </b>", show_n(data$Responses), "<br>", source_line),
            tooltip = filter_meta()$tooltip
          ) %>%
          viz_export()%>%
          highcharter::hc_yAxis(
            maxPadding = .09,
            max = filter_meta()$y_max,
            endOnTick = TRUE
          )%>%
          highcharter::hc_legend(reversed = TRUE)


      bar_chart
    })

    # Proxy for the Bar Chart
    proxy_bar <- highcharter::highchartProxy(ns("figure"))

    observe({

      req(input$filter,
          input$topic != "jobs_total")  # Ensure inputs are available
      # req(updated_inputs())
      req(filter_meta())
      req(process_chart_data())


      # Remove all old series (to prevent overlap)
      proxy_bar %>%
        highcharter::hcpxy_remove_series(id = "filtered_line_series") %>%
        highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "butterfly_series")

      data <- process_chart_data()

      View(data)

      if (input$filter != "All LHDs") {

        proxy_bar %>%
        highcharter::hcpxy_add_series(
          id = "filtered_bar_series",
          name = input$filter,
          data = data,
          type = "column",  # Ensure this is a column chart (bar graph)
          selected = TRUE
        )

      } else {
          proxy_bar %>%
            highcharter::hcpxy_add_series(
              id = "all_lhds_bar_series",
              name = input$filter,
              data = data,
              type = "column",  # Ensure this is a column chart (bar graph)
              selected = TRUE
            )
      }

    })

    #Butterfly Rendering
    output$butterfly <- highcharter::renderHighchart({
      req(input$topic == "jobs_total")
      req(filter_meta())
      req(process_chart_data())

      data <- process_chart_data()

      butterfly_chart <- highcharter::highchart()

      max_one <- max(data$left, na.rm = TRUE)
      max_two <- max(data$right, na.rm = TRUE)

      butterfly_chart <- set_bar(
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

      butterfly_chart

    })

    proxy_butterfly <- highcharter::highchartProxy(ns("butterfly"))

    observe({

      req(input$topic == "jobs_total")
      req(filter_meta())

      proxy_butterfly %>%
        highcharter::hcpxy_remove_series(id = "filtered_line_series") %>%
        highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "butterfly_series")

      proxy_butterfly %>%
        highcharter::hcpxy_add_series(
          id = "butterfly_series",
          name = "butterfly",
          data = data,
          type = "bar",
          selected = TRUE
        )

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
          endOnTick = TRUE
        )%>%
        highcharter::hc_legend(reversed = TRUE)
      # %>%
      #   highcharter::hc_chart(style = list(minHeight = "00px"))
    })

    # Proxy for the Line Chart
    proxy_line <- highcharter::highchartProxy(ns("line"))

    observe({
      req(filter_meta())
      req(input$view == "Line Graph", !(input$topic %in% c("FTE_numcat", "employee_numcat")))

     proxy_line %>%
       highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "butterfly_series")

      data <- process_line_chart_data() %>%
        dplyr::mutate(x_label = NA)

      if (nrow(data) > 0) {
        proxy_line %>%
          highcharter::hcpxy_remove_series(id = "filtered_line_series") %>%
          highcharter::hcpxy_add_series(
            id = "filtered_line_series",
            name = input$filter,
            data = data,
            type = "line"
          )
      } else {
        message("No data for line graph update.")
      }


    })


    # Table Rendering
    output$data_table <- reactable::renderReactable({
      # req(updated_inputs())
      req(input$view == "Table")

      data <- table_data()
      colors <- colors()
      topic <- input$topic

      filtering <- input$filter

      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL  # If input$classifications does not exist, set it to NULL
      }

      category <- case_when(
        input$filter == "urbanrural2022" ~ "Rurality",
        input$filter == "c0govcat" ~ "Governance",
        input$filter == "c0popcat3" ~ "LHD size",
        input$filter == "censusregion" ~ "Region",
        input$filter == "All LHDs" ~ "All LHDs"
      )


      create_non_time_table(data, category, colors, topic)
    })

    # Show Occupation Classifications
observe({
    output$show_class_input <- renderUI({

      if(input$filter != "All LHDs" && input$view != "Table") {

        # div(
        #   p(htmltools::HTML(
        #     "<b>Select an Occupation Classification:</b> <br>")),
        #
        #   # radioButtons(
          #   inputId = ns("classifications"),
          #   label = "",
          #   choices = workforce_classifications,  # List of services
          #   selected = workforce_classifications[1]
          # )


          selectInput(inputId = ns("classifications"),
                             label = "Select an Occupation Classification:",
                             choices =  workforce_classifications,
                             multiple = FALSE,
                             selected =  workforce_classifications[1],
                            selectize = TRUE)
        # )


      } else {

        # div(
        #   p(htmltools::HTML(
        #     "<b>Select Occupation Classification(s):</b> <br>")),

          checkboxGroupInput(
            inputId = ns("classifications"),
            label = HTML('<b>Select Occupation Classification(s):</b>'),
            choices = workforce_classifications,  # List of services
            selected = workforce_classifications[1],
            width = "100%"
          )

          # selectInput(inputId = ns("classifications"),
          #                   label = "Select Occupation Classification(s):",
          #                   choices =  workforce_classifications,
          #                   multiple = TRUE,
          #                   selected =  workforce_classifications[1],
          #                   size = 19,
          #                   selectize = FALSE)


        # )

      }


    })
})

    observeEvent(input$get_class, {

      toggle_sidebar_content("toggle_get_class",
                             c("toggle_takeaway", "toggle_context",
                               "toggle_definitions", "toggle_get_class"))

      })




    observeEvent(input$takeaway, {
      toggle_sidebar_content("toggle_takeaway",
                             c("toggle_takeaway", "toggle_context",
                               "toggle_definitions", "toggle_get_class"))

      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        filter == input$filter)%>%
          dplyr::filter(stringr::str_detect(view, input$view))

        p(htmltools::HTML(
          "<b>Key Takeaway(s)</b>: <br><br>",
          notes$takeaway))
      })
    }, ignoreNULL = FALSE)


    observeEvent(input$context, {

      toggle_sidebar_content("toggle_context",
                               c("toggle_takeaway", "toggle_context",
                                 "toggle_definitions", "toggle_get_class"))


      output$show_context <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        filter == input$filter)%>%
          dplyr::filter(stringr::str_detect(view, input$view))

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

    observeEvent(input$definitions, {

      toggle_sidebar_content("toggle_definitions",
                             c("toggle_takeaway", "toggle_context",
                               "toggle_definitions", "toggle_get_class"))

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
            toggle_sidebar_content("toggle_definitions",
                                   c("toggle_takeaway", "toggle_context",
                                     "toggle_definitions", "toggle_get_class"))
          }
        )
      })
    })

    observe({
      req(input$topic)

      if (input$topic %in% c("a", "b")) {

      toggle_sidebar_content("toggle_get_class",
                             c("toggle_takeaway", "toggle_context",
                                 "toggle_definitions", "toggle_get_class"))

        session$sendCustomMessage("triggerClick", ns("get_class"))
      } else {
        shinyjs::hide("toggle_get_class")
        session$sendCustomMessage("removeActiveClass", ns("get_class"))
      }


    })



  })
}
