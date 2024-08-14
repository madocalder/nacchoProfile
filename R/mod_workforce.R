#' workforce UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_workforce_ui <- function(id){
  ns <- NS(id)
  tagList(
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
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = workforce_topics,
                selectize = TRUE
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              selectInput(
                inputId = ns("view"),
                strong("Visualization Type"),
                choices = c(
                  "Bar Graph",
                  "Line Graph",
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
                choices = "No Filter"
              )
            )
          ),

          hr(),
          br(),

          # add main page content here ----------------------------------------------

          div(
            class = "viz-content",
            id = "resize-content",

            # display when map is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] != \'jobs\'"),
              highcharter::highchartOutput(ns("figure"), height = 575)
            ),

            # display when map is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == \'jobs\'"),
              highcharter::highchartOutput(ns("figure_jobs"), height = 675)
            )
          )
        ),

          # end of main content -----------------------------------------------------

        right = div(
          div(
            class = "row color-block-header",
            div(
              class = "col-sm-3 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("viz_filters"), # key takeaway
                "",
                icon = tags$i(class = "fa-light fa-filter",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Filters")
            ),

            div(
              class = "col-sm-3 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("takeaway"), # key takeaway
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Takeaways")
            ),

            div(
              class = "col-sm-3 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("context"), # survey questions and responses
                "",
                icon = tags$i(class = "fa-light fa-circle-info",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Notes")
            ),

            div(
              class = "col-sm-3 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("definitions"), # related definitions
                "",
                icon = tags$i(class = "fa-light fa-list",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr"),
          ),

          div(
            class = "row color-block-content",

            # toggle side content -----------------------------------------------------
            shinyjs::hidden(
              div(
                id = ns("toggle_viz_filters"),
                class = "context-content",
                uiOutput(ns("show_viz_filters"))
              )
            ),

            shinyjs::hidden(
              div(
                id = ns("toggle_takeaway"),
                class = "context-content",
                uiOutput(ns("show_takeaway"))
              )
            ),

            shinyjs::hidden(
              div(
                id = ns("toggle_context"),
                class = "context-content",
                uiOutput(ns("show_context"))
              )
            ),

            shinyjs::hidden(
              div(
                id = ns("toggle_definitions"),
                class = "context-content"
                # uiOutput(ns("show_definitions"))
              )
            )
          ),
          div(
            class = "row color-block-footer"
          )
        )
      )
    )
  )
}

#' workforce Server Functions
#'
#' @noRd
mod_workforce_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic == "occupation_class") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)
      } else if (input$topic == "c5q37") {
        updateSelectInput(session, "filter",
                          choices = "No Filter")
      }
    })

    observeEvent(input$type_fte, {
      if (input$type_fte %in% c("c5q37", "fte_bypop")) {
        updateSelectInput(session, "filter",
                          choices = "No Filter"
        )
      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)
      }
    })

    observeEvent(input$type_emp, {
      if (input$type_fte %in% c("c5q36", "employee_bypop")) {
        updateSelectInput(session, "filter",
                          choices = "No Filter"
        )
      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)
      }
    })

    observeEvent(input$type_jobs, {
        updateSelectInput(session, "filter",
                          choices = "All Filters")
    })


    # right sidebar -----------------------------------------------------------
    observeEvent(input$viz_filters, {
      shinyjs::show("toggle_viz_filters")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")



      # additional filters for right sidebar ------------------------------------
      output$show_viz_filters <- renderUI({

        # display if occupational classifications is selected
        if (input$topic == "occupation_class") {
          div(
            radioButtons(
              ns("type_class"),
              strong("Select a Measure"),
              choices = workforce_classifications_measure
            ),
            br(),
            div(
              class = "checkboxes",
              checkboxGroupInput(
                ns("classifications"),
                strong("Select Classification(s)"),
                workforce_classifications,
                selected = c("c5q63", "c5q52")
              )
            )
          )

          # display fte is selected
        } else if (input$topic == "c5q37") {
          radioButtons(
            ns("type_fte"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between various measurements or
                 groupings. <br> <br>")),
            choices = workforce_fte_types
          )

          # display employees is selected
        } else if (input$topic == "c5q36") {
          radioButtons(
            ns("type_emp"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between various measurements or
                 groupings. <br> <br>")),
            choices = workforce_employee_types
          )

          # display job gains and losses is selected
        } else if (input$topic == "jobs") {
          radioButtons(
            ns("type_jobs"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between measurements or
                 groupings. <br> <br>")),
            choices = list("Jobs Lost/Added (Total)" = "jobs_total",
                        "Jobs Lost/Added (Standardized)" = "jobs_standard")
          )
        }
      })
    })



    # takeaway ----------------------------------------------------------------
    observeEvent(input$takeaway, {
      shinyjs::hide("toggle_viz_filters")
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view,
                        filter == input$filter)

        p(htmltools::HTML(
          "<b>Key Takeaway(s)</b>: <br><br>",
          notes$takeaway))
      })
    }, ignoreNULL = FALSE)



    # survey questions, responses, and notes ----------------------------------
    observeEvent(input$context, {
      shinyjs::hide("toggle_viz_filters")
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view,
                        filter == input$filter)

        if (is.na(notes$notes)) {
          p(htmltools::HTML(
            "<b>Survey Question</b>: <br>",
            notes$survey_question,
            "<br> <br> <b> Survey Response Options: </b>",
            notes$survey_response))

        } else {
          p(htmltools::HTML(
            "<b>Survey Question</b>: <br>",
            notes$survey_question,
            "<br> <br> <b> Survey Response Options: </b>",
            notes$survey_response,
            "<br> <br> <b> Notes: </b>",
            notes$notes))
        }
      })
    })



    # definitions -------------------------------------------------------------
    observeEvent(input$definitions, {
      shinyjs::hide("toggle_viz_filters")
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
          links
        )
      })
    })



    # main content ------------------------------------------------------------

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      if (input$topic == "c5q37") {
        req(input$type_fte)
        meta %>% dplyr::filter(field == input$type_fte)
      } else if (input$topic == "occupation_class") {
        meta %>% dplyr::filter(field %in% input$type_class)
      } else if (input$topic == "c5q36") {
        req(input$type_emp)
        meta %>% dplyr::filter(field == input$type_emp)
      } else if (input$topic == "jobs") {
        req(input$type_jobs)
        meta %>% dplyr::filter(field %in% input$type_jobs)
      }
    })



    # colors ------------------------------------------------------------------
    # reactively define the colors based on the filter
    colors <- reactive ({
      if (input$filter == "urbanrural2022") {
        c(naccho.viz::naccho_blue,
          naccho.viz::naccho_green)
      } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
        c(naccho.viz::naccho_blue,
          naccho.viz::naccho_green,
          naccho.viz::naccho_orange)
      } else if (input$filter == "censusregion") {
        c(naccho.viz::naccho_blue,
          naccho.viz::naccho_green,
          naccho.viz::naccho_orange,
          naccho.viz::naccho_pink)
      }
    })



    # figure output -----------------------------------------------------------
    # used for first histogram
    output$figure <- highcharter::renderHighchart({
      data <- create_hist(input = "c5q37",
                          x_label = "Total FTEs")

      set_hist(data) %>%
        viz_options(
          legend_enable = FALSE,
          viz_title = "The Distribution of Total FTEs Across LHDs",
          viz_caption = paste0("<b> N: </b>", sum(data$y), "<br>", source_line),
          viz_subtitle = unique(data$subtitle),
          tooltip = "{point.y} LHDs have between {point.low_bin} and {point.high_bin} total FTEs"
        ) %>%
        viz_export()
      # %>%
      #   highcharter::hc_plotOptions(
      #     series = list(
      #       groupPadding = 0
      #     )
      #   )
    })


    # used for jobs losses and gains
    output$figure_jobs <- highcharter::renderHighchart({

      data <- filtered_jobs()

      set_bar(data) %>%
        viz_options(
          colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green),
          legend_enable = TRUE,
          viz_title = "Jobs Lost and Added - Estimated Total",
          viz_caption = paste0("<b> N: </b>", unique(responses()$responses), "<br>", source_line),
          viz_subtitle = "All and Subsetted LHDs",
          tooltip = "<b> {series.name}: </b>
              {point.y} <br>",
          theme = naccho.viz::bar_pos_neg_theme
        ) %>%
        viz_export() %>%
        highcharter::hc_tooltip(
          headerFormat = "<b>{point.x} LHDs </b> <hr class = 'tt'>",
          style = list(
            textAlign = "left"
          ),
          hideDelay = 2000,
          shadow = FALSE,
          positioner = highcharter::JS("
          function (labelWidth, labelHeight, point) {
            return {
              x: 10,
              y: 10
            };
          }
        ")
        )
    })


    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    proxy <- highcharter::highchartProxy(ns("figure"))

    # save the figure_jobs as a proxy to redraw
    proxy_jobs <- highcharter::highchartProxy(ns("figure_jobs"))

    # update the yAxis title while maintaining the line animation
    use_js_proxy <- reactive({
      shinyjs::runjs(paste0("
        var chart = $('#workforce_1-figure').highcharts();
        chart.yAxis[0].update({
          title: {
            text: '", filter_meta()$y_label, "',
            style: { color: '#333333' }
          }
        }, true, { duration: 1000 });
    "))
    })



    # inputs for each topic ---------------------------------------------------
    inputs_fte <- reactive({
      input$topic
      input$filter
      input$type_fte
    }) %>% debounce(250)


    inputs_emp <- reactive({
      input$topic
      input$filter
      input$type_emp
    }) %>% debounce(250)


    inputs_jobs <- reactive({
      input$topic
      input$filter
      input$type_jobs
    }) %>% debounce(250)


    # inputs for classifications
    inputs_class <- reactive({
      input$topic
      input$filter
      input$type_class
      input$classifications
    }) %>% debounce(250)




    # update proxy ------------------------------------------------------------
    # for Full-Time Equivalent ------------------------------------------------
    observeEvent(inputs_fte(), {
      use_js_proxy()

      # Subtopic: FTEs by Category
      # View: Bar Graph
      # Filters: No Filter
      if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter == "No Filter") {
        data <- return_survey_weighted_counts("FTE_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)

        # Subtopic: FTEs by Category
        # View: Bar Graph
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter != "No Filter") {
        data <- return_survey_weighted_counts("FTE_numcat", input$filter) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group) # provide ID for proxy

        proxy %>%
          proxy_hist_to_group(data = data,
                              data_all = NULL,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = sum(data$Responses),
                              theme = naccho.viz::basic_theme)

        # Subtopic: Per Capita FTE (Histogram)
        # View: Bar Graph
        # Filters: No Filter
      } else if (input$topic == "c5q37" & input$type_fte == "fte_bypop") {
        data <- create_hist(input = "fte_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))

        # Subtopic: Total FTE (Histogram)
        # View: Bar Graph
        # Filters: No Filter
      } else if (input$topic == "c5q37" & input$type_fte == "c5q37") {
        data <- create_hist(input = "c5q37",
                            x_label = "Total FTEs")
        proxy %>%
          proxy_to_hist_total(data,
                              meta = filter_meta(),
                              responses = sum(data$y))
      }
    })




    # For Employees -----------------------------------------------------------
    observeEvent(inputs_emp(), {
      use_js_proxy()

      # Subtopic: Total Employee (Histogram)
      # View: Bar Graph
      # Filters: No Filter
      if (input$topic == "c5q36" & input$type_emp == "c5q36") {
        data <- create_hist(input = "c5q36",
                            x_label = "Total Employees")
        proxy %>%
          proxy_to_hist_total(data,
                              meta = filter_meta(),
                              responses = sum(data$y))

        # Subtopic: Per Capita Employee (Histogram)
        # View: Bar Graph
        # Filters: No Filter
      } else if (input$topic == "c5q36" & input$type_emp == "employee_bypop") {
        data <- create_hist(input = "employee_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))

        # Subtopic: Employee by Category
        # View: Bar Graph
        # Filters: No Filter
      } else if (input$topic == "c5q36" & input$type_emp == "employee_numcat" & input$filter == "No Filter") {
        data <- return_survey_weighted_counts("employee_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)

        # Subtopic: Employee by Category
        # View: Bar Graph
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "c5q36" & input$type_emp == "employee_numcat" & input$filter != "No Filter") {
        data <- return_survey_weighted_counts("employee_numcat", input$filter) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group)

        proxy %>%
          proxy_hist_to_group(data = data,
                              data_all = NULL,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = sum(data$Responses),
                              theme = naccho.viz::basic_theme)
      }
    })



    # For Jobs Lost/Added (Total) -------------------------------------------------
    observeEvent(inputs_jobs(), {
      use_js_proxy()

      # Subtopic: Jobs Lost/Added (Standardized)
      # View: Bar Graph
      # Filters: All
      if (input$type_jobs == "jobs_standard") {

        data <- filtered_jobs() %>%
          dplyr::mutate(jobsgone = jobsgone_standard,
                        jobsgain = jobsgain_standard)

        proxy_jobs %>%
          proxy_to_bar(data,
                       meta = filter_meta(),
                       max = 10)

      # Subtopic: Jobs Lost/Added (Total)
      # View: Bar Graph
      } else {

        data <- filtered_jobs()

        proxy_jobs %>%
          proxy_to_bar(data,
                       meta = filter_meta(),
                       max = 15000)
      }

    })

    # For Occupational Classifications ----------------------------------------
    observeEvent(inputs_class(), {
      req(input$topic == "occupation_class")
      use_js_proxy()

      # prevent error
      req(length(input$classifications) >= 1)

      selected_vars <- c(paste0(input$classifications, input$type_class))
      selected_data <- get(paste0("classification_data_", input$type_class))
      data <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::filter(filter %in% c(input$filter, "No Filter"))

      data <- data %>%
        dplyr::filter(category == "Yes") %>%
        dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(.data$Question)]),
                      x = 1:length(category) -1,
                      x_label = NA)

      # for responses
      Responses <- selected_data %>%
        group_by(Question, filter) %>%
        summarise(Responses = sum(Responses))


      if (length(data$x) >= 8) {
        rotate = -45
        size = "13px"
        overflow = "ellipsis"
      } else {
        rotate = 0
        size = "17px"
        overflow = "none"
      }

      if (input$filter != "No Filter") {
        data <- data %>%
          group_by(group) %>%
          mutate(x = 1:length(x)-1)

        splitter <- data$filter
        data <- split(data, splitter)

        series_id <- unique(data[[input$filter]]$group)

        proxy %>%
          proxy_hist_to_group(data = data[[input$filter]],
                              data_all = data[['No Filter']],
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              rotate = rotate,
                              size = size,
                              overflow = overflow,
                              responses = show_n(Responses$Responses),
                              theme = naccho.viz::basic_theme)
      } else {
        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               responses = show_n(Responses$Responses),
                               theme = naccho.viz::basic_theme,
                               rotate = rotate,
                               size = size,
                               overflow = overflow)
      }
    })
  })
}

## To be copied in the UI
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
