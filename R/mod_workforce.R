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
            id = ns("slide_occ"),
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
                choices = "All LHDs"
              )
            )
          ),
          hr(),

          div(
            id = ns("slide_measures"),
            div(
              id = ns("slide"),
              actionButton(
                ns("slide_button"),
                "",
                icon = icon("filter"),
                class = "dropdown-button"
              )
            ),
            div(
              id = ns("slideMenu"),
              class = "measures-shown",
              # FTEs
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'c5q37\'"),
                radioButtons(
                  ns("type_fte"),
                  "",
                  choices = workforce_fte_types,
                  inline = TRUE
                )
              ),

              # Employees
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'c5q36\'"),
                radioButtons(
                  ns("type_emp"),
                  "",
                  choices = workforce_employee_types,
                  inline = TRUE
                )
              ),

              # Jobs
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'jobs\'"),
                radioButtons(
                  ns("type_jobs"),
                  "",
                  choices = list("Jobs Lost/Added (Total)" = "jobs_total",
                                 "Jobs Lost/Added (Standardized)" = "jobs_standard"),
                  inline = TRUE
                )
              ),

              # Occupation Classifications
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'occupation_class\'"),
                div(
                  class = "occupation-measures",
                  radioButtons(
                    ns("type_class"),
                    "",
                    choices = workforce_classifications_measure,
                    inline = TRUE
                  )
                )
              )
            )
          ),

          tags$script(HTML("
              document.getElementById('workforce_1-slide_button').addEventListener('click', function() {
                var slideMenu = document.querySelector('#workforce_1-slideMenu');
                if (slideMenu.classList.contains('open')) {
                  slideMenu.classList.remove('open');
                } else {
                  slideMenu.classList.add('open');
                }
              });
            ")
          ),

          br(),

          # add main page content here ----------------------------------------------

          div(
            class = "viz-content with-slider",
            id = "resize-content", # needed for script.js

            # display when map is selected and view is not Table
            conditionalPanel(
              condition = paste('input["', ns("topic"), '"] != "jobs" && input["', ns("view"), '"] != "Table"', sep = ""),
              highcharter::highchartOutput(ns("figure"), height = 575)
            ),

            # display when map is not selected and view is not Table
            conditionalPanel(
              condition = paste('input["', ns("topic"), '"] == "jobs" && input["', ns("view"), '"] != "Table"', sep = ""),
              highcharter::highchartOutput(ns("figure_jobs"), height = 675)
            ),

            # Adding the new table view
            conditionalPanel(
              condition = paste('input["', ns("view"), '"] == "Table"', sep = ""),
              reactable::reactableOutput(ns("data_table"), height = "auto")
            )
          ),

          # end of main content -----------------------------------------------------

        right = div(
          div(
            class = "row color-block-header",
            id = ns("dynamic_row"),
            div(
              class = "col hover-text-side hide-icon",  # Hidden by default
              align = "center",
              actionLink(
                ns("get_classifications"), # add occupation class
                "",
                icon = tags$i(class = "fa-light fa-filter",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Add Classification")
            ),

            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("takeaway"), # key takeaway
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Takeaways")
            ),

            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("context"), # survey questions and responses
                "",
                icon = tags$i(class = "fa-light fa-circle-info",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Notes")
            ),

            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("definitions"), # related definitions
                "",
                icon = tags$i(class = "fa-light fa-list",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),

          div(
            class = "row color-block-content",

            # toggle side content -----------------------------------------------------
            shinyjs::hidden(
              div(
                id = ns("toggle_get_classifications"),
                class = "context-content",
                uiOutput(ns("show_get_classifications"))
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
                class = "context-content",
                uiOutput(ns("show_definitions"))
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
      if (input$topic == "c5q37") {

        # nested if/else
        if (input$type_fte == "FTE_numcat") {
          updateSelectInput(session, "filter",
                            choices = landscape_filters)
        } else {
        updateSelectInput(session, "filter",
                          choices = "All LHDs")
        }

      } else if (input$topic == "c5q36") {

        # nested if/else
        if (input$type_emp == "employee_numcat") {
          updateSelectInput(session, "filter",
                            choices = landscape_filters)
        } else {
          updateSelectInput(session, "filter",
                            choices = "All LHDs")
        }

      } else if (input$topic == "occupation_class") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)

      } else if (input$topic == "jobs") {
        updateSelectInput(session, "filter",
                          choices = "All Filters")
      }

    })


    # Reactive to get the inputs that I need for filtering --------------------
    get_workforce_input <- function() {
      if (input$topic == "c5q37") { # FTEs
        input$type_fte
      } else if (input$topic == "c5q36") { # Employees
        input$type_emp
      } else if (input$topic == "jobs") { # Jobs
        input$type_jobs
      } else if (input$topic == "occupation_class") { # occupation classifications
        input$type_class
      } else {
        NULL
      }
    }


    # main content ============================================================

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(get_workforce_input()))
      meta %>% dplyr::filter(field == get_workforce_input())
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
      } else if (input$filter %in% c("All LHDs")) {
        c(naccho.viz::naccho_blue)

             }
           }
         )



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
        # highcharter::hc_plotOptions(
        #   series = list(
        #     groupPadding = 0
        #   )
        # )
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
          backgroundColor = "#fff",
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
    }) %>% debounce(100)


    inputs_emp <- reactive({
      input$topic
      input$filter
      input$type_emp
    }) %>% debounce(100)


    inputs_jobs <- reactive({
      input$topic
      input$filter
      input$type_jobs
    }) %>% debounce(100)


    # inputs for classifications
    inputs_class <- reactive({
      input$topic
      input$filter
      input$type_class
      input$classifications
    }) %>% debounce(100)




    # update proxy ------------------------------------------------------------
    # for Full-Time Equivalent ------------------------------------------------
    observeEvent(inputs_fte(), {
      use_js_proxy()

      # Subtopic: FTEs by Category
      # View: Bar Graph
      # Filters: All LHDs
      if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter == "All LHDs") {
        data <- return_survey_weighted_counts("FTE_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)

        # Subtopic: FTEs by Category
        # View: Bar Graph
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter != "All LHDs") {
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
        # Filters: All LHDs
      } else if (input$topic == "c5q37" & input$type_fte == "fte_bypop") {
        data <- create_hist(input = "fte_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))

        # Subtopic: Total FTE (Histogram)
        # View: Bar Graph
        # Filters: All LHDs
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
      # Filters: All LHDs
      if (input$topic == "c5q36" & input$type_emp == "c5q36") {
        data <- create_hist(input = "c5q36",
                            x_label = "Total Employees")
        proxy %>%
          proxy_to_hist_total(data,
                              meta = filter_meta(),
                              responses = sum(data$y))

        # Subtopic: Per Capita Employee (Histogram)
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "c5q36" & input$type_emp == "employee_bypop") {
        data <- create_hist(input = "employee_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))

        # Subtopic: Employee by Category
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "c5q36" & input$type_emp == "employee_numcat" & input$filter == "All LHDs") {
        data <- return_survey_weighted_counts("employee_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)

        # Subtopic: Employee by Category
        # View: Bar Graph
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "c5q36" & input$type_emp == "employee_numcat" & input$filter != "All LHDs") {
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
        dplyr::filter(filter %in% c(input$filter, "All LHDs"))

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

      if (input$filter != "All LHDs") {
        data <- data %>%
          group_by(group) %>%
          mutate(x = 1:length(x)-1)

        splitter <- data$filter
        data <- split(data, splitter)

        series_id <- unique(data[[input$filter]]$group)

        proxy %>%
          proxy_hist_to_group(data = data[[input$filter]],
                              data_all = data[['All LHDs']],
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



    # For tables--------

    output$data_table <- reactable::renderReactable({
      req(input$view == "Table")

      #add topic value for function
      topic = input$topic

        workforce_input <- get_workforce_input()  # Get the relevant input type based on the topic

         # Debugging: Print workforce_input and all relevant inputs
  cat("Debug: workforce_input = ", workforce_input, "\n")

        subtopic <- workforce_input




        # Debugging: print the final topic and subtopic values
        cat("Debug: topic = ", input$topic, "\n")
        cat("Debug: subtopic = ", subtopic, "\n")




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

      #FTE---------------------

      # Subtopic: FTEs by Category -------
      # Filters: All LHDs
      if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter == "All LHDs") {

        data <- return_survey_weighted_counts("FTE_numcat") %>%
          dplyr::mutate(x_label = NA)%>%
          dplyr::select(category, group, y, subtitle, Responses)  %>%
          tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))%>%
          dplyr::mutate(category = dplyr::case_when(category == "All Lhds" ~ "All LHDs",
                                                    TRUE ~ category))



      } else if (input$topic == "c5q37" & input$type_fte != "FTE_numcat" & input$filter == "All LHDs") {
        data <- return_survey_weighted_statistics(input$type_fte) %>%
          dplyr::mutate(x_label = NA,
                        subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter])))%>%
          dplyr::select(category, group, y, subtitle, Responses) %>%
          tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))%>%
          dplyr::mutate(category = dplyr::case_when(category == "All Lhds" ~ "All LHDs",
                                                    TRUE ~ category))




        # Subtopic: FTEs by Category -------
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter != "All LHDs") {

        data <- return_survey_weighted_counts("FTE_numcat", input$filter)  %>%
          dplyr::mutate(x_label = NA)%>%
          dplyr::select(category, group, y, subtitle, Responses)%>%
          dplyr::group_by(category)%>%
          dplyr::mutate('Responses_All LHDs' = sum(Responses), 'y_All LHDs' = mean(y, na.rm = TRUE))%>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))%>%
          dplyr::mutate(category = dplyr::case_when(category == "All Lhds" ~ "All LHDs",
                                                    TRUE ~ category))



        # Subtopic: FTEs Total and Per Capita -------
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "c5q37" & input$type_fte != "FTE_numcat" & input$filter != "All LHDs") {

        data <- return_survey_weighted_statistics(input$type_fte, input$filter)  %>%
          dplyr::mutate(x_label = NA)%>%
          dplyr::select(category, group, y, subtitle, Responses)  %>%
          dplyr::mutate('y_All LHDs' = mean(y, na.rm = TRUE), 'Responses_All LHDs' = sum(Responses)) %>%
          tidyr::pivot_wider(names_from = category, values_from = c(y, Responses)) %>%
          dplyr::mutate(category = dplyr::case_when(category == "All Lhds" ~ "All LHDs",
                                                    TRUE ~ category))

      }


      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic, subtopic)
    })



    # right content ===========================================================

    observeEvent(input$topic, {
      if (input$topic == "occupation_class") {
        shinyjs::addClass(selector = "#workforce_1-dynamic_row", class = "show-icon")
        session$sendCustomMessage(type = "triggerClick", message = "workforce_1-get_classifications")
      } else {
        shinyjs::removeClass(selector = "#workforce_1-dynamic_row", class = "show-icon")
        session$sendCustomMessage(type = "triggerClick", message = "workforce_1-takeaway")
      }
    })

    # show occupational classifications ---------------------------------------
    observeEvent(input$get_classifications, {
      shinyjs::show("toggle_get_classifications")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_get_classifications <- renderUI({
        checkboxGroupInput(
          ns("classifications"),
          strong("Select Classification(s)"),
          workforce_classifications,
          selected = c("c5q63", "c5q52")
        )
      })
    }, ignoreNULL = FALSE)


    # takeaway ----------------------------------------------------------------
    observeEvent(input$takeaway, {
      shinyjs::hide("toggle_get_classifications")
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_takeaway <- renderUI({
        print(get_workforce_input())
        req(!is.null(get_workforce_input()))
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        subtopic == get_workforce_input(),
                        view == input$view,
                        filter == input$filter)

        p(htmltools::HTML(
          "<b>Key Takeaway(s)</b>: <br><br>",
          notes$takeaway))
      })
    }, ignoreNULL = FALSE)



    # survey questions, responses, and notes ----------------------------------
    observeEvent(input$context, {
      shinyjs::hide("toggle_get_classifications")
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        req(!is.null(get_workforce_input()))
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        subtopic == get_workforce_input(),
                        view == input$view,
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
            notes$notes))
        }
      })
    })



    # definitions -------------------------------------------------------------
    observeEvent(input$definitions, {
      shinyjs::hide("toggle_get_classifications")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::show("toggle_definitions")

      output$show_definitions <- renderUI({
        definitions <- definitions %>%
          dplyr::filter(field %in% c(get_workforce_input(), input$filter))

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
          links
        )
      })
    })

  })
}

## To be copied in the UI
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
