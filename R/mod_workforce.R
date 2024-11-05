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
                choices = "All LHDs"
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
                HTML('<i class="fas fa-chart-simple"></i>'), # Chart icon
                HTML('<i class="fas fa-chart-line"></i>'), # Line graph icon
                HTML('<i class="fas fa-table"></i>') # Table icon
              ),
              choiceValues = c("Bar Graph", "Line Graph", "Table")
            )
          ),



          # add main page content here ----------------------------------------------
          div(
            class = "viz-content",

            # Display bar graph when the topic is NOT jobs_standard or jobs_total and view is Bar Graph
            conditionalPanel(
              condition = paste0("input['", ns('topic'), "'] != 'jobs_standard' && input['", ns('topic'), "'] != 'jobs_total' && input['", ns('view'), "'] == 'Bar Graph'", sep = ""),
              div(
                highcharter::highchartOutput(ns("figure"), height = "100%"),
                style = "height: 70vh; width: 50vw;")
            ),

            # Display bar graph when the topic is jobs_standard or jobs_total and view is Bar Graph
            conditionalPanel(
              condition = paste0("(input['", ns('topic'), "'] == 'jobs_standard' || input['", ns('topic'), "'] == 'jobs_total') && input['", ns('view'), "'] == 'Bar Graph'", sep = ""),
              div(
                highcharter::highchartOutput(ns("figure_jobs"), height = "100%"),
                style = "height: 68vh; width: 50vw;")
            ),

            # Adding the new table view
            conditionalPanel(
              condition = paste('input["', ns("view"), '"] == "Table"', sep = ""),
              div(
                uiOutput(ns("table_title")),
                reactable::reactableOutput(ns("data_table"), height = "auto"),
                br(),
                HTML('<div style="font-size: 1.8vmin;">', source_line,'
                   <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),

                style = "height: 65vh; width: 50vw;")
            ),

            # view output Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("line"), height = "65vh"),
                style = "height: 65vh; width: 50vw;"
              )
            )
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
}

#' workforce Server Functions
#'
#' @noRd
mod_workforce_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("FTE_numcat", "employee_numcat", "a", "b") &
          input$view != "Line Graph") {

        updateSelectInput(session, "filter",
                          choices = landscape_filters)

      } else if (input$topic == "b" & input$view == "Line Graph") {

        updateSelectInput(session, "filter",
                          choices = "All LHDs")

      } else if (input$topic %in% c("c5q37", "c5q36") &
                 input$view %in% c("Line Graph", "Table")) {

        updateSelectInput(session, "filter",
                          choices = c(landscape_filters[1], landscape_filters[4]))

      } else if (input$topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop")) {
        updateSelectInput(session, "filter",
                          choices = "All LHDs")

      } else if (input$topic %in% c("jobs_total", "jobs_standard")) {
        updateSelectInput(session, "filter",
                          choices = "All LHDs")
      }
    })

    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("b", "c5q36", "c5q37")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Line Graph", "Table"))


      } else {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"))
      }
    })

    # main content ============================================================

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(input$topic))
      meta %>% dplyr::filter(field == input$topic)
    })


    # colors ------------------------------------------------------------------
    # reactively define the colors based on the filter
    colors <- reactive ({
      if (input$filter == "urbanrural2022") {
        c(naccho.viz::navy,
          naccho.viz::teal)
      } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
        c(naccho.viz::navy,
          naccho.viz::teal,
          naccho.viz::terracotta)
      } else if (input$filter == "censusregion") {
        c(naccho.viz::navy,
          naccho.viz::teal,
          naccho.viz::terracotta,
          naccho.viz::pink)
      } else if (input$filter %in% c("All LHDs")) {
        c(naccho.viz::navy,
          naccho.viz::terracotta)
      }
    })


    #table titles---------------------------------------------------------------
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      req(input$topic)

      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(subset(meta, field == input$topic)$title)

      # Render the HTML
      p(title_values, style = "font-size: 3vmin; font-weight: bold !important; text-align: center;")
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
          tooltip = "{point.y:.0f} LHDs have between {point.low_bin} and {point.high_bin} total FTEs"
        ) %>%
        viz_export()
    })


    # used for jobs losses and gains
    output$figure_jobs <- highcharter::renderHighchart({

      data <- filtered_butterfly()

      max_one <- max(data$left)
      max_two <- max(data$right)

      set_bar(data,
              max_one = max_one,
              max_two = max_two,
              y_label = "# of Jobs") %>%
        viz_options(
          colors = c(naccho.viz::terracotta, naccho.viz::green),
          legend_enable = TRUE,
          viz_title = "Jobs Lost and Added - Estimated Total",
          viz_caption = paste0("<b> N: </b>", unique(jobs_responses()$responses), "<br>", source_line),
          viz_subtitle = "All and Subsetted LHDs",
          tooltip = "<b> {series.name}: </b>{point.y:.0f} <br>",
          theme = naccho.viz::butterfly
        ) %>%
        viz_export()
  #     %>%
  #       highcharter::hc_tooltip(
  #         backgroundColor = "white",
  #         headerFormat = "<b>{point.x:.0f} LHDs </b> <hr class = 'tt'>",
  #         style = list(
  #           textAlign = "left",
  #           fontSize = "1.5vmin",
  #           lineHeight = "1.6vmin",# Responsive font size for readability
  #           # color = "#333333"  ,
  #           width = "auto",                               # Auto width to adapt to content
  #           maxWidth = "15vw",                           # Set maximum width for tooltip box
  #           whiteSpace = "normal" # Darker text color for contrast
  #         ),
  #         hideDelay = 2000,
  #         shadow = FALSE,
  #         outside = FALSE,                    # Keep tooltip within chart area
  #         positioner = highcharter::JS("
  #   function (labelWidth, labelHeight) {
  #     const chart = this.chart;
  #     // Position tooltip at the bottom-right corner of the chart
  #     const x = chart.plotRight + chart.plotWidth - labelWidth - 20;
  #     const y = chart.plotTop + chart.plotHeight - labelHeight - 20;
  #     return { x: x, y: y };
  #   }
  # ")
  #       )


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
    updated_inputs <- reactive({
      input$topic
      input$filter
    })

    updated_inputs_class <- reactive({
      input$topic
      input$filter
      input$classifications
    })


    # update proxy ------------------------------------------------------------
    observeEvent(updated_inputs(), {
      use_js_proxy()


      # Topic: FTEs by Category
      # View: Bar Graph
      # Filters: All LHDs
      if (input$topic == "FTE_numcat" & input$filter == "All LHDs") {
        data <- return_survey_weighted_counts("FTE_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data = data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic)


        # Topic: FTEs by Category
        # View: Bar Graph
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "FTE_numcat" & input$filter != "All LHDs") {
        data <- return_survey_weighted_counts("FTE_numcat", input$filter) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group) # provide ID for proxy

        print(series_id)

        proxy %>%
          proxy_hist_to_group(data = data,
                              data_all = NULL,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = sum(data$Responses),
                              theme = naccho.viz::basic)



        # Topic: Per Capita FTE (Histogram)
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "fte_bypop") {
        data <- create_hist(input = "fte_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))



        # Topic: Total FTE (Histogram)
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "c5q37") {
        data <- create_hist(input = "c5q37",
                            x_label = "Total FTEs")
        proxy %>%
          proxy_to_hist_total(data,
                              meta = filter_meta(),
                              responses = sum(data$y))



        # Topic: Total Employees (Histogram)
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "c5q36") {
        data <- create_hist(input = "c5q36",
                            x_label = "Total Employees")
        proxy %>%
          proxy_to_hist_total(data,
                              meta = filter_meta(),
                              responses = sum(data$y))



        # Topic: Per Capita Employee (Histogram)
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "employee_bypop") {
        data <- create_hist(input = "employee_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))



        # Topic: Employee by Category
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "employee_numcat" & input$filter == "All LHDs") {
        data <- return_survey_weighted_counts("employee_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic)


        # Topic: Employee by Category
        # View: Bar Graph
        # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "employee_numcat" & input$filter != "All LHDs") {
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
                              theme = naccho.viz::basic)



        # Topic: Jobs Standard
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "jobs_standard") {
        data <- filtered_butterfly() %>%
          dplyr::mutate(left = left_standard,
                        right = right_standard)

        max_one <- max(data$left)
        max_two <- max(data$right)

        proxy_jobs %>%
          proxy_to_bar(data,
                       y_label = "# of Jobs",
                       meta = filter_meta(),
                       max_one = max_one,
                       max_two = max_two)




        # Topic: Jobs Total
        # View: Bar Graph
        # Filters: All LHDs
      } else if (input$topic == "jobs_total") {
        data <- filtered_butterfly()

        max_one <- max(data$left)
        max_two <- max(data$right)

        proxy_jobs %>%
          proxy_to_bar(data,
                       meta = filter_meta(),
                       max_one = max_one,
                       max_two = max_two,
                       y_label = "# of Jobs")
      }
    })



    # Need to keep separate from above observe
    observeEvent(updated_inputs_class(), {
      req(length(input$classifications) >= 1) # prevents error

      if (input$topic %in% c("a", "b")) {

        selected_vars <- c(paste0(input$classifications, input$topic))
        selected_data <- get(paste0("classification_data_", input$topic))

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

        # nested
        if (length(data$x) >= 6) {
          rotate = -45
          size = "2vmin"
          overflow = "ellipsis"
        } else {
          rotate = 0
          size = "2vmin"
          overflow = "none"
        }

        # nested
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
                                theme = naccho.viz::basic)


        } else if (input$topic %in% c("a", "b") & input$filter == "All LHDs") {
          proxy %>%
            proxy_hist_to_single(data,
                                 meta = filter_meta(),
                                 responses = show_n(Responses$Responses),
                                 theme = naccho.viz::basic,
                                 rotate = rotate,
                                 size = size,
                                 overflow = overflow)
        }
      }
    })





    # View: Tables ------------------------------------------------------------------
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

      #debug
      message("input$topic =")
      print(input$topic)
      message("input$filter =")
      print(input$filter)
      message("input$classifications =")
      print(input$classifications)
      #debug
      message("topic =")
      print(topic)
      message("filtering =")
      print(filtering)
      message("classifications")
      print(classifications)

      data <- process_data(topic, filtering, classifications)

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

      print(data)


      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic)
    })

    #Line graph output-----------
    output$line <- highcharter::renderHighchart({
      req(input$view == "Line Graph")

      #add option value for function
      option <- input$topic

      filtering <- input$filter

      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL  # If input$classifications does not exist, set it to NULL
      }

      #debug
      message("input$topic =")
      print(input$topic)
      message("option variable =")
      print(option)

      message("view selected =")
      print(input$view)

      #debug
      message("input$filter =")
      print(input$filter)
      message("filtering variable =")
      print(filtering)

      #debug
      message("input$classifications =")
      print(input$classifications)
      message("classifications =")
      print(classifications)

      data <- process_line_data(option, filtering, classifications)

      View(data)

      naccho.viz::create_grouped_line(
        data = data,
        x_variable = "year",
        x_group = "legend_label",
        y_variable = "y",
        y_label = filter_meta()$y_label,
        x_label = "Year",
        select_theme = naccho.viz::minimal,
        title = filter_meta()$title,
        tooltip = "In {point.year}, {point.one}{point.two}."
      )


    })



    # right content ===========================================================

    observeEvent(input$topic, {
      if (input$topic %in% c("a", "b")) {
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
        req(!is.null(input$topic))
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
      shinyjs::hide("toggle_get_classifications")
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        req(!is.null(input$topic))
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view,
                        filter == input$filter)

        if (is.na(notes$notes)) {
          p(htmltools::HTML(
            "<b>Survey Question(s)</b>: <br>",
            notes$survey_question,
            "<br> <br> <b> Survey Response Options: </b>",
            notes$survey_response,
            "<br> <br> <b> Notes: </b>
            <br> There were more than 3,300 LHDs in the U.S.,
            but not every unit was included in the Profile study.
            For the purposes of surveying and developing national estimates of
            the LHD landscape, NACCHO designed and utilizes a methodology to account
            for the most unique individuals in the U.S. population without \"double counting.\"
            Therefore, the denominator of LHDs used for surveying and national
            estimation purposes will always be smaller than the actual number of LHDs in the U.S.,
            while still covering all local jurisdictions uniquely.
            NACCHO is in frequent contact and consultation with state and local
            officials to update the denominator. There were 2,512 LHDs included
            in the 2022 Profile study population."))

        } else {
          p(htmltools::HTML(
            "<b>Survey Question(s)</b>: <br>",
            notes$survey_question,
            "<br> <br> <b> Survey Response Options: </b>",
            notes$survey_response,
            "<br> <br> <b> Notes: </b>",
            notes$notes,
            "<br> There were more than 3,300 LHDs in the U.S.,
            but not every unit was included in the Profile study.
            For the purposes of surveying and developing national estimates of
            the LHD landscape, NACCHO designed and utilizes a methodology to account
            for the most unique individuals in the U.S. population without \"double counting.\"
            Therefore, the denominator of LHDs used for surveying and national
            estimation purposes will always be smaller than the actual number of LHDs in the U.S.,
            while still covering all local jurisdictions uniquely.
            NACCHO is in frequent contact and consultation with state and local
            officials to update the denominator. There were 2,512 LHDs included
            in the 2022 Profile study population."))
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
          dplyr::filter(field %in% input$filter)

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
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
