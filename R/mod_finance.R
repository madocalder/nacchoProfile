#' finance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_finance_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'finance_tab'",
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
                choices = finance_topics,
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
                  "Box Plot",
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
                choices = landscape_filters
              )
            )
          ),
          hr(),


          #  Sliders ----------------------------------------------------------------
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

              # Expenditures
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'expenditures\'"),
                radioButtons(
                  ns("type_exp"),
                  "",
                  choices = finance_expenditure_types,
                  inline = TRUE
                )
              ),

              # Revenues
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'revenues\'"),
                radioButtons(
                  ns("type_rev"),
                  "",
                  choices = finance_revenue_types,
                  inline = TRUE
                )
              ),

              # Jobs
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'budget_changes\'"),
                radioButtons(
                  ns("type_budget"),
                  "",
                  choices = list("Changes in Budgets" = "budget_changes",
                                 "Expected Budget Cuts" = ""),
                  inline = TRUE
                )
              )
            )
          ),

          # supports the sliding out menu
          tags$script(HTML("
              document.getElementById('finance_1-slide_button').addEventListener('click', function() {
                var slideMenu = document.querySelector('#finance_1-slideMenu');
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

            # display when Box Plot is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Box Plot\'"),
              highcharter::highchartOutput(ns("figure_box"), height = 575)
            ),

            # display when Bar Graph is selected and Budget Changes is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Bar Graph\' && input[\'", ns('topic'), "\'] != \'budget_changes\'"),
              highcharter::highchartOutput(ns("figure"), height = 575)
            ),

            # display when Budget Changes is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == \'budget_changes\'"),
              highcharter::highchartOutput(ns("figure_budget"), height = 675)
            )
          )
        ),

        # end of main content -----------------------------------------------------

        right = div(
          div(
            class = "row color-block-header",
            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("takeaway"), # key takeaway
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-3"),
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
                class = "link-no-underline action-link-3"),
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
                class = "link-no-underline action-link-3"),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),

          div(
            class = "row color-block-content",


            # toggle side content -----------------------------------------------------
            shinyjs::hidden(
              div(
                id = ns("toggle_takeaway"),
                class = "context-content",
                # uiOutput(ns("show_takeaway"))
              )
            ),

            shinyjs::hidden(
              div(
                id = ns("toggle_context"),
                class = "context-content",
                # uiOutput(ns("show_context"))
              )
            ),

            shinyjs::hidden(
              div(
                id = ns("toggle_definitions"),
                class = "context-content",
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



#' finance Server Functions
#'
#' @noRd
mod_finance_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("expenditures", "revenues")) {

        if (input$type_exp %in% c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap")) {
          updateSelectInput(session, "view",
                            choices = c("Box Plot",
                                        "Line Graph",
                                        "Table"))
        } else {
          updateSelectInput(session, "view",
                            choices = c("Bar Graph",
                                        "Table"))
        }

      } else if (input$topic == "budget_changes") {
        updateSelectInput(session, "filter",
                          choices = "All Filters")

        updateSelectInput(session, "view",
                          choices = c("Bar Graph",
                                      "Table"))
      }
    })



    # Reactive to get the inputs that I need for filtering --------------------
    get_finance_input <- function() {
      if (input$topic == "expenditures") {
        input$type_exp
      } else if (input$topic == "revenues") {
        input$type_rev
      } else if (input$topic == "budget_changes") {
        input$type_budget
      } else {
        NULL
      }
    }


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


    # main content ============================================================

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(get_finance_input()))
      meta %>% dplyr::filter(field == get_finance_input())
    })


    # figure output -----------------------------------------------------------
    # used for boxplot
    output$figure_box <- highcharter::renderHighchart({
      data <- boxplot_list[["boxplot_data_c3q15_all"]]

      # Create the boxplot
      set_box(data) %>%
        viz_options(
          legend_enable = FALSE,
          viz_title = "Estimated Total Expenditures: Mean, Median, and Quartiles",
          viz_caption = paste0("<b> N: </b>", sum(data[[1]]$responses), "<br>",
                               "<b> Note: </b> There were ", data[[1]]$total_outliers,
                               " observations above the upper limit ranging from ",
                               scales::dollar(data[[1]]$min_outlier)," to ",
                               scales::dollar(data[[1]]$max_outlier), ". <br>",
                               source_line),
          viz_subtitle = "",
          tooltip = "<b> Upper Limit: </b> ${point.high} <br>
                     <b> 75th Percentile: </b> ${point.q3} <br>
                     <b> Median: </b> ${point.median} <br>
                     <b> 25th Percentile: </b> ${point.q1} <br>
                     <b> Lower Limit: </b> ${point.low} <br>",
          tooltip_align = "left"
        ) %>%
        viz_export()
    })


    # used for bar graph
    output$figure <- highcharter::renderHighchart({
      data <- return_survey_weighted_counts("exp_total_numcat")

      set_column(data) %>%
        viz_options(
          viz_title = "Percent of LHDs by Grouped Total Expenditures",
          viz_caption = source_line,
          viz_subtitle = "Estimated Across All LHDs",
          tooltip = "<span style=\"color:{point.color}\">\u25CF</span> {point.y}% of LHDs had per capita expenditures between {point.category}."
        ) %>%
        viz_export()
    })


    # used for budget changes
    output$figure_budget <- highcharter::renderHighchart({
      req(input$topic == "budget_changes")
      data <- filtered_butterfly("budget_data")
      responses <- data %>%
        dplyr::filter(group == "All") %>%
        dplyr::summarise(n = left_n + right_n)

      set_bar(data,
              set_max = 100,
              name_one = "Cut",
              name_two = "Same or Higher",
              set_format = "{value}%") %>%
        viz_options(
          colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green),
          legend_enable = TRUE,
          legend_title = "Budget...",
          viz_title = "Changes in Budget From Prior Year",
          viz_caption = paste0("<b> N: </b>", unique(responses$n), "<br>", source_line),
          viz_subtitle = "All and Subsetted LHDs",
          tooltip = "<b> {series.name}: </b>
              {point.y}% <br>",
          theme = naccho.viz::bar_pos_neg_theme
        ) %>%
        viz_export() %>%
        highcharter::hc_tooltip(
          backgroundColor = "transparent",
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
    proxy_box <- highcharter::highchartProxy(ns("figure_box"))
    proxy_bar <- highcharter::highchartProxy(ns("figure"))



    # update the yAxis title while maintaining the line animation
    use_js_proxy <- reactive({
      shinyjs::runjs(paste0("
        var chart = $('#finance_1-figure').highcharts();
        chart.yAxis[0].update({
          title: {
            text: '", filter_meta()$y_label, "',
            style: { color: '#333333' }
          }
        }, true, { duration: 1000 });
    "))
    })



    # inputs for each topic ---------------------------------------------------
    inputs_exp <- reactive({
      input$topic
      input$filter
      input$type_exp
    })

    inputs_rev <- reactive({
      input$topic
      input$filter
      input$type_rev
    })

    inputs_budget <- reactive({
      input$topic
      input$filter
      input$type_budget
    })



    # update proxy ------------------------------------------------------------
    # for Expenditures --------------------------------------------------------
    observeEvent(inputs_exp(), {
      req(input$topic == "expenditures")
      use_js_proxy()

      if (input$type_exp %in% c("c3q15", "totexp_percap") & input$filter == "All LHDs" & input$view == "Box Plot") {
        get_boxplot_list <- paste0("boxplot_data_", input$type_exp, "_all")
        data <- boxplot_list[get_boxplot_list]

        names(data) <- NULL
        data[[1]] <- data[[1]][[1]]

        proxy_box %>%
          proxy_to_box(data,
                       meta = filter_meta())


      } else if (input$type_exp %in% c("c3q15", "totexp_percap") & input$filter != "All LHDs" & input$view == "Box Plot") {
        get_boxplot_list <- paste0("boxplot_data_", input$type_exp, "_", input$filter)
        data <- boxplot_list[get_boxplot_list]

        names(data) <- NULL
        data <- data[[1]]

        proxy_box %>%
          proxy_to_group_box(data,
                       meta = filter_meta())

      } else if (input$type_exp %in% c("exp_total_numcat", "exp_percap_numcat") & input$filter == "All LHDs") {

        data <- return_survey_weighted_counts(input$type_exp) %>%
          dplyr::mutate(x_label = NA)
        proxy_bar %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)


      } else if (input$type_exp %in% c("exp_total_numcat", "exp_percap_numcat") & input$filter != "All LHDs") {
        data <- return_survey_weighted_counts(input$type_exp, input$filter) %>%
          dplyr::mutate(x_label = NA)

        data_all <- return_survey_weighted_counts(input$type_exp) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group) # provide ID for proxy

        proxy_bar %>%
          proxy_hist_to_group(data = data,
                              data_all = data_all,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = sum(data$Responses),
                              theme = naccho.viz::basic_theme)
      }
    })


    # for Revenues --------------------------------------------------------
    observeEvent(inputs_rev(), {
      req(input$topic == "revenues")
      use_js_proxy()

      if (input$type_rev %in% c("non_weighted_revenue_total", "non_weighted_revenue_percap") & input$filter == "All LHDs" & input$view == "Box Plot") {
        get_boxplot_list <- paste0("boxplot_data_", input$type_rev, "_all")
        data <- boxplot_list[get_boxplot_list]

        names(data) <- NULL
        data[[1]] <- data[[1]][[1]]

        proxy_box %>%
          proxy_to_box(data,
                       meta = filter_meta())


      } else if (input$type_rev %in% c("non_weighted_revenue_total", "non_weighted_revenue_percap") & input$filter != "All LHDs" & input$view == "Box Plot") {
        get_boxplot_list <- paste0("boxplot_data_", input$type_rev, "_", input$filter)
        data <- boxplot_list[get_boxplot_list]

        names(data) <- NULL
        data <- data[[1]]

        proxy_box %>%
          proxy_to_group_box(data,
                             meta = filter_meta())

      } else if (input$type_rev %in% c("rev_total_numcat", "rev_percap_numcat") & input$filter == "All LHDs") {

        data <- return_survey_weighted_counts(input$type_rev) %>%
          dplyr::mutate(x_label = NA)

        proxy_bar %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)


      } else if (input$type_rev %in% c("rev_total_numcat", "rev_percap_numcat") & input$filter != "All LHDs") {

        data <- return_survey_weighted_counts(input$type_rev, input$filter) %>%
          dplyr::mutate(x_label = NA)

        data_all <- return_survey_weighted_counts(input$type_rev) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group) # provide ID for proxy

        proxy_bar %>%
          proxy_hist_to_group(data = data,
                              data_all = data_all,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = sum(data$Responses),
                              theme = naccho.viz::basic_theme)
      }
    })


    # for budget changes -----------------------------------------------------
    observeEvent(inputs_budget(), {
      req(input$topic == "budget_changes")
      use_js_proxy()

      # if (input$type_budget %in% c("non_weighted_revenue_total", "non_weighted_revenue_percap")) {
      #
      # } else if {
      #
      # }

    })

  })
}

## To be copied in the UI
# mod_finance_ui("finance_1")

## To be copied in the server
# mod_finance_server("finance_1")
