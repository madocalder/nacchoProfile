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
            align = "center",
            div(
              class = "col-sm-6 min-height-400",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = finance_topics,
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


          # viz toggles here --------------------------------------------------
          div(
            class = "viz-view",
            shinyWidgets::radioGroupButtons(
              ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-column"></i>'), # Chart icon
                HTML('<i class="fas fa-table"></i>') # Map icon
              ),
              choiceValues = c("Histogram", "Table")
            )
          ),


          # add main page content here ----------------------------------------------
          div(
            class = "viz-content",

            # display when Bar Graph is selected and Table is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Histogram\' && input[\'", ns('view'), "\'] != \'Table\'"),
              div(
                highcharter::highchartOutput(ns("figure_hist"), height = "100%"),
                style = "height: 65vh; width: 50vw;")
            ),

            # display when Bar Graph is selected, Budget Changes is not selected, and Table is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Bar Graph\' && input[\'", ns('topic'), "\'] != \'budget_changes\' && input[\'", ns('view'), "\'] != \'Table\'"),
              div(
                highcharter::highchartOutput(ns("figure"), height = "100%"),
                style = "height: 65vh; width: 50vw;")
            ),

            # display when Budget Changes is selected and Table is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == \'budget_changes\' && input[\'", ns('view'), "\'] != \'Table\'"),
              div(
                highcharter::highchartOutput(ns("figure_budget"), height = "100%"),
                  style = "height: 68vh; width: 50vw;")
            ),

            # display when Tree Map is selected, non-weighted revenue total is selected, and Table is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Tree Map\' && input[\'", ns('topic'), "\'] == \'non_weighted_revenue_total\' && input[\'", ns('view'), "\'] != \'Table\'"),
              div(
                highcharter::highchartOutput(ns("figure_tree"), height = "100%"),
                style = "height: 65vh; width: 50vw;")
            ),

            # Table view
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),

              div(
                uiOutput(ns("table_title")),
                  reactable::reactableOutput("data_table", height = "auto"),
                br(),
                HTML('<div style="font-size: 2vmin;">', source_line,'
                   <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),

                style = "height: 65vh; width: 50vw;")

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
        ),

        right_button = right_button
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


    # inputs for each topic ---------------------------------------------------
    updated_inputs <- reactive({
      input$topic
      input$filter
    }) %>% debounce(100)


    #table titles--------------
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      req(input$topic)

      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(subset(meta, field == input$topic)$title)

      # Render the HTML
      p(title_values, style = "font-size: 3vmin; font-weight: bold !important; text-align: center;")
    })




    # update inputs based on selections ---------------------------------------
    observe({

      if (input$topic == "non_weighted_revenue_total") {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-column"></i>'),
                                                HTML('<i class="fas fa-chart-tree-map"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Histogram", "Tree Map", "Table"))

      } else if (input$topic %in% c("c3q15", "totexp_percap", "non_weighted_revenue_percap")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-column"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Histogram", "Table"))


      } else if (input$topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"))
      }
    })

    observe({
      if (input$topic == "budget_changes") {
        updateSelectInput(session, "filter",
                          choices = "All LHDs")

        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"))

      } else if (input$topic %in% c("c3q15", "totexp_percap",
                                    "non_weighted_revenue_percap",
                                    "non_weighted_revenue_total") &
                 input$view == "Histogram") {

        updateSelectInput(session, "filter",
                          choices = "All LHDs")

     } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)
      }
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


    # main content ============================================================

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(input$topic))
      meta %>% dplyr::filter(field == input$topic)
    })


    # figure output -----------------------------------------------------------

    output$figure_hist <- highcharter::renderHighchart({

      data <- create_hist(input = "c3q15",
                          x_label = "Total Expenditures $")

      data2 <- nacchoProfile::return_survey_weighted_statistics("c3q15")

      x_mean <- data2$Mean

      mean_bin <- data %>%
        dplyr::filter(low_bin <= x_mean & high_bin >= x_mean) %>%
        dplyr::select(x) %>%
        dplyr::pull()

      x_max <- mean_bin*5


      set_hist(data, x_label = unique(data$x_label), y_label = "# of LHDs", x_max = x_max) %>%
        viz_options(
          legend_enable = FALSE,
          viz_title = "The Distribution of Estimated Total Expenditures Across LHDs",
          viz_caption = paste0("<b> N: </b>", sum(data$y), "<br>", source_line),
          viz_subtitle = unique(data$subtitle),
          tooltip = "{point.y:.0f} LHDs spent between ${point.low_bin} and ${point.high_bin} in the last fiscal year reported."
        ) %>%
        highcharter::hc_xAxis(
          plotLines = list(
            list(
              width = 2,
              color = naccho.viz::terracotta,
              value = mean_bin,  # Position of the line at the mean
              label = list(
                text = paste0("Mean Total Expenditures: <br>", scales::label_currency(prefix = "$")(x_mean)),     # Label for the line,
                rotation = 0,
                align = "left",   # Label alignment
                style = list(
                  fontWeight = "bold"
                )))
          )) %>%
        viz_export()
      # data <- boxplot_list[["boxplot_data_c3q15_all"]]
      #
      #
      #
      # # Create the boxplot
      # set_box(data) %>%
      #   viz_options(
      #     legend_enable = FALSE,
      #     viz_title = "Estimated Total Expenditures: Mean, Median, and Quartiles",
      #     viz_caption = paste0("<b> N: </b>", sum(data[[1]]$responses), "<br>",
      #                          "<b> Note: </b> There were ", data[[1]]$total_outliers,
      #                          " observations above the upper limit ranging from ",
      #                          scales::dollar(data[[1]]$min_outlier)," to ",
      #                          scales::dollar(data[[1]]$max_outlier), ". <br>",
      #                          source_line),
      #     viz_subtitle = "",
      #     tooltip = "<b> Upper Limit: </b> ${point.high:.0f} <br>
      #                <b> 75th Percentile: </b> ${point.q3:.0f} <br>
      #                <b> Median: </b> ${point.median:.0f} <br>
      #                <b> 25th Percentile: </b> ${point.q1:.0f} <br>
      #                <b> Lower Limit: </b> ${point.low:.0f} <br>",
      #     tooltip_align = "left"
      #   ) %>%
      #   viz_export()
    })


    # used for bar graph
    output$figure <- highcharter::renderHighchart({
      data <- return_survey_weighted_counts("exp_total_numcat")

      set_column(data) %>%
        viz_options(
          viz_title = "Percent of LHDs by Grouped Total Expenditures",
          viz_caption = source_line,
          viz_subtitle = "Estimated Across All LHDs",
          tooltip = "<span style=\"color:{point.color}\">\u25CF</span>
                    {point.y:.0f}% of LHDs had per capita expenditures between
                    {point.category}.",
          theme = naccho.viz::basic
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

      max_one <- max(data$left)
      max_two <- max(data$right)

      set_bar(data,
              y_label = NA,
              max_one = max_one,
              max_two = max_two,
              name_one = "Cut",
              name_two = "Same or Higher",
              set_format = "{value}%") %>%
        viz_options(
          colors = c(naccho.viz::terracotta, naccho.viz::green),
          legend_enable = TRUE,
          legend_title = "Budget...",
          viz_title = "Changes in Budget From Prior Year",
          viz_caption = paste0("<b> N: </b>", unique(responses$n), "<br>", source_line),
          viz_subtitle = "All and Subsetted LHDs",
          tooltip = "<b> {series.name}: </b>
              {point.y:.0f}% <br>"
          # ,
          # theme = naccho.viz::butterfly
        ) %>%
        viz_export()%>%
        highcharter::hc_tooltip(
          headerFormat = "<b>{point.x:.0f} LHDs </b> <hr class = 'tt'>",
          style = list(
            textAlign = "left"
          ),
          hideDelay = 2000,
          shadow = FALSE
        )
    })


    # used for the tree map
    output$figure_tree <- highcharter::renderHighchart({
      req(input$topic == "non_weighted_revenue_total")

      if (input$filter == "All LHDs") {

        data <- revenue_percentages %>%
          dplyr::rename(name = revenue_type) %>%
          dplyr::mutate(id = paste(name, sep = " "),
                        value = round(percentage*100,1)) %>%
          dplyr::select(id, name, value)

        add_info <- data.frame(
          color = naccho.viz::green
        )

        data <- cbind(data, add_info)

        # Create the grouped treemap with specified colors
        highcharter::highchart() %>%
          highcharter::hc_add_series(
            type = "treemap",
            layoutAlgorithm = "squarified",
            data = highcharter::list_parse(data),
            allowDrillToNode = TRUE, # enables drill down
            borderColor = "#FFFFFF",
            borderWidth = 2
          ) %>%
          highcharter::hc_plotOptions(
            series = list(
              dataLabels = list(
                enabled = TRUE,
                color = "black",
                align = "center",
                verticalAlign = "middle",
                allowOverlap = TRUE,
                overflow = "allow",
                format = '<b>{point.name}</b> <br> {point.value:.0f}%',
                style = list(
                  fontSize = "1.8vmin",
                  fontFamily = "Heebo",
                  fontWeight = 300,
                  textOutline = "none"
                )
              )
            )
          ) %>%
          viz_options(viz_title = "Revenue Sources as a Share of Total Revenue",
                      viz_subtitle = "Estimated Across All LHDs",
                      viz_caption = source_line,
                      caption_align = "center",
                      tooltip = "{point.value:.0f}% of all LHD revenue came from <b>{point.name} sources.</b>",
                      legend_enable = TRUE)

      } else {

        data <- revenue_percentages_filter %>%
          dplyr::filter(filter == input$filter) %>%
          dplyr::rename(name = revenue_type,
                        parent = category) %>%
          dplyr::mutate(id = paste(name, parent, sep = " "),
                        value = round(percentage*100,1)) %>%
          dplyr::select(all_off(id, name, parent, value, subtitle))

        id_values <- unique(data$parent)

        add_info <- data.frame(
          id = id_values,
          name = id_values,
          parent = "Overall",
          value = NA,
          subtitle = NA
        )

        # bind data
        data <- rbind(data, add_info)


        # set colors for each parent group
        set_colors <- c(
          naccho.viz::teal,
          naccho.viz::pink,
          naccho.viz::terracotta,
          naccho.viz::green
        )

        # map colors and ids
        color_mapping <- setNames(set_colors[1:length(id_values)], id_values)

        # final data
        data <- data %>%
          mutate(
            color = color_mapping[id]
          )

        # Create the grouped treemap with specified colors
        highcharter::highchart() %>%
          highcharter::hc_add_series(
            type = "treemap",
            layoutAlgorithm = "squarified",
            data = highcharter::list_parse(data),
            allowDrillToNode = TRUE, # enables drill down
            borderColor = "#FFFFFF",
            borderWidth = 2,
            levels = list(
              list(
                level = 1,
                dataLabels = list(
                  enabled = TRUE,
                  format = '<b>{point.name}</b>',
                  align = 'left',
                  verticalAlign = 'top',
                  style = list(
                    fontSize = '2vmin',
                    fontWeight = 'bold'
                  )
                )
              ),
              list(
                level = 2
              )
            )
          ) %>%
          highcharter::hc_plotOptions(
            series = list(
              dataLabels = list(
                enabled = TRUE,
                color = "black",
                align = "center",
                verticalAlign = "middle",
                allowOverlap = TRUE,
                overflow = "allow",
                format = '<b>{point.name}</b> <br> {point.value:.0f}%',
                style = list(
                  fontSize = "2vmin",
                  fontFamily = "Heebo",
                  fontWeight = 300,
                  textOutline = "none"
                )
              )
            )
          ) %>%
          viz_options(viz_title = "Revenue Sources as a Share of Total Revenue",
                      viz_subtitle = unique(data$subtitle),
                      viz_caption = source_line,
                      caption_align = "center",
                      tooltip = "{point.value:.0f}% of all revenue in {point.parent} LHDs came from <b>{point.name} sources.</b>",
                      legend_enable = TRUE) %>%
          highcharter::hc_tooltip(
            useHTML = TRUE,
            formatter =  highcharter::JS(
              "function() {
              if (this.point.node && this.point.node.level === 1) {
                return '<b>' + this.point.name + '</b>';
              } else {
          return this.point.value:.0f + '% of all revenue in ' + this.point.parent + ' LHDs came from <b>' + this.point.name + ' sources.</b>';
              }
            }"
            )
          )
      }
    })

    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    proxy_hist <- highcharter::highchartProxy(ns("figure_hist"))
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



    # update proxy ------------------------------------------------------------
    observeEvent(updated_inputs(), {
      use_js_proxy()

      if (input$topic %in% c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap") & input$filter == "All LHDs" & input$view == "Histogram") {

        data <- create_hist(input = input$topic,
                            x_label = "$")

        data2 <- nacchoProfile::return_survey_weighted_statistics(input$topic)


        x_mean <- data2$Mean


        mean_bin <- data %>%
          dplyr::filter(low_bin <= x_mean & high_bin >= x_mean) %>%
          dplyr::select(x) %>%
          dplyr::pull()

        if (input$topic %in% c("c3q15", "non_weighted_revenue_total")) {
          x_max <- mean_bin*5
        } else {
        x_max <- mean_bin*8
        }

        proxy_hist %>%
          proxy_to_hist_total(data,
                       x_max = x_max,
                       meta = filter_meta())%>%
          highcharter::hcpxy_update(
            xAxis = list(
              # max = x_max,
              # label = list(autoRotation = TRUE),
              plotLines = list(
                list(
                  width = 2,
                  color = naccho.viz::terracotta,
                  value = mean_bin,  # Position of the line at the mean
                  label = list(
                    text = paste0("Mean Total Expenditures: <br>", scales::label_currency(prefix = "$")(x_mean)),     # Label for the line,
                    rotation = 0,
                    align = "left",   # Label alignment
                    style = list(
                      fontWeight = "bold"
                    )))
              ))
          )

#
#       } else if (input$topic %in% c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap") & input$filter != "All LHDs" & input$view == "Histogram") {
#         get_boxplot_list <- paste0("boxplot_data_", input$topic, "_", input$filter)
#         data <- boxplot_list[get_boxplot_list]
#
#         names(data) <- NULL
#         data <- data[[1]]
#
#         proxy_hist %>%
#           proxy_to_group_box(data,
#                        meta = filter_meta())
#


      } else if (input$topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & input$filter == "All LHDs" & input$view == "Bar Graph") {
        data <- return_survey_weighted_counts(input$topic) %>%
          dplyr::mutate(x_label = NA)

        proxy_bar %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic)



      } else if (input$topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & input$filter != "All LHDs" & input$view == "Bar Graph") {
        data <- return_survey_weighted_counts(input$topic, input$filter) %>%
          dplyr::mutate(x_label = NA)

        data_all <- return_survey_weighted_counts(input$topic) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group) # provide ID for proxy

        proxy_bar %>%
          proxy_hist_to_group(data = data,
                              data_all = data_all,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = sum(data$Responses),
                              theme = naccho.viz::basic)
      }
    })

# for Tables -------
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

      # print(data)

      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic)
    }) #end of tables output code



  })
}

## To be copied in the UI
# mod_finance_ui("finance_1")

## To be copied in the server
# mod_finance_server("finance_1")
