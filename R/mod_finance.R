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
          class = "main-content",
          openMenu(),

          div(
            class = "topic-select",

            selectInput(
              inputId = ns("topic"),
              strong("Topic"),
              choices = finance_topics,
              selectize = TRUE,
              selected = finance_topics[1]
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
              inputId = ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-line"></i>'), # Chart icon
                HTML('<i class="fas fa-table"></i>') # Map icon
              ),
              choiceValues = c("Line Graph", "Table"),
              selected = "Table"
            )
          ),


          # add main page content here ----------------------------------------------
          div(
            class = "viz-content",

            # Line Graph View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Line Graph\'"),

              highcharter::highchartOutput(ns("figure_line"), height = "100%")

            ),

            # Bar Graph View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Bar Graph\'"),
              # div(
              highcharter::highchartOutput(ns("figure"), height = "100%"),
              style = "height: 100%"
            ),

            # Tree Map View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Tree Map\'"),
              # div(
              highcharter::highchartOutput(ns("figure_tree"), height = "100%"),
              style = "height: 100%"
            ),

            # Table View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), '\'] == \'Table\'
                                  && input[\'', ns('view'), '\'] != \'Bar Graph\'
                                && input[\'', ns('view'), '\'] != \'Line Graph\''),
              div( class = "table-grid",

                   div(class = "table-title-grid",

                       uiOutput(ns("table_title")),
                       csvDownloadButton(ns("data_table"), filename = "financeNACCHO.csv")
                   ),

                   reactable::reactableOutput(ns("data_table"),
                                              height = "100%",
                                              width = "auto"
                   ),

                   HTML('<div style="font-size: calc(5px + .2em + .8vmin);">', source_line, '<br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>')

              ),
              style = "height: 100%"
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
                              style = "font-size: calc(28px + .2em); text-align: center;"),
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
                              style = "font-size: calc(28px + .2em); text-align: center;"),
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
                              style = "font-size: calc(28px + .2em); text-align: center;"),
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
        ),

        right_button = right_button,
        left_width = 25,
        right_width = 25,
        center_width = 50
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


    #table title output
    output$table_title <- renderUI({
      req(filter_meta())
      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(filter_meta()$title)

      # Render the HTML
      p(title_values, class = "table-title")
    })



    # update inputs based on selections ---------------------------------------
    observe({

      req(input$topic, input$view)

      topic <- input$topic
      selected_view <- input$view

      if (input$topic == "rev_total") {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-chart-tree-map"></i>'),
                                                HTML('<i class="fas fa-table"></i>')

                                              ),
                                              choiceValues = c("Bar Graph", "Tree Map", "Table"),
                                              selected = dplyr::case_when(
                                                selected_view %in% c("Bar Graph", "Tree Map", "Table") ~ selected_view,
                                                TRUE ~ "Tree Map"))

      } else if (input$topic %in% c("totexp_percap", "budget_changes")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Line Graph", "Table"),
                                              selected = dplyr::case_when(
                                                selected_view %in% c("Line Graph", "Table") ~ selected_view,
                                                TRUE ~ "Table"))


      } else if (input$topic %in% c("c3q15", "exp_total_numcat",
                                    "rev_percap")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"),
                                              selected = dplyr::case_when(
                                                selected_view %in% c("Bar Graph", "Table") ~ selected_view,
                                                TRUE ~ "Bar Graph"))

      }
      # else if (input$topic == "budget_changes"){
      #
      #   shinyWidgets::updateRadioGroupButtons(session, "view",
      #                                         choiceNames = list(
      #                                           HTML('<i class="fas fa-chart-line"></i>'),
      #                                           HTML('<i class="fas fa-chart-simple"></i>'),
      #                                           HTML('<i class="fas fa-table"></i>')
      #                                         ),
      #                                         choiceValues = c("Line Graph", "Bar Graph", "Table"),
      #                                         selected = dplyr::case_when(
      #                                           selected_view %in%
      #                                             c("Line Graph", "Bar Graph", "Table") ~ selected_view,
      #                                           TRUE ~ "Bar Graph"))
      # }
    })

    observe({
      req(!is.null(input$topic), !is.null(input$view))

      topic <- input$topic
      selected_view <- input$view
      selected_filter <- input$filter

      if (selected_view == "Line Graph" &
          topic %in% c("totexp_percap", "budget_changes")) {

        updateSelectInput(session, "filter",
                          choices = landscape_filters[1],
                          selected = "All LHDs"
        )

      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters,
                          selected = selected_filter)
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
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      meta %>%
        dplyr::filter(field == input$topic,
                      stringr::str_detect(view, input$view),
                      stringr::str_detect(filter, input$filter))

    })





    # figure output -----------------------------------------------------------

    output$figure_line <- highcharter::renderHighchart({
      req(!is.null(input$topic), !is.null(input$filter))
      req(filter_meta())
      req(input$view == "Line Graph")
      req(input$topic == "totexp_percap")
      req(input$filter == "All LHDs")

      topic <- input$topic
      filtering <- input$filter

      # req(updated_inputs())
      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL
      }

      data <- process_line_data(topic, filtering, classifications)%>%
        dplyr::mutate(x_label = NA,
                      legend_label = category)

      data$year <- factor(data$year)

      View(data)

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
      ) %>%
        viz_options(viz_caption = paste(
          "<b> N: </b>", show_n(data$Responses),
          "<br>", source_line
        ),
        tooltip = filter_meta()$tooltip,
        ) %>%
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

    proxy_line <- highcharter::highchartProxy(ns("figure_line"))

    observe({
      req(!is.null(input$topic), !is.null(input$filter))
      req(filter_meta())
      req(input$view == "Line Graph")
      req(input$topic %in% c("totexp_percap", "budget_changes"))
      req(input$filter == "All LHDs")

      topic <- input$topic
      filtering <- input$filter

      # req(updated_inputs())
      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL
      }

      data <- process_line_data(topic, filtering, classifications)%>%
        dplyr::mutate(x_label = NA,
                      legend_label = category)

      proxy_line %>%
        highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_line_series") %>%
        highcharter::hcpxy_remove_series(id = "butterfly_series")




      if (nrow(data) > 0) {
        proxy_line %>%
          highcharter::hcpxy_remove_series(id = "filtered_line_series") %>%
          highcharter::hcpxy_add_series(
            id = "all_lhds_line_series",
            name = input$filter,
            data = data,
            type = "line"
          )
      } else {
        message("No data for line graph update.")
      }


    })


    process_bar_chart_data <- reactive({

      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))

      req(input$topic %in% c("c3q15", "rev_percap", "rev_total", "budget_changes",
                             "exp_total_numcat"))
      req(input$view == "Bar Graph")

      filtering <- input$filter
      topic <- input$topic

      data <- if (filtering == "All LHDs") {

        if (topic %in%  c("c3q15", "rev_percap", "rev_total")) {

          data <- return_survey_weighted_statistics(topic)%>%
            dplyr::select(Responses, y, group, Quantile_q50)%>%
            dplyr::rename(Median = Quantile_q50,
                          Mean = y)%>%
            tidyr::pivot_longer(cols = c("Mean", "Median"),
                                names_to = "category",
                                values_to = "y")

          if (topic == "rev_total") {

            data <- data %>%
              dplyr::mutate(x = dplyr::case_when(category == "Mean" ~ 0,
                                                 TRUE ~ 1),
                            y = dplyr::case_when(
                              (y >= 10000000) ~ round_up(y, -5),
                              (9999999 > y & y >= 100000) ~ round_up(y, -4),
                              (99999 > y & y >= 10000) ~  round_up(y, -3),
                              TRUE ~ round_up(y, 0)
                            )
              )
          } else if (topic == "c3q15") {
            data <- data %>%
              dplyr::mutate(x = as.numeric(factor(category, levels = unique(category))) - 1,
                            y = dplyr::case_when(
                              (y >= 10000000) ~ round_up(y, -5),
                              (9999999 > y & y >= 100000) ~ round_up(y, -4),
                              (99999 > y & y >= 10000) ~  round_up(y, -3),
                              TRUE ~ round_up(y, 0)
                            )  )

          } else {
            data <- data %>%
              dplyr::mutate(x = as.numeric(factor(category, levels = unique(category))) - 1,
                            y = dplyr::case_when(
                              (y >= 10000000) ~ round_up(y, -5),
                              (9999999 > y & y >= 100000) ~ round_up(y, -4),
                              (99999 > y & y >= 10000) ~  round_up(y, -3),
                              TRUE ~ round_up(y, 0)
                            )
              )
          }



        } else if (topic %in% c("exp_total_numcat", "budget_changes")) {

          data <- return_survey_weighted_counts(topic) %>%
            dplyr::mutate(Responses = sum(Responses),
                          # category_old = category,
                          # category = group,
                          # group = category_old,
                          x = as.numeric(factor(category, levels = unique(category))) - 1
            )


        } else {
          data <- return_survey_weighted_counts(topic)%>%
            dplyr::mutate(category = group
            )
        }



      } else {
        if (topic %in%  c("c3q15", "rev_percap", "rev_total")) {

          data <-  return_survey_weighted_statistics(topic, filtering)%>%
            dplyr::select(Responses, y, group, category, Quantile_q50)%>%
            dplyr::rename(Median = Quantile_q50,
                          Mean = y)%>%
            dplyr::mutate(Responses = sum(Responses),
                          group = category) %>%
            dplyr::select(Responses, Mean, group, Median)%>%
            tidyr::pivot_longer(cols = c("Mean", "Median"),
                                names_to = "category",
                                values_to = "y") %>%



            dplyr::mutate(x = as.numeric(factor(category, levels = unique(category))) - 1,
                          y = dplyr::case_when(
                            (y >= 10000000) ~ round_up(y, -5),
                            (9999999 > y & y >= 100000) ~ round_up(y, -4),
                            (99999 > y & y >= 10000) ~  round_up(y, -3),
                            TRUE ~ round_up(y, 0)
                          )
            )

        } else if (topic %in% c("exp_total_numcat", "budget_changes")) {

          data <- return_survey_weighted_counts(topic, filtering)%>%
            dplyr::mutate(Responses = sum(Responses)) %>%
              dplyr::mutate(x = as.numeric(factor(category, levels = unique(category))) - 1)

        } else {
          data <- return_survey_weighted_counts(topic, filtering)%>%
            dplyr::mutate(category = group
            )
        }

      }

      data
    })



    # used for bar graph
    output$figure <- highcharter::renderHighchart({


      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      req(filter_meta())
      req(process_bar_chart_data())
      req(input$topic %in% c("c3q15", "rev_percap", "rev_total", "budget_changes",
                             "exp_total_numcat"))
      req(input$view == "Bar Graph")

      data <- process_bar_chart_data()

      # set_column(data) %>%
      #   viz_options(
      #     viz_title = "% of LHDs by Grouped Total Expenditures",
      #     viz_caption = source_line,
      #     viz_subtitle = "Estimated Across All LHDs",
      #     tooltip = "<span style=\"color:{point.color}\">\u25CF</span>
      #               {point.y:.0f}% of LHDs had per capita expenditures between
      #               {point.category}.",
      #     theme = naccho.viz::basic
      #   ) %>%
      #   viz_export()

      bar_chart <- highcharter::highchart()

      View(data)

      bar_chart <- set_column(data,
                              y_label = filter_meta()$y_label,
                              y_format = filter_meta()$y_format,
                              data_label = filter_meta()$data_label) %>%
        viz_options(
          viz_title = filter_meta()$title,
          viz_caption = paste("<b> N: </b>", show_n(data$Responses), "<br>", source_line),
          tooltip = filter_meta()$tooltip,
          theme = naccho.viz::basic
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



    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    # proxy_hist <- highcharter::highchartProxy(ns("figure_hist"))
    proxy_bar <- highcharter::highchartProxy(ns("figure"))

    observe({

      req(input$topic %in% c("c3q15", "rev_percap", "rev_total", "budget_changes",
                             "exp_total_numcat"),
          input$filter,
          input$view == "Bar Graph")
      req(process_bar_chart_data())
      req(filter_meta())



      # Remove all old series (to prevent overlap)
      proxy_bar %>%
        highcharter::hcpxy_remove_series(id = "filtered_line_series") %>%
        highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "butterfly_series")



      data <- process_bar_chart_data()


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


    # used for budget changes
    output$figure_budget <- highcharter::renderHighchart({
      req(input$topic == "budget_changes")

      data <- filtered_butterfly("budget_data")

      responses <-  master_data %>%
        group_by(c10q301) %>%
        dplyr::filter(c10q301 %in% c(1, 2, 3)) %>%
        summarise(count = n(),
                  n = sum(count))


      max_one <- max(data$left)
      max_two <- max(data$right)

      message("max_one = ", max_one, " max_two = ", max_two)

      set_bar(data,
              y_label = NA,
              max_one = max_one,
              max_two = max_two,
              name_one = "Cut",
              name_two = "Same or Higher",
              y_format = "{value}%") %>%
        viz_options(
          colors = c(naccho.viz::terracotta, naccho.viz::green),
          legend_enable = TRUE,
          legend_title =  "% of Budgets that were...",
          viz_title = "Changes in Budget From Prior Year",
          viz_caption = paste0("<b> N: </b>", sum(responses$n), "<br>", source_line),
          viz_subtitle = "All and Subsetted LHDs",
          tooltip = "<b> {series.name}: </b>
              {point.y:.0f}% <br>",
          theme = naccho.viz::butterfly
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
      req(input$topic == "rev_total")

      if (input$filter == "All LHDs") {

        data <- revenue_percentages %>%
          dplyr::rename(name = revenue_type) %>%
          dplyr::mutate(id = paste(name, sep = " "),
                        value = round_up(percentage*100, 1)) %>%
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
                # format = '<b>{point.name}</b> <br> {point.value:.0f}%',
                style = list
                #         ,
                #         formatter = JS(
                #           "
                # function () {
                #   let roundedValue;
                #   if (this.y > 0 && this.y < 1) {
                #     roundedValue = this.y.toFixed(1);
                #   } else {
                #     roundedValue = this.y.toFixed(0);
                #   }
                #   return '<b>' + this.point.name + '</b><br>' + roundedValue + '%';
                # }
                # "
                #         )
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
          dplyr::rename(name = revenue_type, parent = category) %>%
          dplyr::mutate(
            id = paste(name, parent, sep = " "),
            value = round_up(percentage * 100, 1)
          ) %>%
          dplyr::select(id, name, parent, value, subtitle)

        # Create parent nodes with children explicitly defined
        id_values <- unique(data$parent)

        parent_nodes <- data.frame(
          id = id_values,
          name = id_values,
          parent = "Overall",
          value = NA,
          subtitle = NA
        )

        # Final hierarchical data
        data <- rbind(data, parent_nodes) %>%
          arrange(parent, id)

        # Map colors to parent nodes
        set_colors <- c(
          naccho.viz::teal,
          naccho.viz::pink,
          naccho.viz::terracotta,
          naccho.viz::green
        )

        color_mapping <- setNames(set_colors[1:length(id_values)], id_values)

        data <- data %>%
          mutate(color = ifelse(!is.na(parent) & parent == "Overall", color_mapping[id], NA))


        View(data)

        # Create the grouped treemap with specified colors
        highcharter::highchart() %>%
          highcharter::hc_add_series(
            type = "treemap",
            layoutAlgorithm = "squarified",
            data = highcharter::list_parse(data),
            allowDrillToNode = TRUE,  # Enables drill down
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
                    fontSize = '1.2em',
                    fontWeight = 'bold'
                  )
                )
              ),
              list(
                level = 2,
                dataLabels = list(
                  enabled = TRUE,
                  format = '<b>{point.name}</b> <br> {point.value:.0f}%',
                  align = 'center',
                  verticalAlign = 'middle',
                  style = list(
                    fontSize = '1em'
                  )
                )
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
                # format = '<b>{point.name}</b> <br> {point.value:.0f}%',
                style = list(
                  fontSize = "1.4em",
                  fontFamily = "Heebo",
                  fontWeight = 300,
                  textOutline = "none"
                )
                #         ,
                #         formatter = JS(
                #           "
                # function () {
                #   let roundedValue;
                #   if (this.y > 0 && this.y < 1) {
                #     roundedValue = this.y.toFixed(1);
                #   } else {
                #     roundedValue = this.y.toFixed(0);
                #   }
                #   return '<b>' + this.point.name + '</b><br>' + roundedValue + '%';
                # }
                # "
                #         )
              )
            )
          ) %>%
          viz_options(viz_title = "Revenue Sources as a Share of Total Revenue",
                      viz_subtitle = unique(data$subtitle) ,
                      viz_caption = source_line,
                      caption_align = "center",
                      tooltip = "",
                      legend_enable = TRUE) %>%
          highcharter::hc_tooltip(
            useHTML = TRUE,
            formatter = highcharter::JS(
              "function() {
      if (this.point) {
        // Check if the node is a parent
        if (this.point.node && this.point.node.level === 1) {
          return `<b>${this.point.name}</b>: Click to drill down.`;
        } else {
          // Show child node details
          return `${this.point.value.toFixed(0)}% of revenue in <b>${this.point.parent}</b> LHDs came from: <b>${this.point.name}</b> sources`;
        }
      }
    }"
            )
          )



      }
    })


    # update the yAxis title while maintaining the line animation
    # use_js_proxy <- reactive({
    #   shinyjs::runjs(paste0("
    #     var chart = $('#finance_1-figure').highcharts();
    #     chart.yAxis[0].update({
    #       title: {
    #         text: '", filter_meta()$y_label, "',
    #         style: { color: '#333333' }
    #       }
    #     }, true, { duration: 1000 });
    # "))
    # })



    # update proxy ------------------------------------------------------------
    # observeEvent(updated_inputs(), {
    #   use_js_proxy()
    #
    #   if (input$topic %in% c("c3q15", "totexp_percap", "rev_total", "rev_percap") & input$filter == "All LHDs" & input$view == "Line Graph") {
    #
    #
    #
    #
    #   } else if
    #     proxy_bar %>%
    #       proxy_to_single(data,
    #                            meta = filter_meta(),
    #                            theme = naccho.viz::basic)
    #
    #
    #
    #   } else if (input$topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & input$filter != "All LHDs" & input$view == "Bar Graph") {
    #     data <- return_survey_weighted_counts(input$topic, input$filter) %>%
    #       dplyr::mutate(x_label = NA)
    #
    #     data_all <- return_survey_weighted_counts(input$topic) %>%
    #       dplyr::mutate(x_label = NA)
    #
    #     series_id <- unique(data$group) # provide ID for proxy
    #
    #     proxy_bar %>%
    #       proxy_to_group(data = data,
    #                           data_all = data_all,
    #                           meta = filter_meta(),
    #                           colors = colors(),
    #                           id = series_id,
    #                           responses = sum(data$Responses),
    #                           theme = naccho.viz::basic)
    #   }
    # })

    # for Tables -------
    output$data_table <- reactable::renderReactable({
      req(input$view == "Table")

      #add topic value for function
      topic <- input$topic

      filtering <-  input$filter

      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL  # If input$classifications does not exist, set it to NULL
      }

      #debug
      message("input$topic = ")
      print(input$topic)
      message("topic variable = ")
      print(topic)

      #debug
      message("input$filter = ")
      print(input$filter)
      message("filtering variable = ")
      print(filtering)



      data <- process_table_data(topic, filtering, classifications)


      data <- data %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
        )

      message("data =")
      print(data)


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

      message("data =")
      print(data)

      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic)
    }) #end of tables output code

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


  })
}

## To be copied in the UI
# mod_finance_ui("finance_1")

## To be copied in the server
# mod_finance_server("finance_1")
