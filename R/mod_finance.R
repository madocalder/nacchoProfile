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
              inputId = ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-line"></i>'), # Chart icon
                HTML('<i class="fas fa-table"></i>') # Map icon
              ),
              choiceValues = c("Line Graph", "Table")
            )
          ),


          # add main page content here ----------------------------------------------
          div(
            class = "viz-content",

            # Line Graph View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("figure_line"), height = "100%"),
                style = "height: 65vh; width: 50vw;"
              )
            ),

            # Bar Graph View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Bar Graph\'"),
              div(
                highcharter::highchartOutput(ns("figure"), height = "100%"),
                style = "height: 65vh; width: 50vw;"
              )
            ),

            # Tree Map View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Tree Map\'"),
              div(
                highcharter::highchartOutput(ns("figure_tree"), height = "100%"),
                style = "height: 65vh; width: 50vw;"
              )
            ),

            # Table View
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              div(
                uiOutput(ns("table_title")),
                reactable::reactableOutput(ns("data_table"), height = "auto"),
                br(),
                HTML('<div style="font-size: 2vmin;">', source_line, '
                   <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),
                style = "height: 65vh; width: 50vw;"
              )
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
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-chart-tree-map"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Line Graph", "Tree Map", "Table"))

      } else if (input$topic %in% c("c3q15", "totexp_percap", "non_weighted_revenue_percap")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Line Graph", "Table"))


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
                 input$view == "Line Graph") {

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

    output$figure_line <- highcharter::renderHighchart({
      req(input$topic == "totexp_percap", input$filter == "All LHDs")

      data <- process_line_data("totexp_percap", "All LHDs")

     naccho.viz::create_grouped_line(data = data,
                                     x_variable = "year",
                                     y_variable = "y",
                                     x_group = "category",
                                     title_text = "",
                                     subtitle_text = "",
                                     y_label = "Per Capita Expenditures",
                                     my_colors = c(naccho.viz::terracotta, naccho.viz::green),
                                     x_label = "",
                                     y_format = "\u0024{value:,.0f}",
                                     legend_enable = TRUE,
                                     legend_title = "",
                                     allow_export = TRUE,
                                     tooltip = " {point.stat} per capita expeditures:
                                     <b>\u0024{point.value:.0f}</b>")

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
              set_format = "{value}%") %>%
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
      req(input$topic == "non_weighted_revenue_total")

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
                  fontSize = "2vmin",
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

    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    # proxy_hist <- highcharter::highchartProxy(ns("figure_hist"))
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

      if (input$topic %in% c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap") & input$filter == "All LHDs" & input$view == "Line Graph") {




      } else if (input$topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & input$filter == "All LHDs" & input$view == "Bar Graph") {
        data <- return_survey_weighted_counts(input$topic) %>%
          dplyr::mutate(x_label = NA)

        proxy_bar %>%
          proxy_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic)



      } else if (input$topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & input$filter != "All LHDs" & input$view == "Bar Graph") {
        data <- return_survey_weighted_counts(input$topic, input$filter) %>%
          dplyr::mutate(x_label = NA)

        data_all <- return_survey_weighted_counts(input$topic) %>%
          dplyr::mutate(x_label = NA)

        series_id <- unique(data$group) # provide ID for proxy

        proxy_bar %>%
          proxy_to_group(data = data,
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
      topic <- input$topic

      filtering <-  input$filter

      classifications <- if (!is.null(input$classifications) && length(input$classifications) > 0) {
        input$classifications
      } else {
        NULL  # If input$classifications does not exist, set it to NULL
      }

      #debug
      message("input$topic =")
      print(input$topic)
      message("topic variable =")
      print(topic)

      #debug
      message("input$filter =")
      print(input$filter)
      message("filtering variable =")
      print(filtering)



      data <- process_data(topic, filtering, classifications)


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



  })
}

## To be copied in the UI
# mod_finance_ui("finance_1")

## To be copied in the server
# mod_finance_server("finance_1")
