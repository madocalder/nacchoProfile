#' topic_areas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_topic_areas_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'topics_tab'",
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
                choices = other_topics,
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
                HTML('<i class="fas fa-chart-simple"></i>'), # Chart icon
                HTML('<i class="fas fa-table"></i>') # Map icon
              ),
              choiceValues = c("Bar Graph", "Table")
            )
          ),


          # add main page content here ----------------------------------------------

          div(
            class = "viz-content",

            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] != \'Table\'"),
              div(
              highcharter::highchartOutput(ns("figure_bar"), height = "100%"),
              style = "height: 65vh; width: 52vw;")
              ),

            # display when view Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              div(
                uiOutput(ns("table_title")),
                reactable::reactableOutput(ns("data_table"), height = "auto"),
                br(),
                HTML('<div style="font-size: 2vmin;">', source_line,'
                   <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),

                style = "height: 65vh; width: 50vw;")
            ),
            br(),
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

#' topic_areas Server Functions
#'
#' @noRd
mod_topic_areas_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("tech_use", "comm_channels", "medical", "dental", "behavioral")) {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"))


      } else {
        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Table"))
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
      } else {
        naccho.viz::green
      }
    })

    # inputs for each topic ---------------------------------------------------
    updated_inputs <- reactive({
      input$topic
      input$filter
    }) %>% debounce(100)


    #table titles---------------------------------------------------------------
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      req(input$topic)

      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(filter_meta()$title)

      # Render the HTML
      p(title_values, style = "font-size: 3vmin; font-weight: bold !important; text-align: center;")
    })

    # main content ============================================================



    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(input$topic)
      meta %>% dplyr::filter(field == input$topic)
    })


    # figure output -----------------------------------------------------------
    # used for bar graph
    output$figure_bar <- highcharter::renderHighchart({
      req(input$view == "Bar Graph")
      req(input$topic)

      topic = input$topic
      filtering = input$filter

      if (input$filter == "All LHDS"){
        data <- get_bar_data(topic = topic)
      } else {
        data <- get_bar_data(topic = topic, filter = filtering)
      }

      # View(data)

      # Adjust the bar chart settings for long names
      set_simple_bar(data) %>%
        highcharter::hc_chart(type = "bar") %>%  # Specify bar type explicitly
        highcharter::hc_xAxis(
          categories = as.character(data$category),  # Use category column for x-axis labels
          labels = list(
            staggerLines = 2,                  # Stagger labels
            # rotation = -45,
            autoRotation = TRUE, # Rotate labels for better readability
            style = list(
                         textOverflow = "ellipsis",  # Add ellipsis for overflow
                         width = "175vw"
                         )      # Set width to manage label length
          )
        ) %>%
        # highcharter::hc_plotOptions(bar = list(
        #   pointPadding = 0.2,  # Space between bars within a category
        #   groupPadding = 0.2   # Space between different categories
        # )) %>%
        viz_options(
          viz_title = "Percent of LHDs that Have Implemented the Following Communication Channels",
          viz_subtitle = unique(data$subtitle),
          viz_caption = paste0("<b> N: </b>", sum(data$Responses), "<br>", source_line),
          tooltip = "{point.category}: <br> {point.y}%",
          legend_enable = FALSE,
          colors = colors()
        ) %>%
        viz_export()
    })



    # # update the yAxis title while maintaining the line animation
    # use_js_proxy <- reactive({
    #   shinyjs::runjs(paste0("
    #     var chart = $('#landscape_1-figure').highcharts();
    #     chart.yAxis[0].update({
    #       title: {
    #         text: '", filter_meta()$y_label, "',
    #         style: { color: '#333333' }
    #       }
    #     }, true, { duration: 1000 });
    # "))
    # })

    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    # proxy_bar <- highcharter::highchartProxy(ns("figure_bar"))



    # this observe is needed to redraw the figure based on the input selected

    # observe({
    #   req(input$topic)
    #   updated_inputs()

    #   shinyjs::runjs(paste0("
    #     var chart = $('#landscape_1-figure').highcharts();
    #     chart.yAxis[0].update({
    #       title: {
    #         text: '", filter_meta()$y_label, "',
    #         style: { color: '#333333' }
    #       }
    #     }, true, { duration: 1000 });
    # "))

      # Topic: Tenure
      # View: Bar Graph
      # Filters: Rurality, Governance, LHD Size, Region
      # if (input$topic == "comm_channels" & input$filter == "All LHDs") {
      #
      #   data <- comms_use_data %>%
      #     dplyr::filter(filter == "All LHDs",
      #                   category %in% "Checked")%>%
      #     dplyr::mutate(category = comms_type) %>%
      #     dplyr::group_by(group) %>%
      #     dplyr::mutate(x = 1:length(comms_type)-1)
      #   responses <- tech_use_data %>%
      #     dplyr::filter(filter == "All LHDs") %>%
      #     dplyr::group_by(category) %>%
      #     dplyr::summarise(n = sum(Responses))
      #
      #
      #   set_simple_bar(data)

        # proxy_bar %>%
        #   highcharter::hcpxy_remove_series(
        #     id = c("Urban", "Rural")
        #   ) %>%
        #   highcharter::hcpxy_remove_series(
        #     id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
        #   ) %>%
        #   highcharter::hcpxy_remove_series(
        #     id = c("State", "Local", "Shared")
        #   ) %>%
        #   highcharter::hcpxy_remove_series(
        #     id = c("Midwest", "South", "West", "Northeast")
        #   ) %>%
        #   # proxy_to_single(data = data,
        #   #                 meta = filter_meta())%>%
        #   highcharter::hcpxy_set_data(
        #     data = data,
        #     type = "bar",
        #     mapping = highcharter::hcaes(x = x, y = y, group = Group, name = category)
        #   )

#
#       }

      # })


    # for Tables -------
    output$data_table <- reactable::renderReactable({
      req(input$view == "Table")

      #add topic value for function
      topic = input$topic

      filtering = input$filter

      classifications <- if (!is.null(input$service_changes) && length(input$service_changes) > 0) {
        input$service_changes

      } else if (!is.null(input$services) && length(input$services) > 0) {

        input$services

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
      #
      # #debug
      # message("input$services =")
      # print(input$services)
      # message("input$service_changes =")
      # print(input$service_changes)
      #
      # message("classifications variable =")
      # print(classifications)



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



  }) #end of server code
}

## To be copied in the UI
# mod_topic_areas_ui("topic_areas_1")

## To be copied in the server
# mod_topic_areas_server("topic_areas_1")
