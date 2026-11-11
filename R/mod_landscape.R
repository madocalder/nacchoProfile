#' landscape UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landscape_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'landscape_tab'",
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

          div( class = "topic-select",
               style = "grid-area: 1 / 1 / 2 / 2;
              justify-self: center;
              display: block;
              /* align-self: end; */
              /* margin-bottom: clamp(2px, 2.5%, 8px);*/",
               selectInput(
                 inputId = ns("topic"),
                 strong("Topic"),
                 choices = landscape_topics,
                 selectize = TRUE,
                 selected = landscape_topics[2]
               )
          ),
          div( class = "filter-select",
               style = "grid-area: 1 / 2 / 2 / 3;
              justify-self: center;
              display: block;
              /* align-self: end; */
              /* margin-bottom: clamp(2px, 2.5%, 8px);*/",
               selectInput(
                 inputId = ns("filter"),
                 strong("Filter"),
                 choices = landscape_filters
               )
          ),


          # Visualization Toggles
          div(
            class = "viz-view",
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
              ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-simple"></i>'),  # Chart icon
                HTML('<i class="fas fa-chart-line"></i>'),    # Line graph icon
                HTML('<i class="fas fa-table"></i>')          # Table icon
              ),
              choiceValues = c('Bar Graph', 'Line Graph', 'Table'),
              selected = 'Bar Graph'
            )
          ),

          # Bottom Section: Visualization content
          div(
            class = "viz-content",
            style = "grid-area: 3 / 1 / 3 / 3;
    justify-self: center;
    align-self: start;
    overflow-y: visible;
    overflow-x: clip;
    width: clamp(50vw, 100%, 90vw);
    height: clamp(375px, 100%, 70vh);
    padding-bottom: 1em;
    /* margin-top: 1%; */",


            # display when Bar is selected and not Map, Table, or Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), '\'] != \'Map\' && input[\'', ns('view'), '\'] != \'Table\' && input[\'', ns('view'), "\'] != \'Line Graph\'"),
              # div(
              highcharter::highchartOutput(ns("figure"), height = "100%"),
              style = "height: 100%"
              # )
            ),

            # display when Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),

              div( class = "table-grid",


                   div(class = "table-title-grid",


                       uiOutput(ns("table_title")),
                       csvDownloadButton(ns("data_table"), filename = "landscapeNACCHO.csv")
                   ),

                   reactable::reactableOutput(ns("data_table"),
                                              height = "100%",
                                              width = "auto"
                   ),

                   HTML('<div style="font-size: calc(5px + .2em + .8vmin);">', source_line, '<br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>')


              )

            ),

            # view output Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("line"), height = "100%"),
                style = "height: 100%"
              )
            )
          )
        ), #end center content

        right = div(
          div(
            class = "row color-block-header",
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("takeaway"), # key takeaway
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: calc(28px + .2em); text-align: center;"),
                class = "link-no-underline action-link"),
              span(class = "hover-text-content-side", "Takeaways")
            ),
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("context"), # survey questions and responses
                "",
                icon = tags$i(class = "fa-light fa-circle-info",
                              style = "font-size: calc(28px + .2em); text-align: center;"),
                class = "link-no-underline action-link"),
              span(class = "hover-text-content-side", "Notes")
            ),
            div(
              class = "col-sm-4 hover-text-side",
              align = "center",
              br(),
              actionLink(
                inputId = ns("definitions"), # related definitions
                "",
                icon = tags$i(class = "fa-light fa-list",
                              style = "font-size: calc(28px + .2em); text-align: center;"),
                class = "link-no-underline action-link"),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),

          div(
            class = "row color-block-content",
            shinyjs::hidden(
              div(id = ns("toggle_takeaway"),
                  class = "context-content",
                  uiOutput(ns("show_takeaway"))
              )
            ),
            shinyjs::hidden(
              div(id = ns("toggle_context"),
                  class = "context-content",
                  uiOutput(ns("show_context"))
              )
            ),
            shinyjs::hidden(
              div(id = ns("toggle_definitions"),
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


#' landscape Server Functions
#'
#' @noRd
mod_landscape_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Topics that should display 2022 weighted results
    topics_2022 <- c(
      "age_cat",
      "c4q29",
      "c4q34",
      "ethnicity",
      "field",
      "race",
      "tenure"
    )

    selected_source <- reactive({
      if (input$topic %in% c("age_cat", "c4q29", "c4q34",
                             "ethnicity", "field", "race", "tenure")) {
        source_line_2022
      } else {
        source_line
      }
    })

    # Select the appropriate weighted-count function
    get_weighted_counts <- function(q_label, grps = NULL) {

      if (q_label %in% topics_2022) {
        return_survey_weighted_counts_2022(q_label, grps)
      } else {
        return_survey_weighted_counts(q_label, grps)
      }

    }

    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("c2q501", "lboh")) {
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
                                                HTML('<i class="fas fa-chart-line"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Line Graph"))
      }
    })

    # observe({
    #   req(input$topic, input$filter, input$view)
    #
    #   # If the selected view is "Line Graph", ensure the topic is valid
    #   if (input$view == "Line Graph" && input$topic == "c2q501") {
    #     # If the combination is invalid, reset the view to "Bar Graph" or another valid view
    #     shinyWidgets::updateRadioGroupButtons(session, "view", selected = "Bar Graph")
    #
    #     # Optionally, show a message to the user about invalid combination
    #     showNotification("This topic cannot display a Line Graph.", type = "error")
    #   }
    #
    # })

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...

    filter_meta <- reactive({
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      message("filter_meta inputs: ", input$topic, ", ", input$filter, ", ", input$view)
      tryCatch({
        result <- meta %>%
          dplyr::filter(
            field == input$topic,
            stringr::str_detect(view, input$view),
            stringr::str_detect(filter, input$filter)
          )
        message("filter_meta output rows: ", nrow(result))
        result
      }, error = function(e) {
        message("filter_meta error: ", e$message)
        NULL
      })
    })









    #table titles-----------------
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

    # Dynamic color definitions ---------------------------------------------------
    colors <- reactive({
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



    #Tables output ------
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



      data <- process_table_data(topic, filtering, classifications)

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

    })



    # Bar figure output -----------------------------------------------------------
    # Bar Graph output with proxy
    output$figure <- highcharter::renderHighchart({
      req(input$view == "Bar Graph", input$topic, input$filter)

      # Create an empty bar chart for setup
      bar_chart <- highcharter::highchart()

      # Combine data for "All LHDs" and filtered LHDs
      combined_data <- if (input$filter == "All LHDs") {
        if (input$topic == "field") {

          spec_data_all%>%
            dplyr::filter(Question != "spec_combined")

        } else if (input$topic == "tenure"){

          data <- return_survey_weighted_counts_2022("tenure_numcat")

          data$category <- factor(data$category,
                                  levels = c("Less Than 2 Years", "2 - 5 Years",
                                             "6 - 10 Years", "11 Or More Years"),
                                  ordered = TRUE)
          data <- data %>%
            dplyr::mutate(tt = paste0(" all LHD top executives' tenure is ", tolower(category)), ".")

          data

        } else {
          get_weighted_counts(input$topic)
        }
      } else {
        all_data <- if (input$topic == "field") {

          spec_data_all%>%
            dplyr::filter(Question != "spec_combined")

        } else if  (input$topic == "tenure"){

          data <- return_survey_weighted_counts_2022("tenure_numcat")

          data$tenure_numcat <- factor(data$tenure_numcat,
                                       levels = c("Less Than 2 Years", "2 - 5 Years",
                                                  "6 - 10 Years", "11 Or More Years"),
                                       ordered = TRUE)

          data <- data %>%
            dplyr::mutate(tt = paste0(" all LHD top executives' tenure is ", tolower(category)), ".")%>%
            dplyr::arrange(tenure_numcat)

          data

        } else {
          get_weighted_counts(input$topic)
        }

        filtered_data <- if (input$topic == "field") {

          spec_data %>%
            dplyr::filter(filter == input$filter)%>%
            dplyr::filter(Question != "spec_combined")


        } else if (input$topic == "tenure") {

          data <- return_survey_weighted_counts_2022("tenure_numcat", input$filter)

          data$tenure_numcat <- factor(data$tenure_numcat,
                                       levels = c("Less Than 2 Years", "2 - 5 Years",
                                                  "6 - 10 Years", "11 Or More Years"),
                                       ordered = TRUE)

          data <- data %>%
            dplyr::mutate(tt = dplyr::case_when(
              input$filter != "censusregion" ~
                paste0(" ", tolower(group), " LHD top executives' tenure is ", tolower(category), "."),
              TRUE ~  paste0(" ", tolower(group), "ern LHD top executives' tenure is ", tolower(category), ".")
            )) %>%
            dplyr::arrange(tenure_numcat)

          data

        } else {
          get_weighted_counts(input$topic, input$filter)
        }

        combined <- dplyr::bind_rows(
          all_data %>% dplyr::mutate(filter = "All LHDs"),
          filtered_data
        )



        combined


      }


      if (input$filter != "All LHDs"){
        if(input$topic != "field") {
          combined_data <- combined_data %>%
            dplyr::mutate(Responses = dplyr::case_when(
              group == "All LHDs" ~ 0,
              TRUE ~ Responses),
              N = sum(Responses)
            )
        } else {
          combined_data <- combined_data %>%
            dplyr::mutate(N = Responses)
        }
      }

      if (input$filter == "All LHDs"){
        if(input$topic != "field") {
          combined_data <- combined_data %>%
            dplyr::mutate(
              N = sum(Responses)
            )
        } else {
          combined_data <- combined_data %>%
            dplyr::mutate(N = Responses)
        }
      }

      if (input$topic == "c4q29"){
        combined_data<- combined_data%>%
          dplyr::filter(category %in% c("Male", "Female"))
      }

      combined_data <- combined_data %>%
        dplyr::mutate(group_tt = dplyr::case_when(
          group != "All LHDs" & group != "censusregion" ~ tolower(group),
          group == "censusregion" ~ paste0(tolower(group), "ern"),
          TRUE ~ "all"
        ))

      # View(combined_data)

      # Generate the bar chart (no series yet)
      bar_chart <- set_column(combined_data,
                              y_format = filter_meta()$y_format,
                              y_label =filter_meta()$y_label,
                              data_label = filter_meta()$data_label) %>%
        viz_options(
          viz_title = filter_meta()$title,
          viz_caption = paste(
            "<b> N: </b>", show_n(combined_data$N),
            "<br>", selected_source()
          ),
          tooltip = filter_meta()$tooltip
        )%>%
        highcharter::hc_yAxis(
          maxPadding = if (input$topic %in% c("age_cat", "race", "ethnicity")) 0 else .09,

          max = dplyr::case_when(
            input$topic == "age_cat" ~ 50,
            input$topic == "ethnicity" ~ 120,
            input$topic == "race" ~ ceiling(max(combined_data$y, na.rm = TRUE) * 1.10),
            TRUE ~ filter_meta()$y_max
          ),

          endOnTick = if (input$topic %in% c("age_cat", "race", "ethnicity")) FALSE else TRUE
          # ,
          # labels = list(
          #   format = "{value}%"
        )%>%
        highcharter::hc_legend(reversed = TRUE)%>%
        viz_export()

      # if (input$topic == "field" & input$filter != "All LHDs") {
      #   bar_chart <- bar_chart %>%
      #     highcharter::hc_xAxis(
      #       labels = list(enabled = FALSE),
      #       title = list(text = NULL),
      #       categories = unique(combined_data$group)
      #     )
      # }

      if (input$topic == "tenure") {

        tenure_categories <- c(
          "Less Than 2 Years",
          "2 - 5 Years",
          "6 - 10 Years",
          "11 Or More Years"
        )

        bar_chart <- bar_chart %>%
          highcharter::hc_xAxis(
            categories = tenure_categories,
            labels = list(enabled = TRUE),
            title = list(text = NULL)
          )
      }


      bar_chart
    })



    # Proxy for the Bar Chart
    proxy_bar <- highcharter::highchartProxy(ns("figure"))

    # Update the bar chart with new data when inputs change
    observe({

      req(input$topic, input$filter)  # Ensure inputs are available
      req(input$view == "Bar Graph")
      # Remove all old series (to prevent overlap)
      proxy_bar %>%
        highcharter::hcpxy_remove_series(id = "filtered_bar_series") %>%
        highcharter::hcpxy_remove_series(id = "all_lhds_bar_series")

      # Combine the updated data based on input$topic and input$filter
      combined_data <- if (input$filter == "All LHDs") {
        if (input$topic == "field") {
          spec_data_all%>%
            dplyr::filter(Question != "spec_combined")

        } else if (input$topic == "tenure"){

          data <- return_survey_weighted_counts_2022("tenure_numcat")

          data$tenure_numcat <- factor(data$tenure_numcat,
                                       levels = c("Less Than 2 Years", "2 - 5 Years",
                                                  "6 - 10 Years", "11 Or More Years"),
                                       ordered = TRUE)

          data <- data %>%
            dplyr::mutate(tt = paste0(" all LHD top executives' tenure is ", tolower(category), "."))


          data

        } else {
          get_weighted_counts(input$topic)
        }

      } else {
        all_data <- if (input$topic == "field") {

          all_data <- spec_data_all%>%
            dplyr::filter(Question == "spec_combined")

        } else if (input$topic == "tenure") {
          data <- return_survey_weighted_counts_2022("tenure_numcat", input$filter)

          data$tenure_numcat <- factor(data$tenure_numcat,
                                       levels = c("Less Than 2 Years", "2 - 5 Years",
                                                  "6 - 10 Years", "11 Or More Years"),
                                       ordered = TRUE)

          data <- data %>%
            dplyr::mutate(tt = dplyr::case_when(
              input$filter != "censusregion" ~
                paste0(" ", tolower(group), " LHD top executives' tenure is ", tolower(category), "."),
              TRUE ~  paste0(" ", tolower(group), "ern LHD top executives' tenure is ", tolower(category), ".")
            ))

          data

        } else {
          get_weighted_counts(input$topic)
        }

        filtered_data <- if (input$topic == "field") {
          spec_data %>% dplyr::filter(filter == input$filter,
                                      category == group)

        } else if (input$topic == "tenure"){

          data <- return_survey_weighted_counts_2022("tenure_numcat", input$filter)

          data$tenure_numcat <- factor(data$tenure_numcat,
                                       levels = c("Less Than 2 Years", "2 - 5 Years",
                                                  "6 - 10 Years", "11 Or More Years"),
                                       ordered = TRUE)

          data


        } else {
          get_weighted_counts(input$topic, input$filter)
        }

        combined <- dplyr::bind_rows(
          all_data %>% dplyr::mutate(filter = "All LHDs"),
          filtered_data
        )

        # if (input$topic == "tenure"){
        #
        #   combined <- combined %>%
        #     dplyr::mutate(x = seq_along(category)-1,
        #                   group = category)
        #
        # }

        combined

      }

      if (input$filter != "All LHDs"){
        if(input$topic != "field") {
          combined_data <- combined_data %>%
            dplyr::mutate(Responses = dplyr::case_when(
              group == "All LHDs" ~ 0,
              TRUE ~ Responses),
              N = sum(Responses)
            )
        } else {
          combined_data <- combined_data %>%
            dplyr::mutate(N = Responses)
        }

        if(input$topic != "field") {
          combined_data <- combined_data %>%
            dplyr::mutate(Responses = dplyr::case_when(
              group == "All LHDs" ~ 0,
              TRUE ~ Responses),
              N = sum(Responses)
            )

        } else {
          combined_data <- combined_data %>%
            dplyr::mutate(N = Responses)
        }
      }

      if (input$filter == "All LHDs"){
        if(input$topic != "field") {
          combined_data <- combined_data %>%
            dplyr::mutate(
              N = sum(Responses)
            )
        } else {
          combined_data <- combined_data %>%
            dplyr::mutate(N = Responses)
        }
      }

      if (input$topic == "c4q29"){
        combined_data<- combined_data%>%
          dplyr::filter(category %in% c("Male", "Female"))
      }

      combined_data <- combined_data %>%
        dplyr::mutate(group_tt = dplyr::case_when(
          group != "All LHDs" ~ tolower(group),
          TRUE ~ "all"
        ))

      # View(combined_data)


      # Dynamically update the bar chart with new data
      proxy_bar %>%
        highcharter::hcpxy_add_series(
          id = "filtered_bar_series",
          name = input$filter,
          data = combined_data,
          type = "column",  # Ensure this is a column chart (bar graph)
          selected = TRUE
        )

      if (input$topic == "field" & input$filter != "All LHDs") {

        proxy_bar %>%
          highcharter::hcpxy_update(
            xAxis = list(
              labels = list(enabled = FALSE),
              title = list(text = NULL),
              categories = unique(combined_data$group)
            )
          )



      }
      #  else if (input$topic == "tenure" & input$filter != "All LHDs") {
      #
      #   proxy_bar %>%
      #     highcharter::hcpxy_update(
      #       xAxis = list(
      #         labels = list(enabled = FALSE),
      #         title = list(text = NULL)),
      #         categories = unique(combined_data$group)
      #        )
      #
      # }

    })


    # Line Output-------------------
    output$line <- highcharter::renderHighchart({

      req(input$view == "Line Graph",
          input$topic != "c2q501",
          input$filter)

      req(filter_meta())

      meta_data <- filter_meta()
      req(meta_data, nrow(meta_data) > 0)

      # Retrieve historical and current data, where available
      data <- process_line_data(input$topic, input$filter)

      # Remove duplicate age-category points for the same year
      if (input$topic == "age_cat") {
        data <- data %>%
          dplyr::group_by(year, legend_label) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup()
      }

      req(data, nrow(data) > 0)

      # Convert year to numeric for plotting
      data <- data %>%
        dplyr::mutate(
          year = as.numeric(as.character(year)),
          legend_label = as.character(legend_label)
        )

      # Build chart
      line_graph <- highcharter::highchart() %>%
        highcharter::hc_chart(type = "line") %>%
        highcharter::hc_xAxis(
          type = "linear",
          title = list(text = "Year"),
          tickPositions = sort(unique(data$year))
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = meta_data$y_label),
          min = 0,
          max = meta_data$y_max,
          endOnTick = TRUE
        ) %>%
        highcharter::hc_plotOptions(
          line = list(
            lineWidth = 4,
            marker = list(enabled = TRUE)
          )
        ) %>%
        highcharter::hc_colors(naccho.viz::colors)

      # Add one line for each category
      for (series_name in unique(data$legend_label)) {

        group_data <- data %>%
          dplyr::filter(legend_label == series_name) %>%
          dplyr::arrange(year)

        line_graph <- line_graph %>%
          highcharter::hc_add_series(
            name = series_name,
            type = "line",
            data = lapply(seq_len(nrow(group_data)), function(i) {
              point <- as.list(group_data[i, , drop = FALSE])

              # Preserve all original fields for custom tooltips
              point$x <- as.numeric(group_data$year[i])
              point$y <- as.numeric(group_data$y[i])
              point$name <- as.character(group_data$year[i])

              point
            })
          )
      }

      # Apply dashboard formatting
      line_graph <- line_graph %>%
        viz_options(
          viz_caption = paste(
            "<b> N: </b>",
            show_n(data$Responses),
            "<br>",
            selected_source()
          ),
          tooltip = meta_data$tooltip
        ) %>%
        highcharter::hc_title(text = meta_data$title) %>%
        highcharter::hc_legend(reversed = TRUE) %>%
        highcharter::hc_chart(
          style = list(minHeight = "50vh")
        ) %>%
        viz_export()

      line_graph
    })


    # Proxy for the Line Chart
    # proxy_line <- highcharter::highchartProxy(ns("line"))

    # observe({
    #  req(input$topic, input$filter)

    # line_data <- process_line_data(input$topic, input$filter)
    # if (is.null(line_data) || nrow(line_data) == 0) {
    #   showNotification("No data available for line chart.", type = "warning")
    #  return()
    # }

    # proxy_line %>%
    #   highcharter::hcpxy_update(
    #     series = list(
    #       id = "line_series",
    #       data = line_data,
    #       name = paste("Line for", input$filter)
    #     )
    #   )
    # })



    # # Update bar graph and line graph when topic or filter changes
    # observe({
    #   req(input$topic, input$filter)  # Ensure inputs are available
    #
    #   # Update the bar graph
    #   if (input$view == "Bar Graph") {
    #     output$figure <- highcharter::renderHighchart({
    #       req(input$topic, input$filter)
    #
    #       # Combine data for "All LHDs" and filtered LHDs
    #       combined_data <- if (input$filter == "All LHDs") {
    #         if (input$topic == "field") {
    #           spec_data_all
    #         } else {
    #           return_survey_weighted_counts(input$topic)
    #         }
    #       } else {
    #         all_data <- if (input$topic == "field") {
    #           spec_data_all
    #         } else {
    #           return_survey_weighted_counts(input$topic)
    #         }
    #
    #         filtered_data <- if (input$topic == "field") {
    #           spec_data %>% dplyr::filter(filter == input$filter)
    #         } else {
    #           return_survey_weighted_counts(input$topic, input$filter)
    #         }
    #
    #         dplyr::bind_rows(
    #           all_data %>% dplyr::mutate(filter = "All LHDs"),
    #           filtered_data
    #         )
    #       }
    #
    #
    #       bar_chart <- set_column(combined_data, y_format = filter_meta()$y_format) %>%
    #         viz_options(
    #           viz_title = filter_meta()$title,
    #           viz_caption = paste("<b> N: </b>", sum(unique(combined_data$Responses), na.rm = TRUE), "<br>", source_line),
    #           tooltip = filter_meta()$tooltip
    #         )
    #
    #       bar_chart
    #     })
    #   }
    #
    #   # Update the line graph
    #   if (input$view == "Line Graph") {
    #     output$line <- highcharter::renderHighchart({
    #       req(input$topic, input$filter)
    #
    #       # Process and generate line graph data
    #       data <- process_line_data(input$topic, input$filter)
    #
    #       # Make sure there is data before rendering
    #       if (nrow(data) > 0) {
    #         naccho.viz::create_grouped_line(
    #           data = data,
    #           x_variable = "year",
    #           x_group = "legend_label",
    #           y_variable = "y",
    #           y_label = filter_meta()$y_label,
    #           y_max = max(data$y, na.rm = TRUE) * 1.1,
    #           x_label = "Year",
    #           select_theme = naccho.viz::basic,
    #           title = filter_meta()$title,
    #           tooltip = filter_meta()$tooltip
    #         )
    #       } else {
    #         return(NULL)  # Return nothing if there's no data for the line graph
    #       }
    #     })
    #   }
    # })
    #
    #


    # right content ===========================================================

    # takeaway ----------------------------------------------------------------
    # takeaway ----------------------------------------------------------------
    observeEvent(input$takeaway, {

      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        stringr::str_detect(view, input$view),
                        stringr::str_detect(filter, input$filter))


        p(htmltools::HTML(
          "<b>Key Takeaway(s)</b>: <br><br>",
          notes$takeaway))
      })
    }, ignoreNULL = FALSE)


    # survey questions, responses, and notes ----------------------------------
    observeEvent(input$context, {

      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        stringr::str_detect(view, input$view),
                        stringr::str_detect(filter, input$filter))

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


    # definitions -------------------------------------------------------------
    observeEvent(input$definitions, {

      toggle_sidebar_content("toggle_definitions",
                             c("toggle_takeaway", "toggle_context",
                               "toggle_definitions"))

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

        print(length(links))

        div(
          p(htmltools::HTML("<b>Definitions</b>: <br>")),

          # side right --------------------------------------------------------------

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
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
