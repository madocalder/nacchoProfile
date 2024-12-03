#' landscape UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landscape_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'landscape_tab'",
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
                choices = landscape_topics,
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
                HTML('<i class="fas fa-chart-simple"></i>'),  # Chart icon
                HTML('<i class="fas fa-chart-line"></i>'), # Line graph icon
                HTML('<i class="fas fa-table"></i>')         # Map icon
              ),
              choiceValues = c('Bar Graph', 'Line Graph', 'Table')
            )
          ),


          # add main page content here ----------------------------------------
          div(
            class = "viz-content",

            # display when Map is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Map\'"),
              div(
                highcharter::highchartOutput(ns("map"), height = "100%"),
                style = "height: 60vh; width: auto;"
              )
            ),

            # display when Bar is selected and not Map, Table, or Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), '\'] != \'Map\' && input[\'', ns('view'), '\'] != \'Table\' && input[\'', ns('view'), "\'] != \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("figure"), height = "100%"),
                style = "height: 58vh; width: auto;"
              )
            ),

            # display when Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              div(
                uiOutput(ns("table_title")),
                reactable::reactableOutput(ns("data_table"), height = "auto", width = "auto"),
                br(),
                HTML('<div style="font-size: 1.8vmin;">', source_line,'
         <br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>'),
                style = "margin-left: auto !important;
                        margin-right: auto !important;"
              )
            ),

            # view output Line
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Line Graph\'"),
              div(
                highcharter::highchartOutput(ns("line"), height = "60vh"),
                br(),
                # HTML('<div style="font-size: 2vmin;">', source_line,'</div>'),
                style = "height: 60vh; width: auto;"
              )
            )
          )
        ),

          # end of main content -----------------------------------------------------

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
                                style = "font-size: 4vmin; text-align: center;"),
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
                                style = "font-size: 4vmin; text-align: center;"),
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
                                style = "font-size: 4vmin; text-align: center;"),
                  class = "link-no-underline action-link"),
                span(class = "hover-text-content-side", "Definitions")
              ),
              hr(class = "right-hr")
            ),


          div(
            class = "row color-block-content",

            # toggle side content -----------------------------------------------------
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


    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic %in% c("c2q501")) {
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
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Bar Graph", "Line Graph", "Table"))
      }
    })

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      meta %>%
        dplyr::filter(field == input$topic)
    })

    #table titles-----------------
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      req(input$topic)

      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(subset(meta, field == input$topic)$title)

      # Render the HTML
      p(title_values, style = "font-size: 3vmin; font-weight: bold !important; text-align: center;")
    })


    # Bar figure output -----------------------------------------------------------
    output$figure <- highcharter::renderHighchart({
      req(input$view == "Bar Graph", input$topic, input$filter)

      if (input$filter == "All LHDs") {
        # Data for "All LHDs"
        data <- if (input$topic == "field") {
          spec_data_all
        } else {
          return_survey_weighted_counts(input$topic)
        }

        # Render chart for "All LHDs" using predefined functions
        set_column(data) %>%
          viz_options(
            viz_title = filter_meta()$title,
            viz_caption = paste("<b> N: </b>", sum(unique(data$Responses), na.rm = TRUE), "<br>", source_line),
            tooltip = "<span style=\"color:{point.color}\">\u25CF</span> {point.y:.0f}% of {point.category}."
          ) %>%
          viz_export()
      }
    })

    proxy <- highcharter::highchartProxy(ns("figure"))

    # Dynamic updates for specific filters ----------------------------------------
    observe({
      req(input$filter != "All LHDs")

      # Fetch data based on topic and filter
      data <- if (input$topic == "field") {
        spec_data %>% dplyr::filter(filter == input$filter)
      } else {
        return_survey_weighted_counts(input$topic, input$filter)
      }

      # Adjust grouping for filters
      if (input$filter == "c0popcat3") {
        data <- data %>%
          dplyr::mutate(group = factor(group, levels = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")))
      } else if (input$filter == "urbanrural2022") {
        data <- data %>%
          dplyr::mutate(group = factor(group, levels = c("Urban", "Rural")))
      } else if (input$filter == "censusregion") {
        data <- data %>%
          dplyr::mutate(group = factor(group, levels = c("Northeast", "Midwest", "South", "West")))
      }

      # Ensure group column exists
      if (!"group" %in% names(data)) {
        data$group <- "All"
      }

      # Update chart using proxy function

      proxy %>%
        proxy_to_group(
          data = data,
          meta = filter_meta(),
          id = unique(data$group),
          colors = colors(),
          tooltip = "<span style=\"color:{point.color}\">\u25CF</span> {point.y:.0f}% of {point.group} LHDs specialize in {point.category}."
        )
    })

    # Update the yAxis title dynamically ------------------------------------------
    use_js_proxy <- reactive({
      shinyjs::runjs(paste0("
    var chart = $('#landscape_1-figure').highcharts();
    chart.yAxis[0].update({
      title: {
        text: '", filter_meta()$y_label, "',
        style: { color: '#333333' }
      }
    }, true, { duration: 1000 });
  "))
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

    })


    # Line Output-------------------

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

      # #debug
      # message("input$topic =")
      # print(input$topic)
      # message("option variable =")
      # print(option)
      #
      # message("view selected =")
      # print(input$view)
      #
      # #debug
      # message("input$filter =")
      # print(input$filter)
      # message("filtering variable =")
      # print(filtering)

      data <- process_line_data(option, filtering)


      if(max(data$y, na.rm = TRUE) == 100) {
       y_max_rel <- max(data$y, na.rm = TRUE) + 1
      } else {
        y_max_rel <- max(data$y, na.rm = TRUE) * 1.01
      }

      if ("legend_title" %in% names(data) && !is.null(data$legend_title[1])) {
        line_title <- data$legend_title[1]  # Get the first value
      } else {
        line_title <- paste(filter_meta()$title, "Over Time")  # Default value if `legend_title` column is missing
      }

      if ("subtitle" %in% names(data) && !is.null(data$subtitle[1])) {
        subtitle <- data$subtitle[1]  # Get the first value
      } else {
        subtitle <- NULL
      }

# data$responses <- sum(data$Responses)
# print(data)

      data$year <- as.factor(data$year)

      naccho.viz::create_grouped_line(
        data = data,
        x_variable = "year",
        x_group = "legend_label",
        y_variable = "y",
        y_label = filter_meta()$y_label,
        y_max = y_max_rel,
        x_label = "Year",
        select_theme = naccho.viz::basic,
        title = line_title,
        subtitle = subtitle,
        # caption_text = paste0("<b> N: </b>", responses, "<br>", meta$caption),
        tooltip = "In {point.year}, {point.one}{point.two}.",
        legend_title = NA
      )


    })




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
                        view == input$view, # fix
                        filter == input$filter)

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
                        # view == input$view, # fix
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
            notes$notes
))
        }
      })
    })


    # definitions -------------------------------------------------------------
    observeEvent(input$definitions, {

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
