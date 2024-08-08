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
                  "Pie Chart",
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
            highcharter::highchartOutput(ns("figure"), height = 575)
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


    # observeEvent(input$topic == "occupation_class", {
    #     updateSelectInput(session, "filter",
    #                       choices = landscape_filters)
    # })
    #
    # observeEvent(input$topic == "c5q37", {
    #   updateSelectInput(session, "filter",
    #                     choices = "No Filter")
    # })


    # right sidebar -----------------------------------------------------------
    # viz filters
    observeEvent(input$viz_filters, {

      shinyjs::show("toggle_viz_filters")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

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

          # display occupational ftes is selected
        } else if (input$topic == "c5q37") {
          radioButtons(
            ns("type_fte"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between various measurements or
                 groupings. <br> <br>")),
            choices = workforce_fte_types
          )
        } else if (input$topic == "c5q36") {
          radioButtons(
            ns("type_fte"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between various measurements or
                 groupings. <br> <br>")),
            choices = c("Total Employees", "Per Capita Employees")
          )

        } else if (input$topic == "jobs") {
          radioButtons(
            ns("type_fte"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between measurements or
                 groupings. <br> <br>")),
            choices = c("Positions Lost and Gained",
                        "Change in Losses and Gains")
          )
        }
      })
    })

    # takeaway
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


    # survey questions, responses, and notes
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


    # definitions
    observeEvent(input$definitions, {

      shinyjs::hide("toggle_viz_filters")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::show("toggle_definitions")

      # output$show_definitions <- renderUI({
      #
      #   definitions <- definitions %>%
      #     dplyr::filter(field %in% c(input$topic, input$filter))
      #
      #   links <- lapply(seq_along(definitions$item), function(i) {
      #     div(
      #       tags$a(
      #         class = "def-link",
      #         `data-bs-toggle` = "collapse",
      #         href = paste0("#collapseExample", i),
      #         paste(definitions$item[[i]])
      #       ),
      #       div(
      #         class= "collapse",
      #         id= paste0("collapseExample", i),
      #         HTML(definitions$definition[[i]], "<br> <br>")
      #       )
      #     )
      #   })
      #   div(
      #     p(htmltools::HTML("<b>Definitions</b>: <br>")),
      #     links
      #   )
      # })
    })



    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      if (input$topic == "c5q37") {
        req(input$type_fte)
        meta %>% dplyr::filter(field == input$type_fte)
      } else if (input$topic == "occupation_class") {
        meta %>% dplyr::filter(field %in% input$type_class)
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
          # theme = naccho.viz::hist_theme
        ) %>%
        viz_export()
    })



    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    proxy <- highcharter::highchartProxy(ns("figure"))




    # update proxy ------------------------------------------------------------
    # this observe is needed to redraw the figure based on the input selected
    # by the user. It also filters and stores tables based on the inputs
    # combinations selected by the user.
    observe ({

      # update the yAxis title while maintaining the line animation
      shinyjs::runjs(paste0("
        var chart = $('#workforce_1-figure').highcharts();
        chart.yAxis[0].update({
          title: {
            text: '", filter_meta()$y_label, "',
            style: { color: '#333333' }
          }
        }, true, { duration: 1000 });
    "))

      req(isTruthy(input$type_fte) || isTruthy(input$classifications)) # prevent error


      if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter == "No Filter") {
        data <- return_survey_weighted_counts("FTE_numcat") %>%
          dplyr::mutate(x_label = NA)

        proxy %>%
          proxy_hist_to_single(data,
                               meta = filter_meta(),
                               theme = naccho.viz::basic_theme)


      } else if (input$topic == "c5q37" & input$type_fte == "FTE_numcat" & input$filter != "No Filter") {
        data <- return_survey_weighted_counts("FTE_numcat", input$filter) %>%
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



      } else if (input$topic == "c5q37" & input$type_fte == "fte_bypop") {
        data <- create_hist(input = "fte_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data,
                           meta = filter_meta(),
                           responses = sum(data$y))



      } else if (input$topic == "c5q37" & input$type_fte == "c5q37") {
        data <- create_hist(input = "c5q37",
                            x_label = "Total FTEs")
        proxy %>%
          proxy_to_hist_total(data,
                              meta = filter_meta(),
                              responses = sum(data$y))



      } else if (input$topic == "occupation_class") {
        req(length(input$classifications) >= 1) # prevents error

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

          print(data)
          print(series_id)
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
      }
    })
  })
}

## To be copied in the UI
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
