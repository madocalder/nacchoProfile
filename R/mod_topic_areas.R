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
                HTML('<i class="fas fa-chart-line"></i>'), # Line graph icon
                HTML('<i class="fas fa-table"></i>') # Map icon
              ),
              choiceValues = c("Bar Graph", "Line Graph", "Table")
            )
          ),


          # add main page content here ----------------------------------------------

          div(
            class = "viz-content",

              highcharter::highchartOutput(ns("figure_bar"), height = 675)
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


    # main content ============================================================

    # inputs for each topic ---------------------------------------------------
    updated_inputs <- reactive({
      input$topic
      input$filter
    }) %>% debounce(100)



    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(input$topic))
      meta %>% dplyr::filter(field == input$topic)
    })


    # figure output -----------------------------------------------------------
    # used for bar graph
    output$figure_bar <- highcharter::renderHighchart({
      data <- tech_use_data %>%
        dplyr::filter(filter == "All LHDs",
                      category %in% c("Implemented")) %>%
        dplyr::mutate(category = tech_type) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(x = 1:length(tech_type)-1)

      responses <- tech_use_data %>%
        dplyr::filter(filter == "All LHDs") %>%
        dplyr::mutate(category = tech_type) %>%
        dplyr::group_by(category) %>%
        dplyr::summarise(n = sum(Responses))

      set_simple_bar(data) %>%
        viz_options(
          viz_title = "Percent of LHDs that Have Implemented the Following Informatics Systems",
          viz_subtitle = unique(data$subtitle),
          viz_caption = paste0("<b> N: </b>",  min(responses$n), " to ", max(responses$n), "<br>", source_line),
          tooltip = "",
          theme = naccho.viz::butterfly,
          legend_enable = FALSE,
          colors = colors()
        ) %>%
        viz_export()
    })



    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    proxy_bar <- highcharter::highchartProxy(ns("figure_bar"))



    # update proxy ------------------------------------------------------------
    # observeEvent(updated_inputs(), {
    #
    #   if (input$topic == "tech_use") {
    #     data <- tech_use_data %>%
    #       dplyr::filter(filter == input$filter,
    #                     category %in% c("Implemented")) %>%
    #       dplyr::mutate(category = tech_type) %>%
    #       dplyr::group_by(group) %>%
    #       dplyr::mutate(x = 1:length(tech_type)-1)
    #
    #     responses <- tech_use_data %>%
    #       dplyr::filter(filter == input&filter) %>%
    #       dplyr::mutate(category = tech_type) %>%
    #       dplyr::group_by(category) %>%
    #       dplyr::summarise(n = sum(Responses))
    #
    #
    #
    #   }
    # })



  })
}

## To be copied in the UI
# mod_topic_areas_ui("topic_areas_1")

## To be copied in the server
# mod_topic_areas_server("topic_areas_1")
