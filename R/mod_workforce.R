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
                choices = landscape_filters
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
            strong("Select Classification"),
            div(
              class = "checkboxes",
              checkboxGroupInput(
                ns("classifications"),
                "",
                workforce_classifications,
                selected = "c5q63"
              )
            )
          )

          # display occupational ftes is selected
        } else if (input$topic == "c5q37") {
          radioButtons(
            ns("type"),
            p(strong(HTML("Select a Measure <br>")),
              HTML("Use the selections below to toggle the
                 figure between various FTE measurements or
                 groupings. <br> <br>")),
            choices = workforce_fte_types
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
      meta %>%
        dplyr::filter(field == input$type)
    })



    # figure output -----------------------------------------------------------
    # used for first histogram
    output$figure <- highcharter::renderHighchart({
      data <- create_hist(input = "c5q37",
                          x_label = "Total FTEs")

      set_hist(data) %>%
        viz_options(
          viz_title = paste("The Number of LHDs by the Number of Total FTEs"),
          viz_caption = source_line,
          viz_subtitle = unique(data$subtitle),
          tooltip = "{point.y} LHDs have between {point.low_bin} and {point.high_bin} total FTEs",
          theme = naccho.viz::hist_theme
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

      req(input$type) # prevent error


      if (input$type == "FTE_numcat") {
        data <- return_survey_weighted_counts("FTE_numcat") %>%
          dplyr::mutate(x_label = NA)
        proxy %>%
          proxy_hist_to_single(data,
                               filter_meta(),
                               theme = naccho.viz::basic_theme)



      } else if (input$type == "fte_bypop") {
        data <- create_hist(input = "fte_bypop",
                            x_label = "Per Capita FTEs")
        proxy %>%
          proxy_to_hist_pc(data, filter_meta())



      } else if (input$type == "c5q37") {
        data <- create_hist(input = "c5q37",
                            x_label = "Total FTEs")
        proxy %>%
          proxy_to_hist_total(data, filter_meta())



      } else if (input$topic == "occupation_class") {


      }
    })






  })
}

## To be copied in the UI
# mod_workforce_ui("workforce_1")

## To be copied in the server
# mod_workforce_server("workforce_1")
