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

              # FTEs
              conditionalPanel(
                condition = paste0('input[\'', ns('topic'), "\'] == \'expenditures\'"),
                radioButtons(
                  ns("type_exp"),
                  "",
                  choices = finance_expenditure_types,
                  inline = TRUE
                )
              ),

              # Employees
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
                  choices = list("Changes in Budgets" = "c10q301",
                                 "Expected Budget Cuts" = ""),
                  inline = TRUE
                )
              )
            )
          ),

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

            # display when map is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Box Plot\'"),
              highcharter::highchartOutput(ns("figure_box"), height = 575)
            )

            # # display when map is not selected
            # conditionalPanel(
            #   condition = paste0('input[\'', ns('topic'), "\'] == \'jobs\'"),
            #   highcharter::highchartOutput(ns("figure_jobs"), height = 675)
            # )
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
      if (input$topic == "expenditures") {

          updateSelectInput(session, "filter",
                            choices = landscape_filters)

        if (input$type_exp == "exp_numcat") {
          updateSelectInput(session, "view",
                            choices = c("Bar Graph",
                                        "Table"))

        } else if (input$type_exp == "c3q15") {
          updateSelectInput(session, "view",
                            choices = c("Box Plot",
                                        "Map",
                                        "Line Graph",
                                        "Table"))
        } else {
          updateSelectInput(session, "view",
                            choices = c("Box Plot",
                                        "Table"))
        }

      } else if (input$topic == "revenues") {

          updateSelectInput(session, "filter",
                            choices = landscape_filters)

      } else if (input$topic == "budget_changes") {
        updateSelectInput(session, "filter",
                          choices = "All Filters")
      }
    })



    # Reactive to get the inputs that I need for filtering --------------------
    get_finance_input <- function() {
      if (input$topic == "expenditures") { # FTEs
        input$type_exp
      } else if (input$topic == "revenues") { # Employees
        input$type_rev
      } else if (input$topic == "budget_changes") { # Jobs
        input$type_budget
      } else {
        NULL
      }
    }


    # main content ============================================================

    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      req(!is.null(get_finance_input()))
      meta %>% dplyr::filter(field == get_finance_input())
    })


    # figure output -----------------------------------------------------------
    # used for first histogram
    output$figure_box <- highcharter::renderHighchart({

      data <- boxplot_data("c3q15")

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


    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    proxy_box <- highcharter::highchartProxy(ns("figure_box"))


    # inputs for each topic ---------------------------------------------------
    inputs_exp <- reactive({
      input$topic
      input$filter
      input$type_exp
    })


    # inputs_emp <- reactive({
    #   input$topic
    #   input$filter
    #   input$type_emp
    # }) %>% debounce(100)
    #
    #
    # inputs_jobs <- reactive({
    #   input$topic
    #   input$filter
    #   input$type_jobs
    # }) %>% debounce(100)





    # update proxy ------------------------------------------------------------
    # for Expenditures --------------------------------------------------------
    observeEvent(inputs_exp(), {

      if (input$type_exp %in% c("c3q15", "totexp_percap") & input$filter == "All LHDs" & input$view == "Box Plot") {
        data <- boxplot_data(input$type_exp)

        proxy_box %>%
          proxy_to_box(data,
                       meta = filter_meta())

      } else if (input$type_exp %in% c("c3q15", "totexp_percap") & input$filter != "All LHDs" & input$view == "Box Plot") {

        data <- boxplot_data(input$type_exp,
                             grps = input$filter)
        names(data) <- NULL

        proxy_box %>%
          proxy_to_group_box(data,
                       meta = filter_meta())

      } else if (input$type_exp == "exp_numcat" & input$filter != "All LHDs" & input$view == "Bar Graph") {





      }
    })


  })
}

## To be copied in the UI
# mod_finance_ui("finance_1")

## To be copied in the server
# mod_finance_server("finance_1")
