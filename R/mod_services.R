#' services UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_services_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'services_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          class = "col-pad",
          openMenu(),
          div(
            class = "row",
            align = "center",
            div(
              class = "col-sm-6 min-height-250",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = service_topics,
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
                HTML('<i class="fas fa-chart-simple"></i>'),
                HTML('<i class="fas fa-chart-line"></i>'),
                HTML('<i class="fas fa-table"></i>')
              ),
              choiceValues = c("Bar Graph", "Line Graph", "Table")
            )
          ),


          # add main page content here ----------------------------------------------
          div(
            class = "viz-content",

            # display when Service Provision and Bar Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] != 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Bar Graph'"),
              highcharter::highchartOutput(ns("figure_bar"), height = 675)
            ),

            # display when Service Provision and Line Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] != 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Line Graph'"),
              # highcharter::highchartOutput(ns("figure_line"), height = 575)
            ),

            # display when Service Changes and Scatter Plot are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Scatter Plot'"),
              highcharter::highchartOutput(ns("figure_scatter"), height = 775)
            ),

            # display when Service Changes and Butterfly Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Butterfly Graph'"),
              # highcharter::highchartOutput(ns("figure_butterfly"), height = 575)
            ),
            br(),
          )
        ),

        # end of main content -----------------------------------------------------

        right = div(
          div(
            class = "row color-block-header",
            id = ns("dynamic_row"),
            div(
              class = "col hover-text-side hide-icon",  # Hidden by default
              align = "center",
              actionLink(
                ns("get_services"), # service topics
                "",
                icon = tags$i(class = "fa-light fa-filter",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Add Services")
            ),

            div(
              class = "col hover-text-side",
              align = "center",
              br(),
              actionLink(
                ns("takeaway"), # key takeaway
                "",
                icon = tags$i(class = "fa-light fa-key",
                              style = "font-size: 30px; text-align: center;"),
                class = "link-no-underline action-link-2"),
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
                class = "link-no-underline action-link-2"),
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
                class = "link-no-underline action-link-2"),
              span(class = "hover-text-content-side", "Definitions")
            ),
            hr(class = "right-hr")
          ),

          div(
            class = "row color-block-content",

            # toggle side content -----------------------------------------------------
            shinyjs::hidden(
              div(
                id = ns("toggle_get_services"),
                class = "context-content",
                uiOutput(ns("show_get_services"))
              )
            ),

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



#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # update inputs based on selections ---------------------------------------
    observe({
      if (input$topic != "Reduced/Expanded Services") {
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
                                                HTML('<i class="fas fa-chart-scatter"></i>'),
                                                HTML('<i class="fas fa-chart-bar"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Scatter Plot", "Butterfly Graph", "Table"))
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
      }
    })

    colors_scatter <- reactive ({
      if (input$filter == "urbanrural2022") {
        c("#1A2545",
          "#2C6A77")
      } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
        c("#1A2545",
          "#2C6A77",
          "#7A3C23")
      } else if (input$filter == "censusregion") {
        c("#1A2545",
          "#2C6A77",
          "#7A3C23",
          "#B090B1")
      }
    })


    # main content ============================================================

    # inputs for each topic ---------------------------------------------------
    updated_inputs <- reactive({
      input$topic
      input$filter
      input$services
    }) %>% debounce(100)

    service_change_inputs <- reactive({
      input$topic
      input$filter
      input$service_changes
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
      data <- services_data %>%
        dplyr::filter(service_category == "Immunization") %>%
        dplyr::filter(filter == "All LHDs") %>%
        dplyr::filter(service_provision == "LHD (Directly)") %>%
        dplyr::mutate(group = filter,
                      category = service_type) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(x = (1:length(group))-1) %>%
        dplyr::ungroup()

      set_simple_bar(data) %>%
        viz_options(
          viz_title = "Estimated Percent of LHDs that Provide Services Directly",
          viz_subtitle = "Immunization Services", # unique(data$service_category)
          viz_caption = paste0("<b> N: </b>",  min(data$demoninator), "-", max(data$demoninator), "<br>", source_line),
          tooltip = "",
          theme = naccho.viz::butterfly,
          legend_enable = FALSE,
          colors = naccho.viz::green
        ) %>%
        viz_export()
    })


    # used for line graph
    output$figure_line <- highcharter::renderHighchart({
      # data <- services_data %>%
      #   dplyr::filter(service_category %in% c(input$services)) %>%
      #   dplyr::filter(service_provision %in% c("Performed Directly by LHD", "Contracted Out by LHD"))
    })


    # used for scatter plot
    output$figure_scatter <- highcharter::renderHighchart({
      data <- scatter_data(set_filter = "All LHDs",
                           set_service = change_classifications) %>%
        dplyr::mutate(Group = sapply(Group, custom_str_to_title))

      scatter_plot(data, colors = "#6D833A")
    })


    # used for scatter plot
    output$figure_butterfly <- highcharter::renderHighchart({
      # data <- services_change_data %>%
      #   dplyr::filter(service_provision %in% c("Reduced Service", "Expanded Service"))
    })


    # proxy -------------------------------------------------------------------
    # save the figure as a proxy to redraw
    proxy_bar <- highcharter::highchartProxy(ns("figure_bar"))
    proxy_line <- highcharter::highchartProxy(ns("figure_line"))
    proxy_scatter <- highcharter::highchartProxy(ns("figure_scatter"))
    proxy_butterfly <- highcharter::highchartProxy(ns("figure_butterfly"))



    # update proxy ------------------------------------------------------------
    observeEvent(updated_inputs(), {

      if (input$topic != "Reduced/Expanded Services" & input$view == "Bar Graph" & input$filter == "All LHDs") {
        req(length(input$services) >= 1) # prevents error

        data <- services_data %>%
          dplyr::filter(service_category %in% c(input$services),
                        filter == "All LHDs",
                        service_provision == input$topic) %>%
          dplyr::mutate(group = Group,
                        category = service_type) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(x = (1:length(group))-1,
                        x_label = "") %>%
          dplyr::ungroup()


        # nested
        if (length(data$x) >= 8) {
          size = "13px"
          overflow = "ellipsis"
        } else {
          size = "17px"
          overflow = "none"
        }

        proxy_bar %>%
          proxy_hist_to_single(data,
                          meta = filter_meta(),
                          size = size,
                          overflow = overflow)



      } else if (input$topic != "Reduced/Expanded Services" & input$view == "Bar Graph" & input$filter != "All LHDs") {
        req(length(input$services) >= 1) # prevents error

        data <- services_data %>%
          dplyr::filter(service_category %in% c(input$services),
                        filter == input$filter,
                        service_provision == input$topic) %>%
          dplyr::mutate(group = sapply(Group, custom_str_to_title),
                        category = service_type) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(x = (1:length(group))-1,
                        x_label = "") %>%
          dplyr::ungroup()

        data_all <- services_data %>%
          dplyr::filter(service_category %in% c(input$services),
                        filter == "All LHDs",
                        service_provision == input$topic) %>%
          dplyr::mutate(group = Group,
                        category = service_type) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(x = (1:length(group))-1,
                        x_label = "") %>%
          dplyr::ungroup()



        # nested
        if (length(data$x) >= 8) {
          size = "13px"
          overflow = "ellipsis"
        } else {
          size = "17px"
          overflow = "none"
        }

        series_id <- unique(data$group) # provide ID for proxy

        proxy_bar %>%
          proxy_hist_to_group(data = data,
                              data_all = data_all,
                              meta = filter_meta(),
                              colors = colors(),
                              id = series_id,
                              responses = "",
                              size = size,
                              overflow = overflow)


      } else if (input$topic != "Reduced/Expanded Services" & input$view == "Line Graph") {
        #ADD

      }
    })



    observeEvent(service_change_inputs(), {
      if (input$topic == "Reduced/Expanded Services" & input$view == "Scatter Plot" & input$filter != "All LHDs") {

        data <- scatter_data(set_filter = input$filter,
                             set_service = input$service_changes) %>%
          dplyr::mutate(Group = sapply(Group, custom_str_to_title))

        series_id <- unique(data$Group)

        proxy_scatter %>%
          proxy_to_scatter_group(data = data,
                           input = input$filter,
                           id = series_id,
                           colors = colors_scatter())



      } else if (input$topic == "Reduced/Expanded Services" & input$view == "Scatter Plot" & input$filter == "All LHDs") {

        req(input$service_changes)
        data <- scatter_data(set_filter = "All LHDs",
                                 set_service = input$service_changes) %>%
          dplyr::mutate(Group = sapply(Group, custom_str_to_title))

        proxy_scatter %>%
          proxy_to_scatter_single(data)

        } else if (input$topic == "Reduced/Expanded Services" & input$view == "Butterfly Graph") {

      }
    })



    # right content ===========================================================

    observeEvent(input$topic, {
        shinyjs::addClass(selector = "#services_1-dynamic_row", class = "show-icon")
        session$sendCustomMessage(type = "triggerClickService", message = "services_1-get_services")
    })

    # show services by cateogry  ---------------------------------------
    observeEvent(input$get_services, {
      shinyjs::show("toggle_get_services")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_get_services <- renderUI({

        if (input$topic != "Reduced/Expanded Services") {
          shinyWidgets::prettyRadioButtons(
            ns("services"),
            strong("Select Service Topic"),
            service_classifications,
            selected = "Immunization"
          )
        # } else if (input$topic == "Reduced/Expanded Services" & input$filter != "All LHDs") {
        #   shinyWidgets::prettyRadioButtons(
        #     ns("service_changes_single"),
        #     strong("Select Service Topic"),
        #     change_classifications,
        #     selected = "Epidemiology and Surveillance"
        #   )
        } else {
          checkboxGroupInput(
            ns("service_changes"),
            strong("Select Service Topic"),
            change_classifications,
            selected = change_classifications
          )

        }
      })
    }, ignoreNULL = FALSE)


    # takeaway ----------------------------------------------------------------
    observeEvent(input$takeaway, {
      shinyjs::hide("toggle_get_services")
      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_takeaway <- renderUI({
        req(!is.null(input$topic))
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view,
                        filter == input$filter)

        p(htmltools::HTML(
          "<b>Key Takeaway(s)</b>: <br><br>",
          notes$takeaway))
      })
    }, ignoreNULL = FALSE)



    # survey questions, responses, and notes ----------------------------------
    observeEvent(input$context, {
      shinyjs::hide("toggle_get_services")
      shinyjs::hide("toggle_takeaway")
      shinyjs::show("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_context <- renderUI({
        req(!is.null(input$topic))
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        view == input$view,
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
            notes$notes))
        }
      })
    })



    # definitions -------------------------------------------------------------
    observeEvent(input$definitions, {
      shinyjs::hide("toggle_get_services")
      shinyjs::hide("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::show("toggle_definitions")

      output$show_definitions <- renderUI({
        definitions <- definitions %>%
          dplyr::filter(field %in% input$filter)

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
          links
        )
      })
    })

  })
}

## To be copied in the UI
# mod_services_ui("services_1")

## To be copied in the server
# mod_services_server("services_1")
