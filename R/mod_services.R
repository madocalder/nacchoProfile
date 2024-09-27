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
              highcharter::highchartOutput(ns("figure_butterfly"), height = 675)
            ),

            # display when view Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              uiOutput(ns("table_title")),
              reactable::reactableOutput(ns("data_table"), height = "auto"),
              br(),
              HTML('<div style="font-size: 13px;">', source_line,'
                   <br> <b> N: </b> Hover over a column’s header to view the respective N count. </div>')
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


    observe({
      if (input$topic == "Reduced/Expanded Services" && input$view == "Butterfly Graph") {
        updateSelectInput(session, "filter",
                          choices = "All LHDs")
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

    #table titles---------------------------------------------------------------
    output$table_title <- renderUI({
      # Ensure input$topic exists before using it
      req(input$topic)

      # Subset based on the reactive input$topic and convert to character
      title_values <- as.character(subset(meta, field == input$topic)$title)

      # Render the HTML
      p(title_values, style = "font-size: 20px; font-weight: bold !important; text-align: center;")
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


    service_change_single_inputs <- reactive({
      input$topic
      input$filter
      input$service_changes_single
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
          viz_title = "Percent of LHDs that Directly Provide Selected Services",
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


    # used for butterfly plot
    output$figure_butterfly <- highcharter::renderHighchart({
      data <- scatter_data(set_filter = c(landscape_filters),
                            set_service = "Immunization") %>%
        dplyr::mutate(group = sapply(Group, custom_str_to_title),
                      left = `Reduced Service`,
                      right = `Expanded Service`,
                      x = (1:length(Group)-1)) %>%
        dplyr::mutate(group = ifelse(group == 13, "All LHDs", group))

      data_n <- data %>%
        dplyr::filter(group == "All LHDs")

      set_bar(data,
              set_max = 100,
              name_one = "Service Shrunk",
              name_two = "Service Grew",
              set_format = "{value}%") %>%
        viz_options(
          colors = c(naccho.viz::terracotta, naccho.viz::green),
          legend_enable = TRUE,
          viz_title = "The Percent of LHDs that Shrunk or Grew Selected Service",
          viz_caption = paste0("<b> N: </b>", data_n$demoninator, "<br>", source_line),
          viz_subtitle = paste(unique(data$service_type)),
          tooltip = "<b> {series.name}: </b>
              {point.y:.0f}% <br>",
          theme = naccho.viz::butterfly
        ) %>%
        viz_export() %>%
        highcharter::hc_tooltip(
          backgroundColor = "transparent",
          headerFormat = "<b>{point.x:.0f} LHDs </b> <hr class = 'tt'>",
          style = list(
            textAlign = "left"
          ),
          hideDelay = 2000,
          shadow = FALSE,
          positioner = highcharter::JS("
          function (labelWidth, labelHeight, point) {
            return {
              x: 0,
              y: 10
            };
          }
        ")
        )
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
                          overflow = overflow,
                          responses = paste0(min(data$demoninator), "-", max(data$demoninator)))


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
                              size = size,
                              overflow = overflow,
                              responses = paste0(min(data_all$demoninator), "-", max(data_all$demoninator)))


      } else if (input$topic != "Reduced/Expanded Services" & input$view == "Line Graph") {
        #ADD
      }
    })


    observeEvent(service_change_inputs(), {
      if (input$topic == "Reduced/Expanded Services" & input$view == "Scatter Plot" & input$filter != "All LHDs") {

        data <- scatter_data(set_filter = input$filter,
                             set_service = input$service_changes) %>%
          dplyr::mutate(Group = sapply(Group, custom_str_to_title))

        data_all <- scatter_data(set_filter = "All LHDs",
                             set_service = input$service_changes) %>%
          dplyr::mutate(Group = sapply(Group, custom_str_to_title))


        series_id <- unique(data$Group)

        proxy_scatter %>%
          proxy_to_scatter_group(data = data,
                                 data_all = data_all,
                                 input = input$filter,
                                 id = series_id,
                                 colors = colors())


      } else if (input$topic == "Reduced/Expanded Services" & input$view == "Scatter Plot" & input$filter == "All LHDs") {
        req(input$service_changes)
        data <- scatter_data(set_filter = "All LHDs",
                                 set_service = input$service_changes) %>%
          dplyr::mutate(Group = sapply(Group, custom_str_to_title))

        proxy_scatter %>%
          proxy_to_scatter_single(data)
        }
    })



    observeEvent(service_change_single_inputs(), {
      if (input$topic == "Reduced/Expanded Services" & input$view == "Butterfly Graph") {
        data <- scatter_data(set_filter = c(landscape_filters),
                             set_service = input$service_changes_single) %>%
          dplyr::mutate(group = sapply(Group, custom_str_to_title),
                        left = `Reduced Service`,
                        right = `Expanded Service`,
                        x = (1:length(Group)-1)) %>%
          dplyr::mutate(group = ifelse(group == 13, "All LHDs", group))

        data_n <- data %>%
          dplyr::filter(group == "All LHDs")

        proxy_butterfly %>%
          highcharter::hcpxy_update(
            series = list(
              list(id = "Left", data = data$left),  # Update Left series data
              list(id = "Right", data = data$right) # Update Right series data
            ),
            subtitle = list(
              text = paste(unique(data$service_type))
            ),
            caption = list(
              text = paste0("<b> N: </b>", data_n$demoninator, "<br>", source_line)
            )
          )
      }
    })

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

      #debug
      message("input$services =")
      print(input$services)
      message("input$service_changes =")
      print(input$service_changes)

      message("classifications variable =")
      print(classifications)



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

      print(data)

      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic)
    }) #end of tables output code



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
          div(
            class = "radio-wrap",
            radioButtons(
              ns("services"),
              strong("Select Service Topic"),
              service_classifications,
              selected = "Immunization"
            )
          )
        } else if (input$topic == "Reduced/Expanded Services" & input$view == "Butterfly Graph") {
          div(
            class = "radio-wrap",
            radioButtons(
              ns("service_changes_single"),
              strong("Select Service Topic"),
              change_classifications,
              selected = "Immunization"
            )
          )
        } else {
          checkboxGroupInput(
            ns("service_changes"),
            strong("Select One or More Topic"),
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
# mod_services_ui("services_1")

## To be copied in the server
# mod_services_server("services_1")
