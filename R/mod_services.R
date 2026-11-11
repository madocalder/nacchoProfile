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
          class = "main-content",
          openMenu(),

            div(
              class = "topic-select",

              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = service_topics,
                selectize = TRUE
              )
            ),
            div(
              class = "filter-select",

              selectInput(
                inputId = ns("filter"),
                strong("Filter"),
                choices = landscape_filters,
                selectize = TRUE
              )
            ),


          # viz toggles here --------------------------------------------------
          div(
            class = "viz-view",
            shinyWidgets::radioGroupButtons(
              ns("view"),
              "",
              choiceNames = list(
                HTML('<i class="fas fa-chart-simple"></i>'),
                HTML('<i class="fas fa-table"></i>')
              ),
              choiceValues = c("Bar Graph", "Table"),
              selected = "Table"
            )
          ),


          # main page content ----------------------------------------------
          div(
            class = "viz-content",

            # display when Service Provision and Bar Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] != 'Reduced/Expanded Services' &&
                                 input['", ns('view'), "\'] == 'Bar Graph'"),
              # div(
                highcharter::highchartOutput(ns("figure_bar"), height = "100%")

              #   , #675
              # style = "height: 60vh; width: auto; margin-left: auto !important;
              #           margin-right: auto !important;")
            ),


            # # display when Service Provision and Line Graph are selected
            # conditionalPanel(
            #   condition = paste0('input[\'', ns('topic'), "\'] != 'Reduced/Expanded Services' && input['", ns('view'), "\'] == 'Line Graph'"),
            #   # highcharter::highchartOutput(ns("figure_line"), height = 575)
            # ),

            # display when Service Changes and Scatter Plot are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == 'Reduced/Expanded Services'
                                 && input['", ns('view'), "\'] == 'Scatter Plot'"),
              # div(
              highcharter::highchartOutput(ns("figure_scatter"), height = "100%")
              # , #775
              # style = "height: 70vh; width: auto;")
            ),

            # display when Service Changes and Butterfly Graph are selected
            conditionalPanel(
              condition = paste0('input[\'', ns('topic'), "\'] == 'Reduced/Expanded Services'
                                 && input['", ns('view'), "\'] == 'Butterfly Graph'"),
              # div(
                highcharter::highchartOutput(ns("figure_butterfly"), height = "100%"),
              style = "height: 100%"
            ),

            # display when view Table is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              div( class = "table-grid",

                div(class = "table-title-grid",

                    uiOutput(ns("table_title")),
                    csvDownloadButton(ns("data_table"), filename = "servicesNACCHO.csv")
                ),

                reactable::reactableOutput(ns("data_table"),
                                           height = "100%",
                                           width = "auto"
                                           ),

                HTML('<div style="font-size: calc(5px + .2em + .8vmin);">', source_line, '<br> <b> N: </b> Hover over a column\'s header to view the respective N count. </div>')

              ),
              style = "height: 100%"

)
)

        ),

     # end of main content -----------------------------------------------------
# right pane begins--------
right = div(
  div(
    class = "row color-block-header",

    # Define the default state with all panels hidden
    div(
      class = "col hover-text-side",
      align = "center",
      br(),
      actionLink(
        ns("get_services"),
        "",
        icon = tags$i(class = "fa-light fa-filter",
                      style = "font-size: calc(28px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Add Services")
    ),
    div(
      class = "col hover-text-side",
      align = "center",
      br(),
      actionLink(
        ns("takeaway"),
        "",
        icon = tags$i(class = "fa-light fa-key",
                      style = "font-size: calc(28px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Takeaways")
    ),
    div(
      class = "col hover-text-side",
      align = "center",
      br(),
      actionLink(
        ns("context"),
        "",
        icon = tags$i(class = "fa-light fa-circle-info", style = "font-size: calc(28px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Context")
    ),
    div(
      class = "col hover-text-side",
      align = "center",
      br(),
      actionLink(
        ns("definitions"),
        "",
        icon = tags$i(class = "fa-light fa-list", style = "font-size: calc(28px + .2em); text-align: center;"),
        class = "link-no-underline action-link-2"
      ),
      span(class = "hover-text-content-side", "Definitions")
    ),
    hr(class = "right-hr")
  ),

  # Add placeholders for the toggled content
  div(
    class = "row color-block-content",
    # Right panel setup in the UI
    # Services panel (filter) visible by default
    div(
      id = ns("toggle_get_services"),
      class = "context-content",
      # p(htmltools::HTML("<b>Select Service(s):</b>")),
      uiOutput(ns("services_input"))
    ),
    shinyjs::hidden(
      div(id = ns("toggle_takeaway"), class = "context-content", uiOutput(ns("show_takeaway")))
    ),
    shinyjs::hidden(
      div(id = ns("toggle_context"), class = "context-content", uiOutput(ns("show_context")))
    ),
    shinyjs::hidden(
      div(id = ns("toggle_definitions"), class = "context-content", uiOutput(ns("show_definitions")))
    )
  ),
  div(class = "row color-block-footer")
),

        right_button = right_button,
        left_width = 25,
        right_width = 25,
        center_width = 50
      )
    )
  )
}



#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Input Updates ==========================================================
    observe({
      req(input$topic, input$view)

      topic <- input$topic
      selected_view <- input$view


      if (input$topic != "Reduced/Expanded Services") {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-simple"></i>'),
            HTML('<i class="fas fa-table"></i>')
          ),
          choiceValues = c("Bar Graph", "Table"),
          selected = dplyr::case_when(
            selected_view %in% c("Bar Graph", "Table") ~ selected_view,
            TRUE ~ "Bar Graph")
        )
      } else {
        shinyWidgets::updateRadioGroupButtons(
          session, "view",
          choiceNames = list(
            HTML('<i class="fas fa-chart-scatter"></i>'),
            # HTML('<i class="fas fa-chart-bar"></i>'),
            HTML('<i class="fas fa-table"></i>')
          ),
          choiceValues = c("Scatter Plot", "Table"),
          selected = dplyr::case_when(
            selected_view %in% c("Scatter Plot", "Table") ~ selected_view,
            TRUE ~ "Scatter Plot")
        )
      }
    })

    observe({
      req(input$topic, input$view, input$filter)

      topic <- input$topic
      selected_view <- input$view
      selected_filter <- input$filter

      if (topic == "Reduced/Expanded Services") {
        updateSelectInput(
          session, "filter",
          choices = landscape_filters[1]
        )
      }
      else {
        updateSelectInput(
          session, "filter",
          choices = landscape_filters,
          selected = selected_filter
        )
      }
    })

    # Dynamically switch between single-select and multi-select
    observe({
      req(input$topic, input$view)

      if (input$topic == "Reduced/Expanded Services") {


        # Multi-select using checkboxes
        output$services_input <- renderUI({

          if (input$view == "Scatter Plot") {
          checkboxGroupInput(
            inputId = ns("services"),
            label = HTML('<b>Select Service(s):</b>'),
            choices = service_cats,  # List of services
            selected = service_cats[1],
            width = "100%"
          )


        } else {

          selectInput(inputId = ns("services"),
                      label = HTML('<b>Select Service:</b>'),
                      multiple = FALSE,
                      choices = service_cats,  # List of services
                      selected = service_cats[1],
                      width = "100%"
          )


        }
        })




      } else {
        # Single-select using radio buttons
        output$services_input <- renderUI({
          # radioButtons(
          #   inputId = ns("services"),
          #   label = HTML('<b>Select Service:</b>'),
          #   choices = service_classifications,  # List of services
          #   selected = service_classifications[1],  # Default first service
          #   width = "100%"
          # )

          selectInput(inputId = ns("services"),
                      label = HTML('<b>Select Service:</b>'),
                      choices =  service_classifications,
                      multiple = FALSE,
                      selected =  service_classifications[1]
                      )
        })
      }
    })





    # observe({
    #   updateSelectInput(session, "view", selected = "Table")
    #   updateSelectInput(session, "filter", selected = landscape_filters[1])
    #   updateSelectInput(session, "services", selected = service_classifications[1])
    #   updateSelectInput(session, "topic", selected = service_topics[[1]][1])
    # })

    # Reactive Color Definitions =============================================
    colors <- reactive({
      if (input$filter == "urbanrural2022") {
        c(naccho.viz::navy, naccho.viz::teal)
      } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
        c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta)
      } else if (input$filter == "censusregion") {
        c(naccho.viz::navy, naccho.viz::teal, naccho.viz::terracotta, naccho.viz::pink)
      } else {
        naccho.viz::green
      }
    })

    # Debounced Inputs =======================================================
    updated_inputs <- reactive({
      input$topic
      input$filter
      input$services
    }) %>% debounce(100)

    # service_change_inputs <- reactive({
    #   input$topic
    #   input$filter
    #   input$services
    # }) %>% debounce(100)


    # Metadata Filtering =====================================================
    filter_meta <- reactive({
      # req(updated_inputs())
      req(!is.null(input$topic), !is.null(input$filter), !is.null(input$view))
      meta %>%
        dplyr::filter(field == input$topic,
                      stringr::str_detect(view, input$view),
                      stringr::str_detect(filter, input$filter))

    })

    # Outputs ================================================================

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

    # Data Table Output
    output$data_table <- reactable::renderReactable({
      req(updated_inputs())
      req(input$view == "Table", input$topic, input$filter) # Ensure the table is only rendered for the "Table" view

      # Define topic and filtering based on input
      topic <- input$topic
      filtering <- input$filter

      # Determine classifications based on service changes or services inputs
      classifications <- input$services

      # Debug messages (optional for troubleshooting)
      # message("input$topic =", input$topic)
      # message("topic variable =", topic)
      # message("input$filter =", input$filter)
      # message("filtering variable =", filtering)
      # message("classifications variable =", classifications)

      # Process the data for the table based on the topic, filter, and classifications
      data <- process_table_data(topic, filtering, classifications)
      # %>%
      #   dplyr::mutate(
      #     dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
      #   )

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

      # Generate the table using a custom table creation function
      create_non_time_table(data, category, colors, topic)
    })

    output$figure_bar <- highcharter::renderHighchart({
      # req(updated_inputs())
      req(input$topic, input$filter, input$services)
      req(filter_meta())



      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      topic <- input$topic
      services <- input$services
      filtering <- input$filter

      data <- services_data %>%
        dplyr::filter(
          service_classification %in% services,
          filter == filtering,
          service_provision == topic,
          category %in% c("Checked", "1")
        ) %>%
        dplyr::mutate(category = service_type) %>%
        dplyr::group_by(Group) %>%
        dplyr::mutate(x = seq_along(category) - 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(group_tt = dplyr::case_when(
          group != "All LHDs" & group != "censusregion" ~ tolower(group),
           group == "censusregion" ~ paste0(tolower(group), "ern"),
          TRUE ~ "all"
        ),
        category_tt = dplyr::case_when(service_type == "Air Polution" ~ "services to address air polution",
                                       service_type == "COVID-19 Adult Immunizations" ~ "COVID-19 adult immunizations",
                                       service_type == "COVID-19 Child Immunizations" ~ "COVID-19 child immunizations",
                                       service_type == "BMI Screening" ~ "BMI screening",
                                       service_type == "HIV/AIDs Screening" ~ "HIV/AIDs screening",
                                       service_type == "Other STDs Screening"   ~ "other STD screening" ,
                                       service_type == "HIV/AIDs Treatment"   ~ "HIV/AIDs treatment" ,
                                       service_type ==  "Other STD Treatment"   ~  "other STD treatment" ,
                                       service_type == "EPSDT" ~ "early and periodic screening, diagnostic and treatment (EPSDT)",
                                       service_type == "WIC" ~ "WIC",
                                       service_type == "Behavioral Risk Factors" ~ "behavioral risk factor surveillance services",
                                       service_type == "Chronic Disease"  ~ "chronic disease surveillance services",
                                       service_type == "Communicable/Infectious Disease "  ~ "communicable/infectious disease surveillance services",
                                       service_type == "Maternal and Child Health" ~ "maternal and child health surveillance services",
                                       service_type == "Mental Illness" ~ "mental illness prevention activities",
                                       service_type == "Nutrition" ~ "nutrition services",
                                       service_type == "Opioids" ~ "opioid prevention activities",
                                       service_type == "Injury" ~ "injury prevention activities",
                                       service_type == "Physical Activity" ~ "physical activity services",
                                       service_type == "Substance Abuse Other Than Opioids" ~ "substance abuse (other than opioids) prevention activities",
                                       service_type == "Tobacco" ~ "tobacco prevention activities",
                                       service_classification ==  "Regulation, Inspection, Licensing" & service_type != "Lead Inspections"  ~  paste(tolower(service_type), "regulation, inspection, and/or licensing activities"),
                                       service_classification ==  "Regulation, Inspection, Licensing" & service_type == "Lead Inspections"  ~  "lead inspection activities",
                                       service_type == "Hazmat Response" ~ "hazmat response services",
                                       service_type == "Indoor Air Quality" ~ "indoor air quality control/improvement services",
                                       service_type == "Noise Pollution"  ~ "noise polution control/improvement services",
                                       service_type == "Radiation Control"   ~ "radiation control services",
                                       service_type ==  "Vector Control"    ~ "vector control services",
                                       TRUE ~ tolower(service_type)

                                       )
        )

      data_n <- services_data %>%
        dplyr::filter(
          service_classification %in% services,
          filter == filtering,
          service_provision == topic
        ) %>%
        dplyr::group_by(service_type) %>%
        dplyr::summarize(Responses = sum(Responses, na.rm = TRUE)) %>%
        dplyr::ungroup()

     bar_chart <- set_column(data,
                 y_label = filter_meta()$y_label,
                 y_format = filter_meta()$y_format,
                 data_label = filter_meta()$data_label) %>%
        viz_options(
          viz_title = filter_meta()$title,
          viz_subtitle = filter_meta()$subtitle,
          viz_caption = paste0("<b> N: </b>", show_n(data_n$Responses), "<br>", source_line),
          caption_align = "center",
          colors = colors(),
          theme = naccho.viz::basic,
          tooltip = filter_meta()$tooltip
        )%>%
        viz_export()

     if (services ==  "Epidemiology and Surveillance") {
       bar_chart <- bar_chart %>%
         highcharter::hc_xAxis(
           labels = list(
             rotation = -45,
             overflow = "allow"
           )
         )
     }

     bar_chart
    })


    proxy_bar <- highcharter::highchartProxy(ns("figure_bar"))

    observeEvent(list(input$topic, input$filter, input$services), {
      req(input$topic, input$filter, input$services, input$view == "Bar Graph")

      req(input$topic, input$filter, input$services)
      req(filter_meta())



      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      topic <- input$topic
      services <- input$services
      filtering <- input$filter

      data <- services_data %>%
        dplyr::filter(
          service_classification %in% services,
          filter == filtering,
          service_provision == topic,
          category %in% c("Checked", "1")
        ) %>%
        dplyr::mutate(category = service_type) %>%
        dplyr::group_by(Group) %>%
        dplyr::mutate(x = seq_along(category) - 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(group_tt = dplyr::case_when(
          group != "All LHDs" & group != "censusregion" ~ tolower(group),
          group == "censusregion" ~ paste0(tolower(group), "ern"),
          TRUE ~ "all"
        ),
        category_tt = dplyr::case_when(service_type == "Air Polution" ~ "services to address air polution",
                                       service_type == "COVID-19 Adult Immunizations" ~ "COVID-19 adult immunizations",
                                       service_type == "COVID-19 Child Immunizations" ~ "COVID-19 child immunizations",
                                       service_type == "BMI Screening" ~ "BMI screening",
                                       service_type == "HIV/AIDs Screening" ~ "HIV/AIDs screening",
                                       service_type == "Other STDs Screening"   ~ "other STD screening" ,
                                       service_type == "HIV/AIDs Treatment"   ~ "HIV/AIDs treatment" ,
                                       service_type ==  "Other STD Treatment"   ~  "other STD treatment" ,
                                       service_type == "EPSDT" ~ "early and periodic screening, diagnostic and treatment (EPSDT)",
                                       service_type == "WIC" ~ "WIC",
                                       service_type == "Behavioral Risk Factors" ~ "behavioral risk factor surveillance services",
                                       service_type == "Chronic Disease"  ~ "chronic disease surveillance services",
                                       service_type == "Communicable/Infectious Disease "  ~ "communicable/infectious disease surveillance services",
                                       service_type == "Maternal and Child Health" ~ "maternal and child health surveillance services",
                                       service_type == "Mental Illness" ~ "mental illness prevention activities",
                                       service_type == "Nutrition" ~ "nutrition services",
                                       service_type == "Opioids" ~ "opioid prevention activities",
                                       service_type == "Injury" ~ "injury prevention activities",
                                       service_type == "Physical Activity" ~ "physical activity services",
                                       service_type == "Substance Abuse Other Than Opioids" ~ "substance abuse (other than opioids) prevention activities",
                                       service_type == "Tobacco" ~ "tobacco prevention activities",
                                       service_classification ==  "Regulation, Inspection, Licensing" & service_type != "Lead Inspections"  ~  paste(tolower(service_type), "regulation, inspection, and/or licensing activities"),
                                       service_classification ==  "Regulation, Inspection, Licensing" & service_type == "Lead Inspections"  ~  "lead inspection activities",
                                       service_type == "Hazmat Response" ~ "hazmat response services",
                                       service_type == "Indoor Air Quality" ~ "indoor air quality control/improvement services",
                                       service_type == "Noise Pollution"  ~ "noise polution control/improvement services",
                                       service_type == "Radiation Control"   ~ "radiation control services",
                                       service_type ==  "Vector Control"    ~ "vector control services",
                                       TRUE ~ tolower(service_type)

        )
        )

      data_n <- services_data %>%
        dplyr::filter(
          service_classification %in% services,
          filter == filtering,
          service_provision == topic
        ) %>%
        dplyr::group_by(service_type) %>%
        dplyr::summarize(Responses = sum(Responses, na.rm = TRUE)) %>%
        dplyr::ungroup()

      proxy_bar %>%
        highcharter::hcpxy_update(data = data)

      if (services ==  "Epidemiology and Surveillance") {
        proxy_bar %>%
          highcharter::hcpxy_update(
            xAxis = list(
            labels = list(
              rotation = -45,
              overflow = "allow"
            )
            )
          )
      }




    })


    output$figure_scatter <- highcharter::renderHighchart({
      req(updated_inputs())
      req(input$topic, input$filter, input$services)

      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      services <- input$services
      filtering <- input$filter


      data <- scatter_data(set_filter = filtering, set_service = services)

      scatter_plot(data, colors = colors())%>%
        viz_export()

    })

    proxy_scatter <- highcharter::highchartProxy(ns("figure_scatter"))

    observeEvent(list(input$topic, input$filter, input$services), {
      req(input$topic, input$filter, input$services, input$view == "Scatter Plot")

      # Debug messages for tracking input values
      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      # Extract inputs
      services <- input$services
      filtering <- input$filter

      # Update the scatter plot
      data <- scatter_data(set_filter = filtering, set_service = services)

      # Send updated data to the scatter plot
      proxy_scatter %>%
        highcharter::hcpxy_update(
        id = "figure_scatter", # The ID of the highchart proxy
        data = data
      )

    })

    output$figure_butterfly <- highcharter::renderHighchart({
      req(updated_inputs())
      req(input$services)


      message("input$topic = ", input$topic)
      message("input$filter = ", input$filter)
      message("input$services = ", input$services)

      services <- input$services

      data <- scatter_data(set_filter = landscape_filters,
                           set_service = services) %>%
        dplyr::mutate(
          left = x,
          right = y,
          x = seq_along(unique(group)) - 1
        ) %>%
        dplyr::mutate(group = ifelse(group == 13, "All LHDs", group))

      data_n <- data %>%
        dplyr::filter(filter == "All LHDs")

      max_one <- max(data$left)
      max_two <- max(data$right)

      set_bar(data,
              y_label = "",
              max_one = max_one,
              max_two = max_two,
              name_one = "Shrunk Service",
              name_two = "Grew Service",
              y_format = "{value}%") %>%
        viz_options(
          colors = c(naccho.viz::terracotta, naccho.viz::green),
          legend_enable = TRUE,
          legend_title = "% of LHDs that...",
          viz_title = "Percent of LHDs that Shrunk or Grew Selected Service",
          viz_caption = paste0("<b> N: </b>", show_n(data_n$Responses), "<br>", source_line),
          viz_subtitle = paste(unique(data$service_type)),
          tooltip = "<b> {series.name}: </b>{point.y:.0f}% <br>",
          theme = naccho.viz::butterfly
        )%>%
        viz_export()
    })

    # Event Handling =========================================================

    # Show Filter Services
    observeEvent(input$get_services, {
      shinyjs::show("toggle_get_services")  # Show the Filter Services UI
      shinyjs::hide("toggle_takeaway")      # Hide Takeaways
      shinyjs::hide("toggle_context")       # Hide Notes
      shinyjs::hide("toggle_definitions")   # Hide Definitions

    })


    #takeaway ----
    observeEvent(input$takeaway, {
      # shinyjs::hide("toggle_get_services")
      # shinyjs::show("toggle_takeaway")
      # shinyjs::hide("toggle_context")
      # shinyjs::hide("toggle_definitions")

      toggle_sidebar_content("toggle_takeaway",
                            c("toggle_takeaway", "toggle_context",
                              "toggle_definitions", "toggle_get_services"))


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

    #context ----

    observeEvent(input$context, {
      # shinyjs::hide("toggle_get_services")
      # shinyjs::hide("toggle_takeaway")
      # shinyjs::show("toggle_context")
      # shinyjs::hide("toggle_definitions")
      toggle_sidebar_content("toggle_context",
                             c("toggle_takeaway", "toggle_context",
                               "toggle_definitions", "toggle_get_services"))

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


    #definitions------

    observeEvent(input$definitions, {

      toggle_sidebar_content("toggle_definitions",
                             c("toggle_takeaway", "toggle_context",
                               "toggle_definitions", "toggle_get_services"))

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
