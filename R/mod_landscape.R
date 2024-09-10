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
    tags$head(
      tags$script(src = "www/script.js") # Correct path if the file is in inst/app/www/
    ),
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
                choices = landscape_filters[4]
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
                HTML('<i class="fas fa-earth-americas"></i>')         # Map icon
              ),
              choiceValues = c('Bar Graph', 'Line Graph', 'Map')
            )
          ),


          # add main page content here ----------------------------------------
          div(
            class = "viz-content",

            # display when map is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Map\'"),
              br(),
              br(),
              highcharter::highchartOutput(ns("map"), height = 675)
            ),

            # display when map is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), '\'] != \'Map\' && input[\'', ns('view'), '\'] != \'Table\''),
              br(),
              br(),
              highcharter::highchartOutput(ns("figure"), height = 575)
            ),

            # adding table
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Table\'"),
              reactable::reactableOutput(ns("data_table"), height = "auto"),
              br(),
             HTML(source_line)
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
                                style = "font-size: 30px; text-align: center;"),
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
                                style = "font-size: 30px; text-align: center;"),
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
                                style = "font-size: 30px; text-align: center;"),
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
        )
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
    # for example, if someone selects "geography," then the view will update to
    # include a map options.
    observe({
      if (input$topic == "tenure") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[-1])


      } else if (input$topic == "totpop") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[4])


      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters)
      }
    })

    observe({
      req(input$topic == "c0govcatn")

        shinyWidgets::updateRadioGroupButtons(session, "view",
                                              choiceNames = list(
                                                HTML('<i class="fas fa-earth-americas"></i>'),
                                                HTML('<i class="fas fa-chart-simple"></i>'),
                                                HTML('<i class="fas fa-chart-line"></i>'),
                                                HTML('<i class="fas fa-table"></i>')
                                              ),
                                              choiceValues = c("Map", "Bar Graph", "Line Graph", "Table"))

      if (input$view == "Map") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[1])

      } else if (input$view != "Map") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[-3])
      }
    })

    observe({
      req(input$topic != "c0govcatn")
      shinyWidgets::updateRadioGroupButtons(session, "view",
                                            choiceNames = list(
                                              HTML('<i class="fas fa-chart-simple"></i>'),
                                              HTML('<i class="fas fa-chart-line"></i>'),
                                              HTML('<i class="fas fa-table"></i>')
                                            ),
                                            choiceValues = c("Bar Graph", "Line Graph", "Table"))
    })


    # filter meta data --------------------------------------------------------
    # used to display the correct title, caption, labels, etc...
    filter_meta <- reactive({
      meta %>%
        dplyr::filter(field == input$topic)
    })



    # map output --------------------------------------------------------------
    # used for the map only, displayed in the UI using a conditional panel
    output$map <- highcharter::renderHighchart({

      req(input$filter == "All LHDs") # needed to prevent glitch

      # filters the data for a map, specifically
      data <- master_data %>%
        haven::as_factor() %>%
        dplyr::mutate(fig_filter = stringr::str_to_title(get(input$topic))) %>%
        dplyr::distinct(fips, .keep_all = TRUE) %>%
        dplyr::select(fips, state_name, fig_filter)

      # uses a function from the naccho.viz package
      naccho.viz::create_cat_map(
        data = data,
        fips_column = "fips",
        selected_value = "fig_filter",
        title_text = filter_meta()$title,
        subtitle_text = "",
        caption_text = filter_meta()$caption,
        accessible_desc = ""
      ) %>%
        highcharter::hc_chart(
          marginBottom = 120,
          spacingTop = 10,
          spacingBottom = 0
        ) %>%
        highcharter::hc_legend(
          margin = 60
        )
    })


    # figure output -----------------------------------------------------------
    # used for bar and line graphs
    output$figure <- highcharter::renderHighchart({

      data <- data.frame(
        x = c(0, 1),
        y = c(10.1, 89.9),
        category = c("Rural", "Urban"),
        group = "Estimated Across All LHDs"
      )

     set_column(data) %>%
        viz_options(
          viz_title = "Percent of U.S. Population Served by LHDs",
          viz_caption = source_line,
          tooltip = "{point.y}% of the U.S. population is served by {point.category} LHDs"
        ) %>%
        viz_export()
    })



    # save the figure as a proxy to redraw below
    proxy <- highcharter::highchartProxy(ns("figure"))


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
      c(naccho.viz::navy)
      }
    })



    updated_inputs <- reactive({
      input$topic
      input$filter
    }) %>% debounce(100)


    # this observe is needed to redraw the figure based on the input selected
    # by the user. It also filters and stores tables based on the inputs
    # combinations selected by the user.
    observeEvent(updated_inputs(), {

      # Topic: Population
      # Filters: Rurality, Governance, LHD Size, Region
      # Weighting Performed:
      if (input$topic == "totpop" & input$filter != "All LHDs") {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(category = stringr::str_to_title(get(input$filter)),
                        fig_value = get(input$topic)) %>%
          dplyr::group_by(category) %>%
          dplyr::summarise(fig_value = sum(fig_value)) %>%
          dplyr::mutate(y = round(fig_value/sum(fig_value)*100,1),
                        x = as.numeric(as.factor(category)) -1, # -1 is required so the numbers start at 0 for the proxy to correctly draw
                        subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(group = "All LHDs")

          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())



      # Topic: Tenure
      # View: Bar Graph
      # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "tenure" & input$filter != "All LHDs") {
        data <- return_survey_weighted_statistics("tenure", input$filter)
        print(data)

          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())


      # Topic: Specialization
      # View: Bar Graph
      # Filters: All LHDs
      } else if (input$topic == "field" & input$filter == "All LHDs") {
        req(input$topic %not_in% c("totpop", "tenure"))
        data <- spec_data_all

          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())


      # Topic: Specialization
      # View: Bar Graph
      # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "field" & input$filter != "All LHDs") {
        data <- spec_data %>% dplyr::filter(filter == input$filter)
        series_id <- unique(data$group)

        proxy %>%
          proxy_to_group(
            data = data,
            meta = filter_meta(),
            id = series_id,
            colors = colors() # comes from earlier reactive and needs to match length of field input
          )


      # Topics: Geographies Served, Governance, Structure, Board of Health, Age, Race, Highest Degree, Gender
      # View: Bar Graph
      # Filters: All LHDs
      } else if (input$filter == "All LHDs") {
        req(input$topic %not_in% c("totpop", "tenure"))

        data <- return_survey_weighted_counts(input$topic)

        if(input$topic == "c4q29") {
          data$x[3] <- 2 # this may need to be hand-edited in 2025
        }

          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())


      # Topics: Geographies Served, Governance, Structure, Board of Health, Age, Race, Highest Degree, Gender
      # View: Bar Graph
      # Filters: Rurality, Governance, LHD Size, Region
      } else {
        req(input$topic %not_in% c("totpop", "tenure"))

        data <- return_survey_weighted_counts(input$topic, input$filter)

        if(input$topic == "c4q29") {
          data <- data %>%
            dplyr::mutate(x = dplyr::case_when(x == 3 ~ 2,
                                               TRUE ~ x))
        }

        series_id <- unique(data$group)

        proxy %>%
          proxy_to_group(
            data = data,
            meta = filter_meta(),
            id = series_id,
            colors = colors() # rendered reactively to match filter length
          )
      }
    })


   #Tables output ------
    output$data_table <- reactable::renderReactable({
      req(input$view == "Table")

      #add topic value for function
      topic <- input$topic

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


      if (input$topic == "field" & input$filter == "All LHDs") {
        data <- spec_data_all %>%
          dplyr::select(category, y, subtitle, Responses) %>%
          dplyr::summarise(category = category,
                           'y_All LHDs' = y,
                           'Responses_All LHDs' = sum(Responses),
                           subtitle = subtitle)


        } else if (input$filter == "All LHDs") {
          data <- return_survey_weighted_counts(input$topic)

          # Calculate total responses and 'y_All LHDs'
          total_responses <- sum(data$Responses, na.rm = TRUE)
          data <- data %>%
            dplyr::mutate(
              y = 100 * (Responses / total_responses),
              Responses = total_responses
            ) %>%
            dplyr::select(category, group, y, Responses) %>%
            dplyr::left_join(., add_all_counts_long(input$topic), by = "category")



      } else if (input$topic == "field" & input$filter != "All LHDs") {
        data <- spec_data %>% dplyr::filter(filter == input$filter) %>%
          dplyr::select(category, group, y, subtitle, Responses) %>%
          dplyr::group_by(category) %>%
          tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))

        # Calculate overall y and Responses for "All LHDs"
        overall_data <- spec_data %>%
          dplyr::filter(filter == input$filter) %>%
          dplyr::group_by(category) %>%
          dplyr::summarize(
            'y_All LHDs' = sum(y * Responses, na.rm = TRUE) / sum(Responses, na.rm = TRUE),
            'Responses_All LHDs' = sum(Responses, na.rm = TRUE)
          )

        # Join the overall data to the pivoted data
        data <- data %>%
          dplyr::left_join(overall_data, by = "category")



      } else if (input$topic == "tenure" & input$filter != "All LHDs") {
        data <- return_survey_weighted_statistics("tenure", input$filter) %>%
            dplyr::mutate(
              'y_All LHDs' = mean(y, na.rm = TRUE),  # Adjust calculation as needed
              'Responses_All LHDs' = sum(Responses, na.rm = TRUE)
            )  %>%
            dplyr::select(category, group, y, subtitle, Responses, 'y_All LHDs', 'Responses_All LHDs') %>%
            tidyr::pivot_wider(names_from = category, values_from = c(y, Responses)) %>%
            dplyr::rename(category = group)



      } else if (input$topic == "totpop" & input$filter != "All LHDs") {
        data <- master_data %>%
          haven::as_factor() %>%
          dplyr::mutate(category = stringr::str_to_title(get(input$filter)),
                        fig_value = get("totpop")) %>%
          dplyr::group_by(category)%>%
          dplyr::summarise(Responses = n(),
                           fig_value = sum(fig_value))%>%
          dplyr::mutate(y = round(fig_value/sum(fig_value)*100,1),
                        subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(group = "All LHDs") %>%
          dplyr::select(category, group, y, subtitle, Responses) %>%
          tidyr::pivot_wider(names_from = category, values_from = c(y, Responses))

        overall_totals <- master_data %>%
          dplyr::summarise('Responses_All LHDs' = n(),
                           fig_value_All_LHDs = sum(get("totpop"))) %>%
          dplyr::mutate('y_All LHDs' = round(fig_value_All_LHDs / sum(fig_value_All_LHDs) * 100, 1))

        # Add the calculated columns to your data
        data <- data %>%
          dplyr::mutate('y_All LHDs' = overall_totals$'y_All LHDs',
                        'Responses_All LHDs' = overall_totals$'Responses_All LHDs')%>%
          dplyr::rename(category = group)



      } else {
        # Process data as usual for other filters
        data <- return_survey_weighted_counts(input$topic, input$filter) %>%
          dplyr::select(category, group, y, subtitle, Responses) %>%
          tidyr::pivot_wider(names_from = group, values_from = c(y, Responses)) %>%
          dplyr::left_join(., add_all_counts_long(input$topic), by = "category") %>%
          dplyr::mutate(
            dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
          )
      }

      # Call the table creation function with the appropriate data and parameters
      create_non_time_table(data, category, colors, topic)
    })




    # right content ===========================================================

    # takeaway ----------------------------------------------------------------
    observeEvent(input$takeaway, {

      shinyjs::show("toggle_takeaway")
      shinyjs::hide("toggle_context")
      shinyjs::hide("toggle_definitions")

      output$show_takeaway <- renderUI({
        notes <- notes %>%
          dplyr::filter(topic == input$topic,
                        # view == input$view, # fix
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
            notes$notes))
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
          links
        )
      })
    })
  })
}
## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
