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
              class = "col-sm-4 min-height-400",
              br(),
              selectInput(
                inputId = ns("topic"),
                strong("Topic"),
                choices = landscape_topics,
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
                choices = landscape_filters[4]
              )
            )
          ),

          hr(),
          br(),
          br(),

          # add main page content here ----------------------------------------------

          div(
            class = "viz-content",

            # display when map is selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] == \'Map\'"),
              highcharter::highchartOutput(ns("map"), height = 575)
            ),

            # display when map is not selected
            conditionalPanel(
              condition = paste0('input[\'', ns('view'), "\'] != \'Map\'"),
              highcharter::highchartOutput(ns("figure"), height = 575)
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
            hr(class = "right-hr"),
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
    # this can create delays and glitches that I need to DEAL WITH // COME BACK
    observe({
      if (input$topic %in% c("totpop", "tenure")) {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[-1]
        )
      } else {
        updateSelectInput(session, "filter",
                          choices = landscape_filters
        )
      }
    })

    observe({
      if (input$topic == "c0govcatn") {
        updateSelectInput(session, "view",
                          choices = c("Map", "Bar Graph", "Line Graph", "Table"))
      } else if (input$topic == "totpop") {
        updateSelectInput(session, "view",
                          choices = c("Bar Graph", "Line Graph", "Pie Chart", "Table"))
        updateSelectInput(session, "filter",
                          choices = landscape_filters[4])
      } else {
        updateSelectInput(session, "view",
                          choices = c("Bar Graph", "Line Graph", "Table"))
      }
    })

    observe({
      if (input$topic == "c0govcatn" & input$view == "Map") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[1])
      }
      if (input$topic == "c0govcatn" & input$view != "Map") {
        updateSelectInput(session, "filter",
                          choices = landscape_filters[-3])
      }
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

      req(input$filter == "No Filter") # needed to prevent glitch

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
      )
    })


    # figure output -----------------------------------------------------------
    # used for bar and line graphs
    output$figure <- highcharter::renderHighchart({

      data <- data.frame(
        x = c(0, 1),
        y = c(10.1, 89.9),
        category = c("Rural", "Urban"),
        group = "Across All LHDs"
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


    updatedInputs <- reactive({
      input$topic
      input$filter
    }) |> debounce(250)


    # this observe is needed to redraw the figure based on the input selected
    # by the user. It also filters and stores tables based on the inputs
    # combinations selected by the user.
    observeEvent(updatedInputs(), {

    #   shinyjs::runjs(paste0("
    #   setTimeout(function() {
    #     var chart = $('#landscape_1-figure').highcharts();
    #     chart.yAxis[0].update({
    #       title: {
    #         text: '", filter_meta()$y_label, "',
    #         style: { color: '#333333' }
    #       },
    #       labels: {
    #       format: '", filter_meta()$y_format, "',
    #       }
    #     }, true, { duration: 1000 });
    #   }, 500); // 500ms delay
    # "))


      # Topic: Population
      # Filters: Rurality, Governance, LHD Size, Region
      # Weighting Performed:
      if (input$topic == "totpop" & input$filter != "No Filter") {
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


        if (input$view == "Bar Graph") {
          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())
        } else {
          proxy %>%
            proxy_to_pie(data = data,
                         meta = filter_meta())
        }



      # Topic: Tenure
      # View: Bar Graph
      # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "tenure" & input$filter != "No Filter") {
        data <- return_survey_weighted_statistics("tenure", input$filter)

          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())


      # Topic: Specialization
      # View: Bar Graph
      # Filters: No Filter
      } else if (input$topic == "field" & input$filter == "No Filter") {
        req(input$topic %not_in% c("totpop", "tenure"))
        data <- spec_data_all

          proxy %>%
            proxy_to_single(data = data,
                            meta = filter_meta())


      # Topic: Specialization
      # View: Bar Graph
      # Filters: Rurality, Governance, LHD Size, Region
      } else if (input$topic == "field" & input$filter != "No Filter") {
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
      # Filters: No Filter
      } else if (input$filter == "No Filter") {
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


    #   } else if (input$view == "Pie Chart") {
    #     naccho.viz::create_pie(
    #       data = filter_data(),
    #       variable = "fig_filter",
    #       value = "n",
    #       my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_pink, naccho.viz::naccho_teal, naccho.viz::naccho_blue, naccho.viz::naccho_green),
    #       legend_enable = TRUE,
    #       title_text = filter_meta()$title,
    #       subtitle_text = paste0("By ", names(landscape_filters[landscape_filters == input$filter])),
    #       caption_text = filter_meta()$caption
    #     ) %>%
    #       naccho.viz::add_logo(
    #         "https://toolbox.naccho.org/pages/img/logo-header.png",
    #         width = 150L,
    #         height = 40L
    #       )
    #
    #   }
    # })



# right sidebar -----------------------------------------------------------

    # takeaway
    observeEvent(input$takeaway, {

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

    # keep so the correct action links are selection upon shiny connection
    session$sendCustomMessage(type = 'setNamespace', message = 'landscape_1')
  })
}

## To be copied in the UI
# mod_landscape_ui("landscape_1")

## To be copied in the server
# mod_landscape_server("landscape_1")
