#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'overview_tab'",
      div(
        openMenu(),
        div(
          align = "right",
          actionButton(
            ns("info_button"),
            label = NULL,
            icon = icon("info-circle", class = "fa-solid"),
            style = "font-size: 2em; color: #3C8A97; background: none; border: none;
                position: absolute; top: .25rem; right: 1rem; padding: .1em;"
          )
        ),
        div(
          class = "key-findings-head",
          align = "left",
          h1(HTML("<br> About the Study Population: <br> Local Health Departments in the U.S.")),
          hr(class = "header-hr")
        ),

        div(
          class = "key-findings-content",

          # carousel
          bsutils::carousel(
            indicators = TRUE,
            controls = TRUE,
            class = "carousel-dark alt-2",
            bsutils::carouselItem(
              bslib::card(
                height = "75vh",
                class = "key-findings-cards",
                bslib::card_header(
                  fill = FALSE
                ),
                bslib::card_body(
                  div(
                    class = "row",
                    div(
                      class = "col-sm-4 color-carousel",
                      HTML("<span style = 'font-size: 2.5vmin; font-weight: 600; color: #212F5B; line-height: 2.5vmin;'>
                          Although only 6% of all LHDs were
                          classified as large, they served more
                          than half of the U.S. population. </span> <br> <br>
                          <span style = 'font-size: 1.8vmin;'>
                           <b> N: </b>2,493 <br>", source_line, "</span>")
                    ),
                    div(
                      class = "col-sm-8",
                      highcharter::highchartOutput(ns("population"), height = "50vh")
                    )
                  )

                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              bslib::card(
                height = "75vh",
                class = "key-findings-cards",
                bslib::card_header(
                  fill = FALSE
                ),
                bslib::card_body(
                  div(
                    class = "row",
                    div(
                      class = "col-sm-5 color-carousel-2",
                      HTML("<span style = 'font-size: 2.4vmin; font-weight: 600; color: #212F5B; line-height: 3vmin;'>
                          Of the 2,493 LHDs included in the 2025
                          Profile study population, 1,928 were
                          locally governed, 463 were units of the
                          state health agency, and 102 have shared
                          governance. In 29 states, all LHDs were locally
                          governed. <br> <br> In most states with mixed governance,
                          units of the state health agency serve
                          most parts of the state, while a small
                          number of large metropolitan areas
                          have locally governed LHDs.</span> <br> <br>
                          <span style = 'font-size: 1.8vmin;'>
                           <b> N: </b>2,493 <br>", source_line, "</span>")
                    ),
                    div(
                      class = "col-sm-7",
                      highcharter::highchartOutput(ns("map"),
                                                   width = "auto",
                                                   height = "55vh")
                      # ,
                      # style = "margin-left: auto !important;
                      #         margin-right: auto !important;"
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              bslib::card(
                height = "75vh",
                class = "key-findings-cards",
                bslib::card_header(
                  fill = FALSE
                ),
                bslib::card_body(
                  div(
                    class = "row",
                    div(
                      class = "col-sm-5 color-carousel",
                      HTML("<span style = 'font-size: 2.6vmin; font-weight: 600; color: #212F5B; line-height: 3vmin;'>
                          Approximately two-thirds of LHDs
                          were county-based and an additional
                          8% serve multiple counties. One-fifth of
                          LHDs serve cities or towns. <br> <br>
                          Large LHDs were less likely to serve
                          cities or towns but were more likely
                          to serve multiple counties than
                          small LHDs. </span> <br> <br>
                          <span style = 'font-size: 1.8vmin;'>
                          *County includes city-counties.
                          <br>
                           <b> N: </b>2,493 <br>", source_line, "</span>")
                    ),
                    div(
                      class = "col-sm-7",
                      highcharter::highchartOutput(ns("cities"),
                                                   # width = "65vmin",
                                                   height = "50vh")
                    )
                  )
                )
              ),
              interval = 60000L
            )
          )

        )
      )
    )
  )

}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns




    # Figure One --------------------------------------------------------------
    output$population <- highcharter::renderHighchart({

      data <- data.frame(
        x = c(0, 1, 2, 0, 1, 2),
        y = c(9, 37, 54, 61, 33, 6),
        category = c("Small", "Medium", "Large",
                     "Small", "Medium", "Large"),
        group = c("Population served by LHDs", "Population served by LHDs", "Population served by LHDs",
                  "All LHDs", "All LHDs", "All LHDs"),
        tt = c("the population served by LHDs are served by ", "the population served by LHDs are served by ",
               "the population served by LHDs are served by ",
               "all LHDs are ", "all LHDs are ", "all LHDs are ")
      )

      data <- data %>%
        dplyr::mutate(tooltip =
                        paste0(y,"% of ", tt,
                               tolower(category), " LHDs."))


      set_column(data,
                 data_label = "{point.y:,.0f}%",
                 y_label = "%") %>%
        viz_options(
          viz_title = "% of U.S. Population Served by LHDs",
          viz_caption = "",
          viz_subtitle = "",
          tooltip = "{point.tooltip}",
          legend_align = "center",
          legend_title = "% of"
        ) %>%
        # highcharter::hc_tooltip(
        #   pointFormat =  "{point.y:.0f}% of {point.group} are served by {point.category} LHDs.") %>%
        viz_export
    })


    # map output --------------------------------------------------------------
    # State governance: more than one observed governance category = Mixed.
    output$map <- highcharter::renderHighchart({

      # State names and numeric FIPS codes used by custom.map
      state_lookup <- data.frame(
        c0state = c(state.abb, "DC"),
        state_name = c(state.name, "District of Columbia"),
        fips = c(
          1, 2, 4, 5, 6, 8, 9, 10, 12, 13,
          15, 16, 17, 18, 19, 20, 21, 22,
          23, 24, 25, 26, 27, 28, 29, 30,
          31, 32, 33, 34, 35, 36, 37, 38,
          39, 40, 41, 42, 44, 45, 46, 47,
          48, 49, 50, 51, 53, 54, 55, 56,
          11
        ),
        stringsAsFactors = FALSE
      )

      # Use the actual assigned governance categories in the dataset.
      # A state is Mixed when it contains two or more distinct categories.
      state_governance <- master_data %>%
        dplyr::transmute(
          c0state = as.character(c0state),
          govcat = tolower(trimws(as.character(c0govcat)))
        ) %>%
        dplyr::filter(
          !is.na(c0state),
          !is.na(govcat),
          govcat %in% c("local", "state", "shared")
        ) %>%
        dplyr::group_by(c0state) %>%
        dplyr::summarise(
          grouping = if (dplyr::n_distinct(govcat) > 1L) {
            "Mixed"
          } else {
            switch(
              dplyr::first(govcat),
              local = "Local",
              state = "State",
              shared = "Shared"
            )
          },
          .groups = "drop"
        )

      # Include all states in the map; states with no observations are NA.
      data <- state_lookup %>%
        dplyr::left_join(state_governance, by = "c0state") %>%
        dplyr::mutate(
          grouping = dplyr::coalesce(grouping, "NA"),
          value_tt = dplyr::if_else(
            state_name == "Rhode Island",
            paste(
              "NA. Rhode Island was excluded from the study because",
              "the state health agency operates on behalf of local",
              "public health and has no sub-state units"
            ),
            dplyr::if_else(
              grouping == "NA",
              "not available in the dataset",
              tolower(grouping)
            )
          )
        )

      colors <- c(
        "Local" = naccho.viz::green,
        "State" = naccho.viz::navy,
        "Shared" = naccho.viz::terracotta,
        "Mixed" = naccho.viz::teal,
        "NA" = naccho.viz::grey
      )

      # Numeric category values make Highcharter's categorical color axis
      # deterministic while retaining the human-readable grouping column.
      categories <- names(colors)
      data <- data %>%
        dplyr::mutate(
          map_value = as.integer(match(grouping, categories)) - 1L
        )

      highcharter::highchart(type = "map") %>%
        custom.map::hc_add_custom_map_cat(
          data = data,
          value = "map_value",
          joinBy = "fips",
          mapType = "b",
          type = "map",
          name = "Governance",
          borderColor = "#020202",
          borderWidth = 0.25,
          dataLabels = list(enabled = FALSE)
        ) %>%
        highcharter::hc_colorAxis(
          dataClasses = lapply(seq_along(categories), function(i) {
            list(
              name = categories[i],
              from = i - 1L,
              to = i - 1L,
              color = unname(colors[i])
            )
          })
        ) %>%
        highcharter::hc_title(text = "Governance of LHDs") %>%
        highcharter::hc_tooltip(
          pointFormat = paste0(
            "In 2025, {point.state_name}'s governance structure ",
            "was {point.value_tt}."
          )
        ) %>%
        highcharter::hc_legend(enabled = TRUE)

    })


    # Figure One --------------------------------------------------------------
    output$cities <- highcharter::renderHighchart({

      data <- data.frame(
        y = c(69, 20, 8, 3,
              69, 26, 3, 1,
              69, 12, 13, 6,
              70, 7, 22, 1),
        category = c("All LHDs", "All LHDs", "All LHDs", "All LHDs",
                     "Small", "Small", "Small", "Small",
                     "Medium", "Medium", "Medium", "Medium",
                     "Large", "Large", "Large", "Large"),
        group = c("County*", "City or Town", "Multi-county", "Multi-city/town",
                  "County*", "City or Town", "Multi-county", "Multi-city/town",
                  "County*", "City or Town", "Multi-county", "Multi-city/town",
                  "County*", "City or Town", "Multi-county", "Multi-city/town")
      )

      data$group <- factor(data$group, levels = c("County*", "City or Town", "Multi-county", "Multi-city/town"))

      data <- data %>%
        dplyr::mutate(
          cat_tt = dplyr::case_when(category != "All LHDs" ~ tolower(category),
                                    TRUE ~ "all"),
          group_tt = dplyr::case_when(group != "County*" ~ tolower(group),
                                      TRUE ~ "county")
        )

      highcharter::hchart(
        data,
        "bar",
        id = "All",
        highcharter::hcaes(
          x = category,
          y = y,
          group = group
        ),
        stacking = "normal",
        dataLabels = list(
          enabled = TRUE,
          overflow = "allow",
          format = '{point.y:.0f}%',
          #   formatter = JS(
          #     "
          # function () {
          #   if (this.y > 0 && this.y < 1) {
          #     roundedValue = this.y.toFixed(1);
          #   } else {
          #     roundedValue = this.y.toFixed(0);
          #   }
          #   return roundedValue + '%';
          # }
          # "
          #   ),
          overflow = TRUE,
          distance = 5,
          padding = 2,
          color = "#fff",
          style = list(
            color = naccho.viz::grey,
            fontFamily = "Heebo",
            fontWeight = "500",
            fontSize = "2vmin",
            textShadow = "none"
          )
        )
      ) %>%
        highcharter::hc_xAxis(
          title = list(text = ""), # x-axis label
          categories = levels(unique(data$category))
        ) %>%
        highcharter::hc_yAxis(
          reserveSpace = TRUE,
          overflow = "allow",
          reversedStacks = FALSE,
          title = list(
            text = "",
            y = 20
          ), # y-axis label
          min = 0,
          max = 100,
          endOnTick = TRUE,
          maxPadding = .1,
          labels = list(
            format = "{value}%"
          )
        ) %>%
        viz_options(
          viz_title = "Geographic Jurisdictions Served by LHDs, by Size of Population Served",
          viz_caption = "",
          tooltip = "{point.y:.0f}% of {point.cat_tt} LHDs serve {point.group_tt} areas.",
          viz_subtitle = "",
          legend_align = "center"
        ) %>%
        viz_export()
    })



    # Reuse existing button functionality to show the modal on click
    observeEvent(input$info_button, {
      showModal(modalDialog(
        title = "About the Denominator of LHDs",
        HTML("<p>For the purposes of surveying and developing national estimates of
              the LHD landscape, NACCHO designed and utilizes a methodology to account
              for the most unique individuals in the U.S. population without \"double counting.\"
              Therefore, the denominator of LHDs used for surveying and national
              estimation purposes will always be smaller than the actual number of LHDs in the U.S.,
              while still covering all local jurisdictions uniquely.
              NACCHO is in frequent contact and consultation with state and local
              officials to update the denominator. There were 2,493 LHDs included
              in the 2025 Profile study population.</p>"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })


  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
