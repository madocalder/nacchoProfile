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
          class = "key-findings-head",
          div(
            class = "row",
            div(
              class = "col-sm-8",
              align = "left",
              h1(HTML("About the Study Population: <br> Local Health Departments in the U.S.")),
              hr(class = "header-hr")
            )
          )
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
                height = "70vh",
                class = "key-findings-cards",
                bslib::card_header(
                  fill = FALSE
                ),
                bslib::card_body(
                  div(
                    class = "row",
                    div(
                      class = "col-sm-3 color-carousel",
                      HTML("<span style = 'font-size: 2.6vmin; font-weight: 600; color: #212F5B; line-height: 3vmin;'>
                          Although only 6% of all LHDs were
                          classified as large, they served more
                          than half of the U.S. population. </span> <br> <br>
                          <span style = 'font-size: 1.8vmin;'>
                           <b> N: </b>2,512 <br>", source_line, "</span>")
                    ),
                    div(
                      class = "col-sm-9",
                      highcharter::highchartOutput(ns("population"), height = "55vmin")
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              bslib::card(
                height = "70vh",
                class = "key-findings-cards",
                bslib::card_header(
                  fill = FALSE
                ),
                bslib::card_body(
                  div(
                    class = "row",
                    div(
                      class = "col-sm-4 color-carousel-2",
                      HTML("<span style = 'font-size: 2.4vmin; font-weight: 600; color: #212F5B; line-height: 3vmin;'>
                          Of the 2,512 LHDs included in the 2022
                          Profile study population, 1,928 were
                          locally governed, 467 were units of the
                          state health agency, and 117 have shared
                          governance. In 30 states, all LHDs were locally
                          governed. <br> <br> In most states with mixed governance,
                          units of the state health agency serve
                          most parts of the state, while a small
                          number of large metropolitan areas
                          have locally governed LHDs.</span> <br> <br>
                          <span style = 'font-size: 1.8vmin;'>
                           <b> N: </b>2,512 <br>", source_line, "</span>")
                    ),
                    div(
                      class = "col-sm-8",
                      highcharter::highchartOutput(ns("map"),
                                                   # width = "60vmin",
                                                   height = "57vmin")
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              bslib::card(
                height = "70vh",
                class = "key-findings-cards",
                bslib::card_header(
                  fill = FALSE
                ),
                bslib::card_body(
                  div(
                    class = "row",
                    div(
                      class = "col-sm-4 color-carousel",
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
                           <b> N: </b>2,512 <br>", source_line, "</span>")
                    ),
                    div(
                      class = "col-sm-8",
                      highcharter::highchartOutput(ns("cities"),
                                                   # width = "65vmin",
                                                   height = "55vmin")
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
        y = c(9, 37, 53, 62, 32, 6),
        category = c("Small", "Medium", "Large",
                     "Small", "Medium", "Large"),
        group = c("% of all LHDs", "% of all LHDs", "% of all LHDs",
                  "% of population served by LHDs", "% of population served by LHDs", "% of population served by LHDs")
      )

      set_column(data) %>%
        viz_options(
          viz_title = "Percent of U.S. Population Served by LHDs",
          viz_caption = "",
          tooltip = "{point.y}% {point.group} are served by {point.category} LHDs",
          viz_subtitle = "",
          legend_align = "center"
        ) %>%
        viz_export
    })


    # map output --------------------------------------------------------------
    # used for the map only, displayed in the UI using a conditional panel
    output$map <- highcharter::renderHighchart({

      # data
      join <- data.frame(
        fips = c(33, 10, 44, 02, 11),
        state_name = c("New Hampshire", "Deleware", "Rhode Island",
                       "Alaska", "District of Columbia"),
        cat = c("Local", "State", "State", "Local", "Mixed")
      )

      data <- master_data %>%
        haven::as_factor() %>%
        dplyr::distinct(fips, .keep_all = TRUE) %>%
        dplyr::mutate(cat = c0govcatn) %>%
        dplyr::select(fips, state_name, cat)%>%
        dplyr::mutate(cat = dplyr::case_when(
          state_name %in% c("Wyoming", "Louisiana",
                            "Oklahoma", "Tennessee", "Texas", "Alabama",
                            "Virginia", "Maryland", "Pennsylvania", "Maine", "South Dakota") ~ "Mixed",
          state_name == "Delaware" ~ "Shared",
          cat == "shared" ~ "Shared",
          cat == "local" ~ "Local",
          cat == "state" ~ "State",
          cat == "mixed" ~ "Mixed",
          TRUE ~ cat))

      data <-  dplyr::bind_rows(data, join)

      View(data)
      # uses a function from the naccho.viz package
      naccho.viz::create_cat_map(
        data = data,
        fips_column = "fips",
        selected_value = "cat",
        title_text = "Governance of LHDs",
        subtitle_text = "",
        tooltip = "In 2022, {point.state_name}'s governance structure was {point.cat}.",
        caption_text = "",
        accessible_desc = ""
      ) %>%
        highcharter::hc_chart(
          marginBottom = 50,
          spacingTop = 0,
          spacingBottom = 0
        ) %>%
        highcharter::hc_legend(
          margin = "2.5vmin"
        )
    })


    # Figure One --------------------------------------------------------------
    output$cities <- highcharter::renderHighchart({

      data <- data.frame(
        y = c(69, 21, 8, 3,
              69, 26, 3, 1,
              68, 12, 14, 6,
              69, 7, 23, 1),
        category = c("All LHDs", "All LHDs", "All LHDs", "All LHDs",
                     "Small", "Small", "Small", "Small",
                     "Medium", "Medium", "Medium", "Medium",
                     "Large", "Large", "Large", "Large"),
        group = c("County", "City or Town", "Multi-county", "Other*",
                  "County", "City or Town", "Multi-county", "Other*",
                  "County", "City or Town", "Multi-county", "Other*",
                  "County", "City or Town", "Multi-county", "Other*")
      )

      # Reorder the category factor
      data$group <- factor(data$group, levels = c("County", "City or Town", "Multi-county", "Other*"))

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
          format = "{point.y:.0f}%",
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
          reversedStacks = FALSE,
          title = list(
            text = "% of LHDs",
            y = 20
          ), # y-axis label
          min = 0,
          max = 100,
          labels = list(
            format = "{value}%"
          )
        ) %>%
        viz_options(
          viz_title = "Geographic Jurisdictions Served by LHDs, by Size of Population Served",
          viz_caption = "",
          tooltip = "{point.y:.0f}% of {point.category} serve {point.group} areas",
          viz_subtitle = "",
          legend_align = "center"
        ) %>%
        viz_export()
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
