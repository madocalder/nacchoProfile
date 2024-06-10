#' key_findings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_key_findings_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'key_findings_tab'",
      div(
        openMenu(),
        div(
          class = "key-findings-head",
          div(
            class = "row",
            div(
              class = "col-sm-8",
              align = "left",
              h1("Key Findings")
            ),
            div(
              class = "col-sm-3 findings-align",
                  actionLink(class = "download-input link-no-underline",
                    inputId = ns("download"),
                    "Download Key Findings"
              )
            )
          )
        ),
        div(
          class = "key-findings-content",

          # carousel
          bsutils::carousel(
            indicators = FALSE,
            controls = TRUE,
            class = "carousel-dark alt-2",
            bsutils::carouselItem(
              div(
                class = "row",
                div(
                  class = "col-xl-6 col-lg-12",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE
                    ),
                    bslib::card_body(
                      highcharter::highchartOutput(ns("finding_one")),
                    )
                  )
                ),
                div(
                  class = "col-xl-6 col-lg-12",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE
                    ),
                    bslib::card_body(
                      highcharter::highchartOutput(ns("finding_two")),
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              div(
                class = "row",
                div(
                  class = "col-lg-6",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-6",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                )
              ),
              interval = 60000L
            ),
            bsutils::carouselItem(
              div(
                class = "row",
                div(
                  class = "col-lg-6",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                ),
                div(
                  class = "col-lg-6",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE,
                      h2("This is a Key Findings."),
                    ),
                    bslib::card_body(
                      p("This is the text for the Key Finding")
                    )
                  )
                )
              ),
              interval = 60000L
            )
          ),

          # modal for key findings download
          bsutils::modal(
            id = ns("modal"),
            bsutils::modalHeader(
              bsutils::modalTitle(
                p("What do you want to download?")
              )
            ),
            bsutils::modalBody(
              p("Some information about how you can use this information
                  for example you can add more visualizations or use it to
                  do this and that."),
              br(),
              br(),
              downloadLink(
                outputId = ns("download_slides"),
                label = p("Download our key findings slide deck")
              ),
              br(),
              downloadLink(
                outputId = ns("download_pdf"),
                label = p("Download our key findings brief")
              )
            )
          )
        )
      )
    )
  )
}

#' key_findings Server Functions
#'
#' @noRd
mod_key_findings_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # modal
    observeEvent(
      input$download,
      bsutils::modal_show(ns("modal"))
    )

    # download slides
    output$download_slides <-
      downloadHandler(
        filename = "key_findings_slides.pdf",
        content = function(file) {
          file.copy("", file) # add content www/
        }
      )

    # download pdf
    output$download_pdf <-
      downloadHandler(
        filename = "key_findings_report.pdf",
        content = function(file) {
          file.copy("", file) # add content www/
        }
      )

    # finding one - workforce
    output$finding_one <- highcharter::renderHighchart({

      data <- data.frame(
        year = c("2019", "2022", "2019", "2022", "2019", "2022", "2019", "2022"),
        value = c(129000, 138000, 17600, 14700, 8000, 22000, 2300, 3400),
        job_type = c("Full-time",
                     "Full-time",
                     "Part-time",
                     "Part-time",
                     "Contractual",
                     "Contractual",
                     "Seasonal",
                     "Seasonal"),
        perc_change = c("+7%", "+7%", "-16%", "-16%", "+175%", "+175%", "+48%", "+48%")
      )

      data <- data %>%
        dplyr::mutate(job_type = factor(job_type, levels = c("Seasonal", "Contractual", "Part-time", "Full-time"))) %>%
        dplyr::arrange(job_type)

      naccho.viz::create_stacked_bar(
        data = data,
        my_colors = c(naccho.viz::naccho_pink, naccho.viz::naccho_blue, naccho.viz::naccho_green, naccho.viz::naccho_teal),
        x_variable = "year",
        y_variable = "value",
        title_text = "Workforce Growth During the Pandemic Is Likely
                         Temporary.",
        subtitle_text = "The LHD workforce grew by 19% between 2019 and 2022,
                        fueled by a 175% increase in contractual employees.",
        x_group = "job_type",
        y_label = "Employees",
        y_max = 200000,
        legend_enable = TRUE,
        legend_title = "",
        tooltip = "<b> Employees: </b> {point.value:,.0f} <br>
                   <b> Percent Change from 2019 to 2022: </b> {point.perc_change} <br>
                   <b> Job Type: </b> {point.job_type}",
        allow_export = FALSE
      ) %>%
        highcharter::hc_add_annotation(
          labels = list(
            list(
              point =
                list(
                  x = 1.42,
                  y = 60000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+7%",
              align = "left",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              style = list(
                color = "#008A96",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            ),
            list(
              point =
                list(
                  x = 1.45,
                  y = 126000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "-16%",
              align = "left",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              style = list(
                color = "#6C7E26",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            ),
            list(
              point =
                list(
                  x = 1.49,
                  y = 145000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+175%",
              align = "left",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              allowOverlap = TRUE,
              style = list(
                color = "#212F5B",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            ),
            list(
              point =
                list(
                  x = 1.45,
                  y = 180000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+48%",
              align = "left",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              allowOverlap = TRUE,
              style = list(
                color = "#9A639F",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            )
          )
        )
    })

    # finding two - revenues
    output$finding_two <- highcharter::renderHighchart({
      data <- data.frame(
        year = c("2019", "2022", "2019", "2022"),
        value = c(17, 31, 10, 18),
        stat = c("Mean", "Mean", "Median", "Median"),
        perc_change = c("+45%", "+45%", "+44%", "+44%")
      )

      data$year <- factor(data$year)

      naccho.viz::create_grouped_line(
        data = data,
        x_variable = "year",
        y_variable = "value",
        title_text = "Average per capita revenues from federal sources nearly
        doubled from 2019 to 2022.",
        subtitle_text = "Nonetheless, nearly one in five LHDs foresaw budget
        cuts their next fiscal year as federal resources expire.",
        x_group = "stat",
        y_label = "Per Capita Revenue",
        x_label = "Year",
        y_max = 35,
        y_format = "${value:,.0f}",
        legend_enable = TRUE,
        legend_title = "",
        tooltip = "<b> Per Capita Revenue: </b> ${point.value:,.0f} <br>
                   <b> Percent Change from 2019 to 2022: </b> {point.perc_change} <br>
                   <b> Statistic: </b> {point.stat}",
        allow_export = FALSE
      ) %>%
        highcharter::hc_add_annotation(
          labels = list(
            list(
              point =
                list(
                  x = 1,
                  y = 30,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+45%",
              align = "center",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              style = list(
                color = "#008A96",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            )
          )
        )
    })

  })
}

## To be copied in the UI
# mod_key_findings_ui("key_findings_1")

## To be copied in the server
# mod_key_findings_server("key_findings_1")
