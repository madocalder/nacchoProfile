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
                      highcharter::highchartOutput(ns("finding_one"))
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
                      highcharter::highchartOutput(ns("finding_two"))
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
                  class = "col-xl-6 col-lg-12",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE
                    ),
                    bslib::card_body(
                     highcharter::highchartOutput(ns("finding_three"))
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
                      highcharter::highchartOutput(ns("finding_four"))
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
                      fill = FALSE
                    ),
                    bslib::card_body(
                      highcharter::highchartOutput(ns("finding_five"))
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
                      fill = FALSE
                    ),
                    bslib::card_body(
                      highcharter::highchartOutput(ns("finding_six"))
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
                  class = "col-xl-6 col-lg-12",
                  align = "center",
                  bslib::card(
                    height = 600,
                    class = "key-findings-cards",
                    bslib::card_header(
                      fill = FALSE
                    ),
                    bslib::card_body(
                      highcharter::highchartOutput(ns("finding_seven"))
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
                      highcharter::highchartOutput(ns("finding_eight"))
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
        my_colors = c(naccho.viz::naccho_pink, naccho.viz::naccho_orange, naccho.viz::naccho_green, naccho.viz::naccho_blue),
        x_variable = "year",
        y_variable = "value",
        title_text = "Workforce grew during the pandemic, but mostly among <span
        style='color:#AB6143;font-weight:bold;'>contract</span> and <span
        style='color:#9A639F;font-weight:bold;'>seasonal</span> workers.",
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
                  x = 1,
                  y = 60000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+7%",
              align = "center",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              style = list(
                color = "#fff",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            ),
            list(
              point =
                list(
                  x = 1,
                  y = 125000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "-16%",
              align = "center",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              style = list(
                color = "#000",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            ),
            list(
              point =
                list(
                  x = 1,
                  y = 144100,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+175%",
              align = "center",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              allowOverlap = TRUE,
              style = list(
                color = "#fff",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            ),
            list(
              point =
                list(
                  x = 1,
                  y = 182000,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "+48%",
              align = "center",
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
        title_text = "<span style='color:#AB6143;font-weight:bold;'>Average per capita revenues</span> from federal sources nearly
        doubled from 2019 to 2022.",
        subtitle_text = "Nonetheless, nearly one in five LHDs foresaw budget
        cuts their next fiscal year as federal resources expire.",
        x_group = "stat",
        y_label = "Per Capita Revenue",
        my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green),
        x_label = "",
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
                color = naccho.viz::naccho_orange,
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            )
          )
        )
    })


    # finding three - expenditures
    output$finding_three <- highcharter::renderHighchart({
      data <- data.frame(
        year = c("2019", "2022", "2019", "2022"),
        value = c(8.38, 9.4, NA, 3.1),
        type = c("Expenditures",
                     "Expenditures",
                     "COVID-19 expenditures",
                     "COVID-19 expenditures")
      )

      data$year <- factor(data$year)

      naccho.viz::create_stacked_bar(
        data = data,
        x_variable = "year",
        y_variable = "value",
        my_colors = c(naccho.viz::naccho_green, naccho.viz::naccho_orange),
        title_text = "LHDs <span style='color:#AB6143;font-weight:bold;'>spent
        an average</span> of $12.5 million in 2022, with most of the $4.12 million increase
        due to <span style='color:#80A14E;font-weight:bold;'>COVID-19.</span>",
        subtitle_text = "COVID-19 expenditures averaged $3.10
        million across LHDs, accounting for most of the increase.",
        x_group = "type",
        y_label = "Total Spending",
        y_max = 15,
        y_format = "${value:,.0f} m.",
        legend_enable = TRUE,
        legend_title = "",
        tooltip = "<b> Total Expenditures: </b> ${point.value:,.2f} million.<br>
                   <b> Type: </b> {point.type}",
        allow_export = FALSE
      ) %>%
        highcharter::hc_add_annotation(
          labels = list(
            list(
              point =
                list(
                  x = 1,
                  y = 9.75,
                  xAxis = 0,
                  yAxis = 0
                ),
              text = "COVID-19",
              align = "center",
              backgroundColor = "transparent",
              borderRadius = 50,
              borderWidth = 0,
              style = list(
                color = "#fff",
                fontFamily = "Heebo",
                fontWeight = "700",
                fontSize = "14px"
              )
            )
          )
        )
    })



    # immunizations -----------------------------------------------------------
    output$finding_four <- highcharter::renderHighchart({

      data <- data.frame(
        value = c(11, 47, 65, 11),
        type = c("Expanded",
                 "Reduced",
                 "Expanded",
                 "Reduced"),
        year = c("2019", "2019", "2022", "2022")
      )


      naccho.viz::create_grouped_bar(
        data = data,
        x_variable = "year",
        y_variable = "value",
        my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green, naccho.viz::naccho_pink),
        y_format = "{value}%",
        y_label = "Percent of LHDs",
        x_group = "type",
        allow_export = FALSE,
        tooltip = "<b> {point.type}</b> <br>
        <b>Percent of LHDs:</b> {point.value}%",
        legend_enable = TRUE,
        title_text = "LHDs <span style='color:#AB6143;font-weight:bold;'>expanded</span>
        immunization services in 2022 following prior <span style='color:#80A14E;font-weight:bold;'>decreases</span> in this service.",
        subtitle_text = "In 2022, 65% of LHDs expanded immunization services, compared to 11% that expanded immunizations in 2020 and 21% in 2019."
      )
    })


    # policy engagement -------------------------------------------------------
    output$finding_five <- highcharter::renderHighchart({

      data <- data.frame(
        value = c(8, 2),
        description = c("Involved in policy activities", "Not involved in policy activities"),
        percent = c(80, 20)
      )

      naccho.viz::create_icon_item(
        data = data,
        y_variable = "value",
        item_group = "description",
        legend_enable = TRUE,
        tooltip =
          "{point.percent}%",
        color_one = naccho.viz::naccho_orange,
        color_two = "#ddd",
        title_text = "<span style='color:#AB6143;font-weight:bold;'>Over 8 in ten LHDs</span> were involved in policy activities addressing COVID-19 emergency preparedness/response and infectious disease.",
        subtitle_text = "Just six in 10 were involved in these same kinds of policy areas not related to COVID-19.",
        caption_align = "left",
        allow_export = FALSE
      )
    })


    # informatics -------------------------------------------------------------
    output$finding_six <- highcharter::renderHighchart({

      data <- data.frame(
        year = c(2008, 2010, 2013, 2016, 2019, 2022, 2008, 2010, 2013, 2016, 2019, 2022),
        type = c("Electronic Health Records",
                 "Electronic Health Records",
                 "Electronic Health Records",
                 "Electronic Health Records",
                 "Electronic Health Records",
                 "Electronic Health Records",
                 "Health Information Exchanges",
                 "Health Information Exchanges",
                 "Health Information Exchanges",
                 "Health Information Exchanges",
                 "Health Information Exchanges",
                 "Health Information Exchanges"),
        value = c(19, 19, 23, 37, 59, 61,
                    8, 6, 14, 17, 21, 32)
      )

      data$year <- factor(data$year)

      naccho.viz::create_grouped_line(
        data = data,
        x_variable = "year",
        y_variable = "value",
        title_text = "Implementation of <span style='color:#AB6143;font-weight:bold;'>electronic health records</span> increased by 42 percentage points between 2008 and 2022.",
        subtitle_text = "Participation in health information exchanges increased 24 percentage points over the same period, with fewer small LHDs using these systems.",
        x_group = "type",
        y_label = "Percent of LHDs",
        my_colors = c(naccho.viz::naccho_orange, naccho.viz::naccho_green),
        y_format = "{value}%",
        legend_enable = TRUE,
        legend_title = "",
        x_label = "",
        tooltip = "<b> Technology Implemented: </b> ${point.type} <br>
                   <b> Percent of LHDs: </b> {point.percent}",
        allow_export = FALSE
      )
    })


    # clinical services -------------------------------------------------------
    output$finding_seven <- highcharter::renderHighchart({

      data <- data.frame(
        rurality = c("Rural",
                     "Rural",
                     "Rural",
                     "Rural",
                     "Rural",
                     "Urban",
                     "Urban",
                     "Urban",
                     "Urban",
                     "Urban"),
        service = c("Oral Health",
                 "Home Health Care",
                 "Substance Use",
                 "Mental Health",
                 "Primary Care",
                 "Oral Health",
                 "Home Health Care",
                 "Substance Use",
                 "Mental Health",
                 "Primary Care"),
        perc = c(18, 21, 12, 8, 7,
                    25, 12, 21, 16, 8)
      )

      data$rurality <- factor(data$rurality)

      naccho.viz::create_grouped_bar(
        data = data,
        x_variable = "rurality",
        y_variable = "perc",
        title_text = "Provision of direct clinical services varies across rural and urban LHDs.",
        subtitle_text = "In general, fewer rural LHDs provide direct clinical services.",
        x_group = "service",
        y_label = "",
        y_max = 30,
        y_format = "{value}%",
        legend_enable = TRUE,
        legend_title = "",
        tooltip = "<b> Technology Implemented: </b> {point.type} <br>
                   <b> Percent of LHDs: </b> {point.perc}%",
        allow_export = FALSE,
        select_theme = naccho.viz::naccho_bare_theme
      ) %>%
        highcharter::hc_plotOptions(
          series = list(
            dataLabels = list(
              enabled = TRUE,
              align = 'center',
              verticalAlign = 'bottom',
              style = list(
                fontWeight = "500",
                fontSize = "11px",
                fontFamily = "Heebo",
                textOutline = "none"),
              format = "{point.perc}%",
              formatter = highcharter::JS("function() {
                        var color = (this.point.color === '#008489' || this.point.color === '#80A14E') ? '#333' : '#FFF';
                        return '<span style=\"color:' + color + '\">' + this.x + ': ' + this.y + '</span>';
                      }"),
              inside = TRUE
            )
          )
        )
    })

    output$finding_eight <- highcharter::renderHighchart({
      # First chart data
      data_2008 <- list(
        list(name = "Under 50", y = 42, color = naccho.viz::naccho_orange),
        list(name = "Over 50", y = 58, color = "#ddd")
      )

      # Second chart data
      data_2022 <- list(
        list(name = "Under 50", y = 34, color = naccho.viz::naccho_orange),
        list(name = "Over 50", y = 66, color = "#ddd")
      )

      # Legend data
      legend_data <- list(
        list(name = "Under 50", y = 0, color = naccho.viz::naccho_orange),
        list(name = "Over 50", y = 0, color = "#ddd")
      )

      highcharter::highchart() %>%
        highcharter::hc_title(text = "Combined Pie Charts") %>%
        highcharter::hc_chart(type = "pie") %>%
        highcharter::hc_plotOptions(pie = list(
          innerSize = '70%',
          point = list(
            events = list(
              legendItemClick = highcharter::JS("function() {
              var name = this.name;
              var series = this.series.chart.series;
              Highcharts.each(series, function(ob) {
                Highcharts.each(ob.data, function(p) {
                  if (p.name === name) {
                    if (p.visible === true) {
                      p.setVisible(false);
                    } else {
                      p.setVisible(true);
                    }
                  }
                });
              });
              return false;
            }")
            )
          )
        )) %>%
        highcharter::hc_add_series(
          data = data_2008,
          name = "Series 1",
          center = c('25%', '50%'),
          size = '40%',
          showInLegend = FALSE
        ) %>%
        highcharter::hc_add_series(
          data = data_2022,
          name = "Series 2",
          center = c('75%', '50%'),
          size = '40%',
          showInLegend = FALSE
        ) %>%
        highcharter::hc_add_series(
          data = legend_data,
          name = "Legend",
          size = 0,
          showInLegend = TRUE,
          enableMouseTracking = FALSE,
          dataLabels = list(enabled = FALSE)
        ) %>%
        highcharter::hc_legend(
          enabled = TRUE,
          title = list(
            text = "Top Executive Is..."
          )
          ) %>%
        highcharter::hc_add_series(
          data = list(
            list(y = 0, x = 0.25, name = "2019"),
            list(y = 0, x = 0.75, name = "2022")
          ),
          type = "scatter",
          marker = list(enabled = FALSE),
          dataLabels = list(
            enabled = TRUE,
            format = '{point.name}',
            align = 'center',
            verticalAlign = 'bottom',
            style = list(fontSize = "14px")
          ),
          enableMouseTracking = FALSE,
          showInLegend = FALSE
        ) %>%
        highcharter::hc_add_theme(
          naccho.viz::naccho_bare_theme
        ) %>%
        highcharter::hc_plotOptions(
          series = list(
            dataLabels = list(
              enabled = TRUE,
              align = 'center',
              verticalAlign = 'bottom',
              style = list(
                fontWeight = "500",
                fontSize = "11px",
                fontFamily = "Heebo",
                textOutline = "none"),
              format = "{point.y}%",
              distance = 5)
          )
        ) %>%
        highcharter::hc_xAxis(
          visible = FALSE
        ) %>%
        highcharter::hc_yAxis(
          visible = FALSE
        ) %>%
        highcharter::hc_title(
          text = "Top executives are slightly younger and less tenured now than in 2008.", # title text
          margin = 40,
          widthAdjust = -60
        ) %>%
        highcharter::hc_subtitle(
          text = "In 2022, 42% of LHDs had a top executive younger than 50 – an increase from the 34% in 2008. During the same period, the average tenure decreased from 8.7 years to 6.6 years." # text under title
        )
    })
  })
}

## To be copied in the UI
# mod_key_findings_ui("key_findings_1")

## To be copied in the server
# mod_key_findings_server("key_findings_1")
