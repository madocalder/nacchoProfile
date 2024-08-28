#' Highcharter Exporting
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param enabled Boolean, TRUE or FALSE, where TRUE allows exporting.
#'
#' @return Export feature for a highcharter figure.
#' @export
#'
viz_export <- function(hc, enabled = TRUE) {
  hc %>%
    highcharter::hc_exporting(
      enabled = enabled,
      accessibility = list(
        enabled = TRUE
      ),
      formAttributes = list(
        target = "_blank" # opens in new window
      ),
      sourceHeight = 700, # sets height of download
      sourceWidth = 1200, # sets width of download
      allowHTML = TRUE,
      buttons = list(
        contextButton = list(
          symbol = "menu",
          symbolStrokeWidth = 2,
          symbolFill = naccho.viz::naccho_teal,
          symbolStroke = naccho.viz::naccho_teal,
          menuItems = c("printChart",
                        "separator",
                        "downloadPNG",
                        "downloadJPEG",
                        "downloadPDF",
                        "downloadSVG",
                        "separator",
                        "downloadCSV",
                        "downloadXLS",
                        "openInCloud")
        )
      )
    )
}



#' Highcharter Options: Title, Subtitle, Caption, Colors, ...
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param viz_title character string; title of the visualization. Defaults to nothing.
#' @param viz_subtitle character string; subtitle to appear underneath the title. Defaults to nothing.
#' @param viz_caption character string; source and data notes to appear underneath the figure. Defaults to nothing.
#' @param caption_align character string; options include "left", "center", or "right" where the default is "center".
#' @param colors vector or character string; one character or a vector of characters, which must be defined hex values. Defaults to naccho.viz::naccho_colors.
#' @param theme hc_theme, must be a defined hc_theme available in {highcharter}, {naccho.viz}, or otherwise defined.
#' @param tooltip character string; text and values to appear in the tooltip, use {point.xx} to call a value from the data frame, where xx is a specific column.
#' @param tooltip_align character string; options include "left", "right", or "center".
#' @param legend_enable boolean, TRUE or FALSE, where TRUE enables the legend. Defaults to TRUE.
#' @param legend_title character string; title of the legend, defaults to nothing unless defined.
#' @param legend_title character string; options include "left", "center" or "right". Defaults to "right".

#'
#' @return Options for rendering a highcharter figure.
#' @export
#'
viz_options <- function(hc,
                        viz_title = "",
                        viz_subtitle = "",
                        viz_caption = "",
                        caption_align = "center",
                        colors = naccho.viz::naccho_colors,
                        theme = naccho.viz::basic_theme,
                        tooltip,
                        tooltip_align = "center",
                        legend_enable = TRUE,
                        legend_title = "",
                        legend_align = "right") {
  hc %>%
    highcharter::hc_colors(
      colors = colors
    ) %>%
    highcharter::hc_add_theme(
      theme
    ) %>%
    highcharter::hc_title(
      text = viz_title, # title text
      margin = 40,
      widthAdjust = -60
    ) %>%
    highcharter::hc_subtitle(
      text = viz_subtitle # text under title
    ) %>%
    highcharter::hc_caption(
      text = viz_caption, # source and data notes
      useHTML = TRUE,
      margin = 20,
      align = caption_align
    ) %>%
    highcharter::hc_tooltip(
      headerFormat = "",
      pointFormat = tooltip,
      useHTML = TRUE,
      style = list(
        fontWeight = 400,
        textAlign = tooltip_align
      )
    ) %>%
    highcharter::hc_legend(
      enabled = legend_enable, # enabled legend
      reversed = FALSE,
      layout = "horizontal",
      align = legend_align,
      verticalAlign = "bottom",
      itemMarginBottom = 3,
      itemMarginTop = 3,
      title = list(
        text = legend_title
      )
    )
}




#' Single Column Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_single <- function(hc, data, meta, ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("State", "Local", "Shared")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Midwest", "South", "West", "Northeast")
    ) %>%
    highcharter::hcpxy_set_data(
      "column",
      data = data
    ) %>%
    highcharter::hcpxy_update(
      series = list(
        list(type = "column"),
        dataLabels = list(
          format = meta$data_label
        )
      ),
      yAxis = list(
        title = list(
          text = meta$y_label
        ),
        max = meta$y_max,
        labels = list(
          format = meta$y_format
        )
      ),
      title = list(
        text = meta$title
      ),
      subtitle = list(
        text = unique(data$subtitle)
      ),
      caption = list(
        text = meta$caption
      ),
      xAxis = list(
        categories = data$category,
        offset = 0
      ),
      tooltip = list(
        pointFormat = meta$tooltip
      )
    )
}



#' Grouped Column Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param colors vector or character string; one character or a vector of characters, which must be defined hex values.
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_group <- function(
    hc,
    data,
    meta,
    id,
    colors,
    ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("State", "Local", "Shared")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Midwest", "South", "West", "Northeast")
    ) %>%
    highcharter::hcpxy_add_series(
      data = data,
      "column",
      highcharter::hcaes(x, y, group = group),
      id = id,
      color = colors
    ) %>%
    highcharter::hcpxy_update(
      series = list(
        list(type = "column"),
        dataLabels = list(
          format = meta$data_label
        )
      ),
      yAxis = list(
        title = list(
          text = meta$y_label
        ),
        max = meta$y_max,
        labels = list(
          format = meta$y_format
        )
      ),
      title = list(
        text = meta$title
      ),
      subtitle = list(
        text = unique(data$subtitle)
      ),
      caption = list(
        text = meta$caption
      ),
      xAxis = list(
        categories = unique(data$category),
        offset = 0
      ),
      tooltip = list(
        tooltip = meta$tooltip
      )
    )
}



#' Pie Chart Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_pie <- function(hc, data, meta, colors, categories, ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("State", "Local", "Shared")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Midwest", "South", "West", "Northeast")
    ) %>%
    highcharter::hcpxy_set_data(
      "pie",
      data = data,
      mapping = highcharter::hcaes(x, y, name = category)
    ) %>%
    highcharter::hcpxy_update(
      series = list(
        type = "pie",
        innerSize = "70%", # Set the inner size for the donut effect
        allowPointSelect = TRUE, # Enable point selection on the chart
        cursor = "pointer", # Set cursor style for selectable points
        showInLegend = TRUE, # Show legend for the chart
        dataLabels = list(
          format = meta$data_label
        )
      ),
      yAxis = list(
        title = list(
          text = ""
        ),
        max = "",
        labels = list(
          format = ""
        )
      ),
      title = list(
        text = meta$title
      ),
      caption = list(
        text = meta$caption
      ),
      xAxis = list(
        categories = "",
        lineColor = "#fff",
        lineWidth = 0,
        offset = 50
      ),
      tooltip = list(
        tooltip = meta$tooltip
      )
    )
}



#' Set First Highcharter Column Graph
#'
#' @description Create the first highcharter visualizations from which proxies will redraw.
#'
#' @param data dataframe; A dataframe with x, y, group, and category columns.
#' @param y_label character string; Label for the Y Axis.
#'
#' @export
#'
#'
#' @return column graph
set_column <- function(data,
                       y_label = "% of Population") {

  highcharter::hchart(
    data,
    "column",
    id = "All",
    name = "Estimated Across All LHDs",
    highcharter::hcaes(
      x = x,
      y = y,
      group = group
    ),
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y}%",
      overflow = TRUE,
      distance = 5,
      padding = 2,
      style = list(
        color = naccho.viz::naccho_grey,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "14px"
      )
    )
  ) %>%
    highcharter::hc_xAxis(
      title = list(text = ""), # x-axis label
      categories = data$category
    ) %>%
    highcharter::hc_yAxis(
      title = list(
        text = y_label
      ), # y-axis label
      min = 0,
      labels = list(
        format = "{value}%"
      )
    )
}




#' Set First Highcharter Column Graph
#'
#' @description Create the first highcharter visualizations from which proxies will redraw.
#'
#' @param data dataframe; A dataframe with x, y, group, and category columns.
#' @param y_label character string; Label for the Y Axis.
#'
#' @export
#'
#'
#' @return column graph
set_simple_bar <- function(data,
                           y_label = "% of LHDs") {

  highcharter::hchart(
    data,
    "bar",
    id = "All",
    highcharter::hcaes(
      x = x,
      y = y,
      group = group
    ),
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y}%",
      overflow = TRUE,
      distance = 5,
      padding = 2,
      style = list(
        color = naccho.viz::naccho_grey,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "14px"
      )
    )
  ) %>%
    highcharter::hc_xAxis(
      title = list(text = ""), # x-axis label
      categories = unique(data$category)
    ) %>%
    highcharter::hc_yAxis(
      title = list(
        text = y_label
      ), # y-axis label
      min = 0,
      labels = list(
        format = "{value}%"
      )
    )
}


#' Set First Highcharter Bar Graph
#'
#' @description Create the first highcharter visualizations from which proxies will redraw.
#'
#' @param data dataframe; A dataframe with x, y, group, and category columns.
#' @param y_label character string; Label for the Y Axis.
#'
#' @export
#'
#'
#' @return column graph
set_bar <- function(data,
                    y_label = "% of Population",
                    name_one = "Jobs Lost",
                    name_two = "Jobs Added",
                    set_max = 15000,
                    set_format = NA) {

  highcharter::highchart() %>%
    highcharter::hc_chart(
      type = "bar"
      # margin = list(90, 0, 180, 0)
    ) %>%
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    ) %>%
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE
    ) %>%
    highcharter::hc_xAxis(
      categories = unique(data$group),
      labels = list(
        enabled = FALSE
      ),
      plotBands = list(
        list(
          from = -0.5,
          to = 0.5,
          color = "transparent",
          label = list(
            text = "Rural",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 0.5,
          to = 1.5,
          color = "transparent",
          label = list(
            text = "Urban",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 1.5,
          to = 2.5,
          color = "transparent",
          label = list(
            text = "Local",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 2.5,
          to = 3.5,
          color = "transparent",
          label = list(
            text = "Shared",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 3.5,
          to = 4.5,
          color = "transparent",
          label = list(
            text = "State",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 4.5,
          to = 5.5,
          color = "transparent",
          label = list(
            text = "Small",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 5.5,
          to = 6.5,
          color = "transparent",
          label = list(
            text = "Medium",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 6.5,
          to = 7.5,
          color = "transparent",
          label = list(
            text = "Large",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 7.5,
          to = 8.5,
          color = "transparent",
          label = list(
            text = "Midwest",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 8.5,
          to = 9.5,
          color = "transparent",
          label = list(
            text = "Northeast",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 9.5,
          to = 10.5,
          color = "transparent",
          label = list(
            text = "South",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 10.5,
          to = 11.5,
          color = "transparent",
          label = list(
            text = "West",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo"
            )
          )
        ),
        list(
          from = 11.5,
          to = 12.5,
          color = "#EEEAE4",
          label = list(
            text = "All LHDs",
            align = "center",
            x = -1,
            style = list(
              fontFamily = "Heebo",
              fontWeight = 700
            )
          )
        )
      )
    ) %>%
    highcharter::hc_yAxis_multiples(
      list(
        max = set_max,
        title = list(text = NULL),
        width = "45%",
        reversed = TRUE,
        offset = 0,
        alignTicks = FALSE,
        labels = list(
          format = set_format
        )
      ),
      list(
        max = set_max,
        title = list(text = NULL),
        offset = 0,
        left = "55%",
        width = "45%",
        offset = 0,
        alignTicks = FALSE ,
        labels = list(
          format = set_format
        )
      )
    ) %>%
    highcharter::hc_add_series(
      id = "Left",
      name = name_one,
      data = data$left,
      yAxis = 0
    ) %>%
    highcharter::hc_add_series(
      id = "Right",
      name = name_two,
      data = data$right,
      yAxis = 1
    )
}






#' Set First Highcharter Histogram
#'
#' @description Create the first highcharter visualizations from which proxies will redraw.
#'
#' @param data dataframe; A dataframe with x, y, group, and category columns.
#' @param x_label character string; Label for the X Axis.
#'
#' @export
#'
#' @return histogram
set_hist <- function(data,
                     x_label = "Total FTEs") {


  highcharter::hchart(
    data,
    type = "column",
    id = "All",
    name = "Estimated Across All LHDs",
    highcharter::hcaes(x = x, y = y)
  ) %>%
    highcharter::hc_chart(
      zoomType = "x",
      resetZoomButton = list(
        theme = list(
          fill = "white",
          stroke = "#80A14E",
          style = list(
            color = "#80A14E",
            fontWeight = "bold",
            fontFamily = "Jost",
            lineHeight = "25px" # positions text correctly inside button
          ),
          r = 20,  # border radius
          padding = 10,
          states = list(
            hover = list(
              fill = "#80A14E",
              stroke = "white",
              style = list(
                color = "white"  # hover text color
              )
            )
          )
        )
      ),
      events = list(
        selection = highcharter::JS(
          "function(event) {
         if (event.resetSelection) {
           this.update({
             xAxis: {
               labels: {
                 step: 20
               }
             }
           });
         } else if (event.xAxis) {
           this.update({
             xAxis: {
               labels: {
                 step: 5
               }
             }
           });
         }
       }"
        )
      )
    ) %>%
    highcharter::hc_xAxis(
      title = list(text = x_label), # x-axis label
      categories = data$category,
      labels = list(
        step = 20,
        format = "{value:,.0f}"
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(
        text =  "# of LHDs"
      ), # y-axis label
      min = 0,
      labels = list(
        format = "{value:,.0f}"
      )
    )
}



#' Set First Boxplot Histogram
#'
#' @description Create the first highcharter boxplot visualizations from which proxies will redraw.
#'
#' @param data list, for the boxplot, the data needs to be a list.
#'
#' @export
#'
#' @return boxplot
set_box <- function(data) {
  highcharter::highchart() %>%
    highcharter::hc_add_series(
      id = "All Box",
      type = "boxplot",
      data = data,
      name = data[[1]]$name,
      fillColor = "#EEEAE4",
      lineWidth = 2,
      medianWidth = 3,
      stemWidth = 2,
      whiskerWidth = 3
    ) %>%
    highcharter::hc_add_series(
      id = "Mean",
      data = highcharter::list_parse(data[[1]]$mean),
      type = "scatter",
      name = "Mean",
      marker = list(
        symbol = "circle",
        radius = 5,
        fillColor = naccho.viz::naccho_orange
      ),
      tooltip = list(
        pointFormat = "<b>Mean:</b> ${point.y}"
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "USD")
    ) %>%
    highcharter::hc_xAxis(
      categories = as.list(stringr::str_to_title(data[[1]]$category))
    ) %>%
    highcharter::hc_add_theme(
      naccho.viz::basic_theme
    )
}



#' Histogram Bar Proxy Redraw Total FTEs
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#'
#'
#' @export
proxy_to_hist_total <- function(hc,
                                data,
                                meta,
                                responses = sum(data$Responses),
                                ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("State", "Local", "Shared")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Midwest", "South", "West", "Northeast")
    ) %>%
    highcharter::hcpxy_set_data(
      "column",
      data = data,
    ) %>%
    highcharter::hcpxy_update(
      series = list(
        groupPadding = 0,
        list(type = "column"),
        dataLabels = list(
          enabled = FALSE
        )
      ),
      title = list(
        text = meta$title
      ),
      subtitle = list(
        text = unique(data$subtitle)
      ),
      caption = list(
        text = paste0("<b> N: </b>", responses, "<br>", meta$caption)
      ),
      xAxis = list(
        categories = data$category,
        title = list(
          text = unique(data$x_label)
        ),
        labels = list(
          step = 20,
          format = "{value:,.0f}",
          style = list(
            fontSize = "17px",
            textOverflow = "none"
          )
        )
      ),
      tooltip = list(
        pointFormat = meta$tooltip
      ),
      legend = list(
        enabled = FALSE
      )
    )
}


#' Histogram Bar Proxy Redraw for Per Capita FTEs
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Data frame; data object to feed the figure.
#' @param meta Data frame; data object to set options (e.g., title, subtitle, etc).
#' @param responses Numeric value; often derived from a filtered dataframe.
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#'
#'
#' @export
proxy_to_hist_pc <- function(hc,
                             data,
                             meta,
                             responses = sum(data$Responses),
                             ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("State", "Local", "Shared")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Midwest", "South", "West", "Northeast")
    ) %>%
    highcharter::hcpxy_set_data(
      "column",
      data = data
    ) %>%
    highcharter::hcpxy_update(
      series = list(
        groupPadding = 0,
        list(type = "column"),
        dataLabels = list(
          enabled = FALSE
        )
      ),
      title = list(
        text = meta$title
      ),
      subtitle = list(
        text = unique(data$subtitle)
      ),
      caption = list(
        text = paste0("<b> N: </b>", responses, "<br>", meta$caption)
      ),
      xAxis = list(
        categories = data$category,
        title = list(
          text = unique(data$x_label)
        ),
        labels = list(
          step = 20,
          format = "{value:,.1f}",
          style = list(
            fontSize = "17px",
            textOverflow = "none"
          )
        )
      ),
      tooltip = list(
        pointFormat = meta$tooltip
      ),
      legend = list(
        enabled = FALSE
      )
    )
}


#' From Histogram to Single Column Graph
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#'
#'
#' @export
proxy_hist_to_single <- function(hc,
                                 data,
                                 meta,
                                 rotate = 0,
                                 size = "17px",
                                 overflow = "none",
                                 responses = sum(data$Responses),
                                 ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("State", "Local", "Shared")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Midwest", "South", "West", "Northeast")
    ) %>%
    highcharter::hcpxy_set_data(
      "column",
      data = data,
      highcharter::hcaes(x, y)
    ) %>%
    highcharter::hcpxy_update(
      series = list(
        groupPadding = .2,
        type = "column",
        dataLabels = list(
          enabled = TRUE,
          format = meta$data_label,
          overflow = TRUE,
          distance = 5,
          padding = 2,
          style = list(
            color = naccho.viz::naccho_grey,
            fontFamily = "Jost",
            fontWeight = "500",
            fontSize = "14px"
          )
        )
      ),
      title = list(
        text = meta$title
      ),
      subtitle = list(
        text = unique(data$subtitle)
      ),
      caption = list(
        text = paste0("<b> N: </b>", responses, "<br>", meta$caption)
      ),
      xAxis = list(
        categories = as.list(data$category),
        title = list(
          text = unique(data$x_label)
        ),
        labels = list(
          step = 1,
          rotate = rotate,
          style = list(
            fontSize = size,
            textOverflow = overflow
          )
        )
      ),
      tooltip = list(
        pointFormat = meta$tooltip
      ),
      legend = list(
        enabled = FALSE
      )
    )

}




#' From Histogram to Grouped Column Graph
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#'
#'
#' @export
proxy_hist_to_group <- function(hc,
                                data,
                                data_all = NULL,
                                meta,
                                id,
                                colors,
                                rotate = 0,
                                size = "17px",
                                overflow = "none",
                                responses = sum(data$Responses),
                                theme = naccho.viz::hist_theme, ...) {

  if (is.null(data_all)) {

    hc %>%
      highcharter::hcpxy_remove_series(
        id = c("Urban", "Rural")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = c("State", "Local", "Shared")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = c("Midwest", "South", "West", "Northeast")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = "All LHDs"
      ) %>%
      highcharter::hcpxy_add_series(
        data = data,
        "column",
        highcharter::hcaes(x, y, group = group),
        id = id,
        color = colors
      ) %>%
      highcharter::hcpxy_update(
        series = list(
          type = "column",
          dataLabels = list(
            enabled = TRUE,
            format = meta$data_label,
            overflow = TRUE,
            distance = 5,
            padding = 2,
            style = list(
              color = naccho.viz::naccho_grey,
              fontFamily = "Jost",
              fontWeight = "500",
              fontSize = "14px"
            )
          )
        ),
        title = list(
          text = meta$title
        ),
        subtitle = list(
          text = unique(data$subtitle)
        ),
        caption = list(
          text = paste0("<b> N: </b>", responses, "<br>", meta$caption)
        ),
        xAxis = list(
          categories = unique(data$category),
          title = list(
            text = unique(data$x_label)
          ),
          labels = list(
            step = 1,
            rotate = rotate,
            style = list(
              fontSize = size,
              textOverflow = overflow
            )
          )
        ),
        tooltip = list(
          pointFormat = meta$tooltip
        ),
        legend = list(
          enabled = TRUE
        )
      )

  } else {

    hc %>%
      highcharter::hcpxy_remove_series(
        id = c("Urban", "Rural")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = c("State", "Local", "Shared")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = c("Midwest", "South", "West", "Northeast")
      ) %>%
      highcharter::hcpxy_remove_series(
        id = "All LHDs"
      ) %>%
      highcharter::hcpxy_add_series(
        data = data,
        "column",
        highcharter::hcaes(x, y, group = group),
        id = id,
        color = colors
      ) %>%
      highcharter::hcpxy_set_data(
        "column",
        data = data_all,
        highcharter::hcaes(x, y, group = group)
      ) %>%
      highcharter::hcpxy_update(
        series = list(
          type = "column",
          dataLabels = list(
            enabled = TRUE,
            format = meta$data_label,
            overflow = TRUE,
            distance = 5,
            padding = 2,
            style = list(
              color = naccho.viz::naccho_grey,
              fontFamily = "Jost",
              fontWeight = "500",
              fontSize = "14px"
            )
          )
        ),
        title = list(
          text = meta$title
        ),
        subtitle = list(
          text = unique(data$subtitle)
        ),
        caption = list(
          text = paste0("<b> N: </b>", responses, "<br>", meta$caption)
        ),
        xAxis = list(
          categories = unique(data$category),
          title = list(
            text = unique(data$x_label)
          ),
          labels = list(
            step = 1,
            rotate = rotate,
            style = list(
              fontSize = size,
              textOverflow = overflow
            )
          )
        ),
        tooltip = list(
          pointFormat = meta$tooltip
        ),
        legend = list(
          enabled = TRUE
        )
      )

  }
}





#' Single Bar Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param max value, max value for the y axis.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_bar <- function(hc,
                         data,
                         max,
                         meta,
                         name_one = "Jobs Lost",
                         name_two = "Jobs Added",
                         ...) {

  hc %>%
    highcharter::hcpxy_update_series(
      id = "Left",
      name = name_one,
      data = data$left,
      redraw = TRUE
    ) %>%
    highcharter::hcpxy_update_series(
      id = "Right",
      name = name_two,
      data = data$right,
      redraw = TRUE
    ) %>%
    highcharter::hcpxy_update(
      yAxis = list(
        list(
          max = max,
          tickWidth = 0,
          lineWidth = .5
        ),
        list(
          max = max,
          tickWidth = 0,
          lineWidth = .5
        )
      ),
      title = list(
        text = meta$title
      )
    )
}





#' Single Box Plot Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_box <- function(hc, data, meta, ...) {

  hc %>%
    highcharter::hcpxy_update_series(
      id = "All Box",
      type = "boxplot",
      data = data,
      name = data[[1]]$name,
      redraw = TRUE
    ) %>%
    highcharter::hcpxy_update_series(
      id = "Mean",
      data = highcharter::list_parse(data[[1]]$mean),
      type = "scatter",
      name = "Mean",
      marker = list(
        symbol = "circle",
        radius = 5,
        fillColor = naccho.viz::naccho_orange
      ),
      tooltip = list(
        pointFormat = "<b>Mean:</b> ${point.y}"
      ),
      redraw = TRUE
    ) %>%
    highcharter::hcpxy_update(
      title = list(
        text = meta$title
      ),
      title = list(
        text = meta$title
      ),
      caption = list(
        text = paste0("<b> N: </b>", sum(data[[1]]$responses), "<br>",
                      "<b> Note: </b> There were ", data[[1]]$total_outliers,
                      " observations above the upper limit ranging from ",
                      scales::dollar(data[[1]]$min_outlier)," to ",
                      scales::dollar(data[[1]]$max_outlier), ". <br>",
                      meta$caption)
      ),
      xAxis = list(
        categories = as.list(stringr::str_to_title(sapply(data, function(x) x$name)))
      ),
      tooltip = list(
        pointFormat = meta$tooltip
      )
    )
}





#' Box Plot Proxy Redraw - Two
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_group_box <- function(hc, data, meta, ...) {

  get_means <- do.call(rbind, lapply(data, function(d) d$mean))

  hc %>%
    highcharter::hcpxy_update_series(
      id = "All Box",
      type = "boxplot",
      data = data,
      name = data[[1]]$name,
      redraw = TRUE
    ) %>%
    highcharter::hcpxy_update_series(
      id = "Mean",
      data = highcharter::list_parse(get_means),
      type = "scatter",
      name = "Mean",
      marker = list(
        symbol = "circle",
        radius = 5,
        fillColor = naccho.viz::naccho_orange
      ),
      tooltip = list(
        pointFormat = "<b>Mean:</b> ${point.y}"
      ),
      redraw = TRUE
    ) %>%
    highcharter::hcpxy_update(
      title = list(
        text = meta$title
      ),
      title = list(
        text = meta$title
      ),
      caption = list(
        text = paste0("<b> N: </b>", sum(data[[1]]$responses), "<br>",
                      "<b> Note: </b> Refer to the tooltip for information on outliers in each group. <br>",
                      meta$caption)
      ),
      xAxis = list(
        categories = as.list(stringr::str_to_title(sapply(data, function(x) x$name)))
      ),
      tooltip = list(
        pointFormat = "<b> Upper Limit: </b> ${point.high} <br>
                      <b> 75th Percentile: </b> ${point.q3} <br>
                      <b> Median: </b> ${point.median} <br>
                      <b> 25th Percentile: </b> ${point.q1} <br>
                      <b> Lower Limit: </b> ${point.low} <br>
                      <hr class = 'tt'>
                      <b> Responses in Group:</b> {point.group_responses} <br>
                      <b> Number of Outliers:</b> {point.total_outliers} <br>
                      <b> Range of Outliers: </b> ${point.min_outlier:,.0f} to ${point.max_outlier:,.0f}"

      )
    )
}

