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
        textAlign = "center"
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
      margin = 22,
      title = list(
        text = legend_title
      )
    )
}




#' Single Bar Proxy Redraw
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
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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



#' Grouped Bar Proxy Redraw
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
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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
    name = "Across All LHDs",
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
      name = "Across All LHDs",
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
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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
          format = "{value:,.0f}"
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
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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
          format = "{value:,.1f}"
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
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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
        categories = data$category,
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
                                data_all,
                                meta,
                                id,
                                colors,
                                rotate = 0,
                                size = "17px",
                                overflow = "none",
                                responses = sum(data$Responses),
                                theme = naccho.viz::hist_theme, ...) {

  hc %>%
    highcharter::hcpxy_remove_series(
      id = c("Urban", "Rural")
    ) %>%
    highcharter::hcpxy_remove_series(
      id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
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

