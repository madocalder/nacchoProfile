#' helpers
#'
#' @param image_file String character representing the file name of the
#'  image to display in the card. The file should be located here: /nacchoProfile/inst/app/www
#' @param date Date class, for example, as.Date("2024/01/01")
#'
#' @description This function creates the story thumbnails that appear on the
#'  landing page of the stories_tab.
#'
#' @return returns an image and date
#'
#' @noRd
card_thumbnail <- function(
    image,
    date) {
  div(
    img(src = image, height = "200px", width = "300px", style = "border-radius:50px 0px 50px 0px;"),
    p(as.Date(date, format = "%m/%d/%Y"), style = "padding-top: .6em")
  )
}



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
#' @param tooltip_header character string; text and values to appear in the tooltip header, use {point.xx} to call a value from the data frame, where xx is a specific column.
#' @param legend_enable boolean, TRUE or FALSE, where TRUE enables the legend. Defaults to TRUE.
#' @param legend_title character string; title of the legend, defaults to nothing unless defined.

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
      caption = list(
        text = meta$caption
      ),
      xAxis = list(
        categories = data$category,
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
  ) %>%
  highcharter::hcpxy_redraw()
}



# hc %>%
#   highcharter::hcpxy_remove_series(
#     id = c("Urban", "Rural")
#   ) %>%
#   highcharter::hcpxy_remove_series(
#     id = c("Small (<50,000)", "Medium (50,000-100,000)", "Large (500.000+)")
#   ) %>%
#   highcharter::hcpxy_remove_series(
#     id = c("State", "Local", "Shared")
#   ) %>%
#   highcharter::hcpxy_remove_series(
#     id = c("Midwest", "South", "West", "Northeast")
#   ) %>%
#   highcharter::hcpxy_remove_series(
#     id = "All"
#   ) %>%
#   highcharter::hcpxy_add_series(
#     data = list(
#       name = data$category,
#       x = data$x,
#       y = data$y)
#   ) %>%
#   highcharter::hcpxy_update(
#     series = list(
#       type = "pie",
#       innerSize = "70%", # Set the inner size for the donut effect
#       allowPointSelect = TRUE, # Enable point selection on the chart
#       cursor = "pointer", # Set cursor style for selectable points
#       showInLegend = TRUE # Show legend for the chart
#     ),
#     yAxis = list(
#       title = list(
#         text = ""
#       ),
#       max = "",
#       labels = list(
#         format = ""
#       )
#     ),
#     title = list(
#       text = meta$title
#     ),
#     caption = list(
#       text = meta$caption
#     ),
#     xAxis = list(
#       categories = "",
#       lineColor = "#fff",
#       lineWidth = 0,
#       offset = 50
#     )
#   ) %>%
#   highcharter::hcpxy_redraw()
# }
#
#
# #

