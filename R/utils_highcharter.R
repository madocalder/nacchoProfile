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
          symbolFill = naccho.viz::teal,
          symbolStroke = naccho.viz::teal,
          menuItems = c("printChart",
                        "separator",
                        "downloadPDF",
                        "downloadSVG",
                        "separator",
                        "downloadCSV",
                        "downloadXLS",
                        "openInCloud")
        )
      ),

      chart = list(
        backgroundColor = "#FFFFFF"  # Set the background color to white
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
#' @param colors vector or character string; one character or a vector of characters, which must be defined hex values. Defaults to naccho.viz::colors.
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
                        colors = naccho.viz::colors,
                        theme = naccho.viz::basic,
                        tooltip,
                        tooltip_align = "center",
                        legend_enable = TRUE,
                        legend_title = "",
                        legend_align = "left") {
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
      margin = 60,
      align = caption_align
    ) %>%
    highcharter::hc_tooltip(
      headerFormat = "",
      pointFormat = tooltip,
      borderWidth = 2,
      useHTML = TRUE,
      style = list(
        fontWeight = 400,
        textAlign = tooltip_align,
        fontSize = "13px",
        color = "black"
      ),
      backgroundColor = "#EEEAE5",
      borderRadius = 2,
      headerFormat = "<b>{point.x:.0f} LHDs </b> <hr class = 'tt'>",
      hideDelay = 1000,
      shadow = FALSE
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
    ) %>%
   highcharter::hc_chart(spacingBottom = 10)
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
proxy_to_single <- function(
    hc,
    data,
    meta,
    add_n = paste(sum(data$Responses)),
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
        text = paste("<b> N: </b>", add_n, "<br>", source_line)
      ),
      xAxis = list(
        categories = as.list(unique(data$category)),
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
    add_n = paste(sum(data$Responses)),
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
        text = paste("<b> N: </b>", add_n,"<br>", source_line)
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
                       y_label = "% of LHDs") {

  highcharter::hchart(
    data,
    "column",
    id = "All",
    highcharter::hcaes(
      x = x,
      y = y,
      group = group
    ),
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:.0f}%",
      overflow = TRUE,
      distance = 5,
      padding = 2,
      style = list(
        color = naccho.viz::grey,
        fontFamily = "Heebo",
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
        format = "{value}"
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
      format = "{point.y:.0f}%",
      overflow = TRUE,
      distance = 5,
      padding = 2,
      style = list(
        color = naccho.viz::grey,
        fontFamily = "Heebo",
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
    name = "Denominator of LHDs",
    highcharter::hcaes(x = x, y = y)
  ) %>%
    highcharter::hc_chart(
      panType = "x",
      panning = TRUE,  # Enable panning
      panKey = "shift",
      zoomType = "x",
      events = list(
        load = highcharter::JS(
          "function() {
    const chart = this;

    // Save initial extremes for resetting pan
    chart.initialExtremes = chart.xAxis[0].getExtremes();

    // Create the 'Reset Pan' button, initially hidden
    chart.resetPanButton = chart.renderer.button(
      'Reset Pan',
      window.innerWidth * 0.4,  // Initial x position at 10vw
      window.innerHeight * 0.1, // Initial y position at 12vh
      function() {
        chart.xAxis[0].setExtremes(chart.initialExtremes.min, chart.initialExtremes.max);
        chart.resetPanButton.hide();
      },
      {
        fill: 'white',
        stroke: '#80A14E',
        style: {
          color: '#80A14E',
          fontWeight: 'bold',
          fontFamily: 'Heebo',
          lineHeight: '25px'
        },
        r: 20,
        padding: 10
      }
    )
    .attr({ zIndex: 1000 })  // Ensure button is above chart elements
    .add()
    .hide();  // Start hidden

    // Function to dynamically position 'Reset Pan' button using vw and vh units
    function positionResetPanButton() {
      chart.resetPanButton.attr({
        x: window.innerWidth * 0.4,   // Position at 90% of viewport width
        y: window.innerHeight * 0.1  // Position at 5% of viewport height
      });
    }

    // Initial positioning of 'Reset Pan' button
    positionResetPanButton();

    // Update button position on chart redraw (e.g., resize)
    chart.update({
      chart: {
        events: {
          redraw: positionResetPanButton  // Bind positioning function to redraw event
        }
      }
    });

    // Ensure button repositions on window resize
    window.addEventListener('resize', positionResetPanButton);
  }"
        ),
        redraw = highcharter::JS(
          "function() {
            const chart = this;
            const currentExtremes = chart.xAxis[0].getExtremes();

            // Show the 'Reset Pan' button if panning has moved from the initial extremes
            if (currentExtremes.min !== chart.initialExtremes.min || currentExtremes.max !== chart.initialExtremes.max) {
              chart.resetPanButton.show();
            }
          }"
        )
      ),
      resetZoomButton = list(
        theme = list(
          fill = "white",
          stroke = "#80A14E",
          style = list(
            color = "#80A14E",
            fontWeight = "bold",
            fontFamily = "Heebo",
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
          "function() {
            const chart = this;
            function updateStep() {
              const width = chart.chartWidth;
              let stepSize;
              if (width > 1200) {
                stepSize = 10;
              } else if (width > 600) {
                stepSize = 25;
              } else {
                stepSize = 50;
              }
              chart.xAxis[0].update({
                labels: {
                  step: stepSize
                }
              });
            }
            updateStep(); // Initial call
            window.addEventListener('resize', updateStep); // Call on resize
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
      ),
      max = 20,
      min = 0
    ) %>%
    highcharter::hc_yAxis(
      title = list(
        text =  "# of LHDs"
      ), # y-axis label
      min = 0,
      minRange = 1,
      allowDecimals = FALSE,
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
        fillColor = naccho.viz::terracotta
      ),
      tooltip = list(
        pointFormat = "<b>Mean:</b> ${point.y:.0f}"
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "USD")
    ) %>%
    highcharter::hc_xAxis(
      categories = as.list(stringr::str_to_title(data[[1]]$category))
    ) %>%
    highcharter::hc_add_theme(
      naccho.viz::basic
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
        visible = TRUE,
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
            fontSize = "14px",
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
        visible = TRUE,
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
            fontSize = "14px",
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
        visible = TRUE,
        groupPadding = .2,
        type = "column",
        dataLabels = list(
          enabled = TRUE,
          format = meta$data_label,
          overflow = TRUE,
          distance = 5,
          padding = 2,
          style = list(
            color = naccho.viz::grey,
            fontFamily = "Heebo",
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
        categories = as.list(unique(data$category)),
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
                                theme = naccho.viz::histogram, ...) {

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
          visible = FALSE,
          type = "column",
          dataLabels = list(
            enabled = TRUE,
            format = meta$data_label,
            overflow = TRUE,
            distance = 5,
            padding = 2,
            style = list(
              color = naccho.viz::grey,
              fontFamily = "Heebo",
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
          categories = as.list(unique(data$category)),
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
          visible = FALSE,
          dataLabels = list(
            enabled = TRUE,
            format = meta$data_label,
            overflow = TRUE,
            distance = 5,
            padding = 2,
            style = list(
              color = naccho.viz::grey,
              fontFamily = "Heebo",
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
        fillColor = naccho.viz::terracotta
      ),
      tooltip = list(
        pointFormat = "<b>Mean:</b> ${point.y:.0f}"
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
        fillColor = naccho.viz::terracotta
      ),
      tooltip = list(
        pointFormat = "<b>Mean:</b> ${point.y:.0f}"
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
        pointFormat = "<b> Upper Limit: </b> ${point.high:.0f} <br>
                      <b> 75th Percentile: </b> ${point.q3:.0f} <br>
                      <b> Median: </b> ${point.median:.0f} <br>
                      <b> 25th Percentile: </b> ${point.q1:.0f} <br>
                      <b> Lower Limit: </b> ${point.low:.0f} <br>
                      <hr class = 'tt'>
                      <b> Responses in Group:</b> {point.group_responses:.0f} <br>
                      <b> Number of Outliers:</b> {point.total_outliers:.0f} <br>
                      <b> Range of Outliers: </b> ${point.min_outlier:,.0f} to ${point.max_outlier:,.0f}"

      )
    )
}



#' Scatter Plot
#'
#' @param data Dataframe for the viz
#'
#' @return A redrawn figure.
#' @export
#'
scatter_plot <- function(data, colors) {

  data_for_n <- data

  if (sum(stringr::str_detect(data_for_n$filter, "All LHDs")) > 0) {

    n <- data_for_n$demoninator

  } else {

    data_for_n <- data_for_n %>%
      dplyr::group_by(service_type, filter) %>%
      dplyr::summarize(n = sum(demoninator))

    n <- data_for_n$n
  }

  req(n)
  highcharter::highchart() %>%
    highcharter::hc_add_series(
      id = "All",
      data = data,
      type = "scatter",
      highcharter::hcaes(x = x, y = y, group = Group, name = service_type)
    ) %>%
    highcharter::hc_chart(
      zoomType = "xy",
      resetZoomButton = list(
        theme = list(
          fill = "white",
          stroke = "#80A14E",
          style = list(
            color = "#80A14E",
            fontWeight = "bold",
            fontFamily = "Heebo",
            lineHeight = "25px" # positions text correctly inside button
          ),
          r = 20,  # border radius
          padding = 10,
          states = list(
            hover = list(
              fill = "#80A14E",
              stroke = "white",
              style = list(
                color = "white"
              )
            )
          )
        )
      )
    ) %>%
    highcharter::hc_plotOptions(
      scatter = list(
        events = list(
          click = highcharter::JS("function() {
                     this.selectionMarker.destroy();
                     }")
        ),
        dataLabels = list(
          enabled = TRUE,
          allowOverlap = TRUE,
          formatter = highcharter::JS("
          function() {
            return this.point.name.length > 12 ? this.point.name.substring(0, 12) + '...' : this.point.name;
          }
        "),
          style = list(
            textOutline = "none",
            fontFamily = "Heebo",
            fontSize = "11px",
            fontWeight = "bold"
          )
        ),
        marker = list(
          radius = 7,
          lineWidth = 1
        )
      )
    ) %>%
    highcharter::hc_xAxis(
      min = 0,
      max = 35,
      title = list(
        text = "Percent of LHDs reducing service",
        align = "high"
      ),
      plotLines = list(
        list(
          value = 20,
          color = "gray",
          width = 2,
          dashStyle = "ShortDash",
          label = list(
            text = "Average percent reduced (20%)",
            style = list(
              fontFamily = "Heebo"
            )
          )
        )
      ),
      plotBands = list(
        list(
          from = 20,
          to = 35,
          color = "rgba(171, 97, 67, 0.2)",
          label = list(
            text = "",
            verticalAlign = "bottom",
            align = "right")
        )
      )
    ) %>%
    highcharter::hc_yAxis(
      min = 0,
      title = list(
        text = "Percent of LHDs expanding service"
      ),
      plotLines = list(
        list(
          value = 27,
          color = "gray",
          width = 2,
          dashStyle = "ShortDash",
          label = list(
            text = "Average percent expanded (27%)",
            style = list(
              fontFamily = "Heebo"
            )
          )
        )
      ),
      plotBands = list(
        list(
          from = 0,
          to = 27,
          color = "rgba(236, 229, 225, 0.3)",
          label = list(
            text = "",
            verticalAlign = "middle",
            align = "right")
        ),
        list(
          from = 27,
          to = 100,
          color = "rgba(0, 138, 150, 0.2)",
          label = list(
            text = "",
            verticalAlign = "middle",
            align = "left")
        )
      )
    ) %>%
    highcharter::hc_add_theme(
      naccho.viz::scatter
    ) %>%
    highcharter::hc_colors(
      colors = colors
    ) %>%
    # highcharter::hc_annotations(
    #   list(
    #     draggable = FALSE,
    #     labels = list(
    #       list(
    #         point = list(
    #           x = 1,
    #           y = 78,
    #           xAxis = 0,
    #           yAxis = 0
    #         ),
    #         text = HTML('Services Growing'),
    #         style = list(
    #           color = "black",
    #           fontWeight = "bold",
    #           fontSize = "14px",
    #           fontFamily = "Heebo"
    #         ),
    #         backgroundColor = "transparent",
    #         borderColor = "transparent"
    #       ),
    #       list(
    #         point = list(
    #           x = 1,
    #           y = 0,
    #           xAxis = 0,
    #           yAxis = 0
    #         ),
    #         text = HTML('Services Stable'),
    #         style = list(
    #           color = "black",
    #           fontWeight = "bold",
    #           fontSize = "14px",
    #           fontFamily = "Heebo"
    #         ),
    #         backgroundColor = "transparent",
    #         borderColor = "transparent"
    #       ),
    #       list(
    #         point = list(
    #           x = 34,
    #           y = 78,
    #           xAxis = 0,
    #           yAxis = 0
    #         ),
    #         text = HTML('Mixed Trend'),
    #         style = list(
    #           color = "black",
    #           fontWeight = "bold",
    #           fontSize = "14px",
    #           fontFamily = "Heebo"
    #         ),
    #         backgroundColor = "transparent",
    #         borderColor = "transparent"
    #       ),
    #       list(
    #         point = list(
    #           x = 33,
    #           y = 0,
    #           xAxis = 0,
    #           yAxis = 0
    #         ),
    #         text = HTML('Services Shrinking'),
    #         style = list(
    #           color = "black",
    #           fontWeight = "bold",
    #           fontSize = "14px",
    #           fontFamily = "Heebo"
    #         ),
    #         backgroundColor = "transparent",
    #         borderColor = "transparent"
    #       )
    #     )
    #   )
    # ) %>%
    highcharter::hc_title(
      text = "Expansion and Reduction of Services Among LHDs That Provide Service", # title text
      margin = 10,
      widthAdjust = -60
    ) %>%
    highcharter::hc_subtitle(
      text = "Click and span a range on the figure to zoom."
    ) %>%
    highcharter::hc_caption(
      text = HTML("<b> About the Quadrants:</b> <span style=\'font-family: Heebo; font-size: 14px; color: black;\'>
                  In the top left quadrant, services are <span style=\'color: #008A96; font-weight: bold;\'>growing</span>.
                  In the top right quadrant, services show a <span style=\'color: #666; font-weight: bold;\'>mixed trend</span>.
                  In the bottom left quadrant, services are <span style=\'color: #B8B3AE; font-weight: bold;\'>stable</span>.
                  In the bottom right quadrant, services are <span style=\'color: #AB6143; font-weight: bold;\'>shrinking</span>.
                  </span> <br>
                  <b> N: </b>", min(n), " to ", max(n), "<br>",
                  source_line),
      useHTML = TRUE,
      margin = 30,
      align = "center"
    ) %>%
    highcharter::hc_chart(spacingBottom = 0) %>%
    highcharter::hc_legend(enabled = TRUE,
                           padding = 0,
                           itemMarginTop = 0,
                           itemMarginBottom = 0)
}




#' Scatter Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_scatter_single <- function(hc, data,...) {

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
    # highcharter::hcpxy_remove_series(
    #   id = "All"
    # ) %>%
      highcharter::hcpxy_set_data(
        # id = "All",
        data = data,
        type = "scatter",
        mapping = highcharter::hcaes(x = x, y = y, group = Group, name = service_type)
      ) %>%
      highcharter::hcpxy_update(
        chart = list(
          zoomType = "xy"),
        series = list(
          visible = TRUE
        )
      )
}





#' Scatter Proxy Redraw
#'
#' @param hc A `highchart` `htmlwidget` object.
#' @param data Dataframe; data object to feed the figure.
#' @param meta Dataframe; data object to set options (e.g., title, subtitle, etc).
#' @param ... Other arguments
#'
#' @return A redrawn figure.
#' @export
#'
proxy_to_scatter_group <- function(hc, data, data_all, id = NULL, input, colors = NULL,...) {

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
    # highcharter::hcpxy_remove_series(
    #   id = "All"
    # ) %>%
      highcharter::hcpxy_add_series(
        data = data,
        type = "scatter",
        mapping = highcharter::hcaes(x = x, y = y, group = Group, name = service_type),
        id = id,
        color = colors
      ) %>%
    highcharter::hcpxy_set_data(
      data = data_all,
      type = "scatter",
      mapping = highcharter::hcaes(x = x, y = y, group = Group, name = service_type)
    ) %>%
    highcharter::hcpxy_update(
      chart = list(
        zoomType = "xy"
      ),
      series = list(
        visible = FALSE
      )
    )
}
