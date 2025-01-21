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
        enabled = TRUE),
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
          menuItems = c(
            # "printChart",
                        # "separator",
                        "downloadPDF",
                        "downloadSVG",
                        # "separator",
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
                        tooltip = "{point.value}",
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
      # margin = 60,
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
        fontSize = "1.125em",
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
                       y_label = "% of LHDs",
                       y_format = "{value}",
                       data_label = "") {

  data$category <- factor(data$category, levels = unique(data$category))
  data$category <- as.character(data$category)

  # Clean the category column to remove leading/trailing spaces or special characters
  data$category <- trimws(data$category)

  # Also check for non-printing characters or unusual encodings
  data$category <- iconv(data$category, "latin1", "UTF-8")

  print(data$category)

  x_axis_labels <- as.list(unique(data$category))

  print(x_axis_labels)

  highcharter::hchart(
    data,
    type = "column",
    id = "All",
    mapping = highcharter::hcaes(
      x = x,
      y = y,
      group = group,
      category = category
    ),
    dataLabels = list(
      enabled = TRUE,
      allowOverlap = TRUE,
      crop = FALSE,
      format = data_label,
      overflow = TRUE,
      distance = 5,
      padding = .5,
      style = list(
        color = naccho.viz::grey,
        fontFamily = "Heebo",
        fontWeight = "400",
        fontSize = "1em"
      )
    )
  ) %>%
    highcharter::hc_xAxis(
      title = list(text = ""), # x-axis label
      categories = x_axis_labels
    ) %>%
    highcharter::hc_yAxis(
      title = list(
        text = y_label
      ), # y-axis label
      min = 0,
      labels = list(
        format = y_format
      )
    ) %>%
    highcharter::hc_plotOptions(
      column = list(
        pointPadding = 0.3,
        groupPadding = .1
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
set_simple_bar <- function(data, y_label = "",
                           y_format = "{value}",
                           data_label = "{value}") {
  if (!"category" %in% colnames(data)) {
    stop("Category column is missing in the data passed to set_simple_bar")
  }
  if (!"group" %in% colnames(data)) {
    stop("group column is missing in the data passed to set_simple_bar")
  }

  highcharter::hchart(
    data,
    "bar",
    id = "All",
    highcharter::hcaes(
      x = category,
      y = y,
      group = group,
      name = category
    ),
    dataLabels = list(
      enabled = TRUE,
      overflow = "allow",
      format = data_label
      # ,
      # formatter = JS("
      #   function () {
      #     let roundedValue;
      #     if (this.y > 0 && this.y < 1) {
      #       roundedValue = this.y.toFixed(1);
      #     } else {
      #       roundedValue = this.y.toFixed(0);
      #     }
      #     return roundedValue + '%';
      #   }
      # ")
    )
  ) %>%
    highcharter::hc_xAxis(
      title = list(text = ""),
      categories = unique(data$category)  # Explicitly reference `data$category`
    ) %>%
    highcharter::hc_yAxis(
      title = list(
        text = y_label
      ),
      min = 0,
      labels = list(
        format = y_format,
        allowOverlap = FALSE
      )
    )
}



#' Set First Highcharter Butterfly Bar Graph
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
                    # legend_title = "Number of Jobs...",
                    name_one = "Lost",
                    name_two = "Added",
                    max_one = 1300,
                    max_two = 5000,
                    y_format = NA,
                    group_by = NULL) {

  # Preprocess data to include grouping
  if (!is.null(group_by)) {
    data <- data %>%
      dplyr::arrange(!!rlang::sym(group_by)) %>%
      dplyr::mutate(group = dplyr::case_when(group %in% c("13", "All Lhds") ~ "All LHDs",
                                             TRUE ~ group)) %>% # Sort data by grouping column
      dplyr::mutate(group = as.factor(!!rlang::sym(group_by)))
  } else {

    data <- data %>%
      dplyr::mutate(group = dplyr::case_when(group %in% c("13", "All Lhds") ~ "All LHDs",
                                             TRUE ~ group))

    data$group <- factor(data$group)
  }

  #reverse order of cats so All LHDs is on top -----
  data <- data[rev(seq_len(nrow(data))), ]


  max_combined <- sum(max_one, max_two)

  message("max_combined = ", max_combined)

  # Define the "pretty" intervals in a vector
  pretty_intervals <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 1500, 2000, 2500)

  # Calculate one-tenth of max_combined
  rough_interval <- round_up(max_combined / 10)

  # Find the largest "pretty" interval that is less than or equal to rough_interval
  interval <- max(pretty_intervals[pretty_intervals <= rough_interval])

  message("interval = ", interval)

  ticks_one <- ceiling(max_one/interval)
  ticks_two <- ceiling(max_two/interval)

  totalTicks <- sum(ticks_two, ticks_one)

  message("ticks_one = ", ticks_one, "  ticks_two = ", ticks_two,
          " totalTicks = ", totalTicks)

  axis_one <- round((ticks_one/totalTicks), digits = 2)
  axis_two <- round((ticks_two/totalTicks), digits = 2)

  message("axis_one = ", axis_one, "  axis_two =", axis_two)

  # Update max_one and max_two to be multiples of interval
  max_one <- ticks_one * interval
  max_two <- ticks_two * interval

  message("max_one = ", max_one, "   max_two =", max_two)


  highcharter::highchart() %>%
    highcharter::hc_chart(
      type = "bar",
      spacingLeft = 10,
      spacingRight = 10
      # ,
      # height = (length(unique(data$group)) * 60)  # Adjust chart height based on the number of groups
    ) %>%
    highcharter::hc_legend(
      enabled = TRUE,
      layout = "vertical",
      align = "left",
      verticalAlign = "bottom",
      itemStyle = list(fontSize = "0.9375em")
      # ,
      # # y = -20,
      # title = list(
      #   text = legend_title,      # Your legend title here
      #   style = list(fontSize = "1.125em", fontWeight = "bold")  # Customize title style
      # )
    ) %>%
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE
    ) %>%
    highcharter::hc_xAxis(
      categories = unique(data$group),
      labels = list(
        enabled = TRUE,
        allowOverlap = FALSE,
        style = list(fontSize = "1.125em"),
        y = 5,
        autoRotation = c(0, -45),
        step = 1
      ),
      tickInterval = 1,
      plotBands = lapply(seq_along(unique(data$group)), function(i) {
        list(
          from = i - 0.5,
          to = i + 0.5,
          color = ifelse(i %% 2 == 0, "#F9F8F7", "transparent")  # Alternating row colors
        )
      })
    ) %>%
    highcharter::hc_yAxis_multiples(
      list(
        max = max_one,
        title = list(
          text = " "
        ),
        labels = list(
          format = y_format,
          style = list(fontSize = "0.9375em")
        ),
        width = paste((100 * axis_one), "%"),
        reversed = TRUE,
        offset = 10,
        alignTicks = FALSE,
        tickInterval = interval,
        tickAmount = ticks_one + 1,
        opposite = TRUE  # Move this y-axis to the top
      ),
      list(
        max = max_two,
        title = list(
          text = " ",
          # ,
        #   align = "low",
        #   verticalAlign = "bottom",
        #   autoRotation = c(0, -45),
          style = list(fontSize = "1.125em")
        #   # ,
        #   # y = -5,
        #   # x = -150
        ),
        labels = list(
          format = y_format,
          style = list(fontSize = "0.9375em")
        ),
        left = paste((100 * axis_one), "%"),
        width = paste((100 * axis_two), "%"),
        offset = 10,
        alignTicks = FALSE,
        tickInterval = interval,
        tickAmount = ticks_two + 1,
        opposite = FALSE
      )
    ) %>%
    highcharter::hc_add_series(
      id = "Left",
      name = name_one,
      data = data$left,
      yAxis = 0,
      dataLabels = list(
        enabled = TRUE,
        overflow = "allow",
        formatter = JS("function() { return Highcharts.numberFormat(this.y, 0); }"),
        style = list(fontSize = "0.9375em")
      )
      # ,
      # showInLegend = FALSE
    ) %>%
    highcharter::hc_add_series(
      id = "Right",
      name = name_two,
      data = data$right,
      yAxis = 1,
      dataLabels = list(
        enabled = TRUE,
        overflow = "allow",
        formatter = JS("function() { return Highcharts.numberFormat(this.y, 0); }"),
        style = list(fontSize = "0.9375em")
      )
      # ,
      # showInLegend = FALSE
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
      type = "scatter",
      data = data,
      name = data[[1]]$name
      # fillColor = "#EEEAE4",
      # lineWidth = 2,
      # medianWidth = 3,
      # stemWidth = 2,
      # whiskerWidth = 3
    ) %>%
    highcharter::hc_add_series(
      id = "Mean",
      data = highcharter::list_parse(data[[1]]$mean),
      type = "line",
      name = "Mean",
      # marker = list(
      #   symbol = "circle",
      #   radius = 5,
      #   fillColor = naccho.viz::terracotta
      # ),
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

set_hist <- function(data, x_label = "Total FTEs", y_label = "% of LHDs" , x_max =  max(data$x) ) {



  highcharter::hchart(
    data,
    type = "column",
    id = "All",
    name = "All LHDs",
    highcharter::hcaes(x = x, y = y)
  ) %>%
    highcharter::hc_chart(
      panType = "x",
      panning = TRUE,
      panKey = "shift",
      zoomType = "x",
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
                stepSize = 30;
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
    highcharter::hc_plotOptions(
      series = list(
        visible = TRUE,
        groupPadding = 0,
        dataLabels = list(
          enabled = FALSE
        )
      ),
      column = list(
        type = "column"
      )
    ) %>%
    highcharter::hc_xAxis(
      title = list(text = x_label),
      categories = data$category,
      # labels = list(
      #   format = "{value:,.0f%}"
      # ),
      max = x_max,
      min = 0
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = y_label),
      min = 0,
      minRange = 1,
      allowDecimals = FALSE,
      labels = list(
        format = "{value:,.0f}"
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

  # data_for_n <- data
  #
  # if (sum(stringr::str_detect(data_for_n$filter, "All LHDs")) > 0) {
  #
  #   n <- show_n(data$Responses)
  #
  # } else {
  #
  #   data_for_n <- data_for_n %>%
  #     dplyr::group_by(service_type, filter) %>%
  #     dplyr::summarize(n = sum(demoninator))
  #
  #   n <- data_for_n$n
  # }
  #
  # req(n)
data <- data %>%
  dplyr::mutate(
    tt_group = dplyr::case_when(
    group != "All LHDs" ~ tolower(group),
    TRUE ~ "all"),
    name = service_type_short
    )

  highcharter::highchart() %>%
    highcharter::hc_add_series(
      id = "All",
      data = data,
      type = "scatter",
      mapping = highcharter::hcaes(x = x,
                                   y = y,
                                   group = group
                                   # ,
                                   # name = service_type,
                                   # data_label = ""
                                   )

    ) %>%
    highcharter::hc_chart(
      panType = "xy",
      panning = TRUE,
      panKey = "shift",
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
    highcharter::hc_tooltip(
      useHTML = TRUE,
      # headerFormat = '<span style="font-size: 1.125em; font-weight: bold">{series.name}</span><br>',
      pointFormat = '<span style="font-size: 1em; line-height: 0.625em">{point.y}% of
      {point.tt_group} LHDS have expanded {point.name} services and {point.x}% have
      reduced them.</span>',
      style = list(
        fontFamily = "Heebo",
        fontSize = "1.0625em",
        # lineHeight = "0.9375em",
        padding = 5,
        textOutline = "none"
      )
    )%>%
    highcharter::hc_plotOptions(
      scatter = list(
        events = list(
          click = highcharter::JS("function() {
                     this.selectionMarker.destroy();
                     }")
        ),
        dataLabels = list(
          enabled = TRUE,
          allowOverlap = FALSE,
          crop = FALSE,
          overflow = "allow",
          format = '{point.name}',
      #     formatter = highcharter::JS("
      #   function() {
      #     if (this.point.name) {
      #       return this.point.name.length > 15 ? this.point.name.substring(0, 15) + '...' : this.point.name;
      #     }
      #     return null; // Return null if name is missing
      #   }
      # "),
          style = list(
            textOutline = "none",
            fontFamily = "Heebo",
            fontSize = "0.875em",
            fontWeight = "bold"
          )
        ),
        marker = list(
          radius = 7,
          lineWidth = 1,
          color = colors()
        )
      )
    ) %>%
    # # Add interactive horizontal line
    # highcharter::hc_add_series(
    #   name = "Average percent reduced (20%)",
    #   type = "line",
    #   data = list(list(x = 0, y = 20), list(x = 35, y = 20)),
    #   color = "gray",
    #   dashStyle = "ShortDash",
    #   enableMouseTracking = TRUE,
    #   marker = list(enabled = FALSE),
    #   tooltip = list(
    #     pointFormat = "<span style='font-size: 0.9375em;'>Average percent reduced: 20%</span>"
    #   )
    # ) %>%
    # # Add interactive vertical line
    # highcharter::hc_add_series(
    #   name = "Average percent expanded (27%)",
    #   type = "line",
    #   data = list(list(x = 27, y = 0), list(x = 27, y = 100)),
    #   color = "gray",
    #   dashStyle = "ShortDash",
    #   enableMouseTracking = TRUE,
    #   marker = list(enabled = FALSE),
    #   tooltip = list(
    #     pointFormat = "<span style='font-size: 0.9375em;'>Average percent expanded: 27%</span>"
    #   )
    # ) %>%
    highcharter::hc_xAxis(
      min = 0,
      max = 35,
      title = list(
        text = "% of LHDs reducing service",
        align = "high"
      ),
      plotLines = list(
        list(
          value = 20,
          color = "gray",
          width = 2,
          dashStyle = "ShortDash",
          label = list(
            text = "Average % reduced (20%)",
            style = list(
              fontFamily = "Heebo",
              fontSize = "1em"
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
      max = 100,
      title = list(
        text = "% of LHDs expanding service"
      ),
      plotLines = list(
        list(
          value = 27,
          color = "gray",
          width = 2,
          dashStyle = "ShortDash",
          label = list(
            text = "Average % expanded (27%)",
            style = list(
              fontFamily = "Heebo",
              fontSize = "1em"
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
    #           fontSize = "1.4375em",
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
    #           fontSize = "1.4375em",
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
    #           fontSize = "1.4375em",
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
    #           fontSize = "1.4375em",
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
      text = "Click and drag to zoom into a specific area of the chart.
    Hold the Shift key and drag to pan horizontally when zoomed in."
    ) %>%
    highcharter::hc_caption(
      fontSize = "1em",
      text = HTML("<b style='font-size: 1em; line-height: 0.625em;'> About the Quadrants:</b> <span style='font-family: Heebo; font-size: 1em; line-height: 0.875em; color: black;'>",
                  "In the top left quadrant, services are <span style='color: #008A96; font-weight: bold;'>growing</span>. ",
                  "In the top right quadrant, services show a <span style='color: #666; font-weight: bold;'>mixed trend</span>. ",
                  "In the bottom left quadrant, services are <span style='color: #B8B3AE; font-weight: bold;'>stable</span>. ",
                  "In the bottom right quadrant, services are <span style='color: #AB6143; font-weight: bold;'>shrinking</span>. ",
                  "</span> <br> <b style='font-size: 1em; line-height: 0.625em;'> N: </b>",
                  "<span style='font-size: 1em; line-height: 0.625em;'>", show_n(data$Responses), "</span><br>",
                  "<span style='font-size: 1em; line-height: 0.625em;'>", source_line, "</span>"),

      useHTML = TRUE,
      margin = 20,
      align = "center"
    ) %>%
    highcharter::hc_chart(spacingBottom = 0) %>%
    highcharter::hc_legend(enabled = TRUE,
                           align = "right",  # Align legend to the right of the chart
                           verticalAlign = "middle",
                           layout = "vertical",
                           padding = 2,
                           itemStyle = list(
                             fontSize = "1.125em"        # Adjust font size for responsiveness            # Set font weight
                           ),
                           symbolRadius = 6,
                           itemMarginTop = 0,
                           itemMarginBottom = 0)
}

#' Generate Bar Chart Data for Other Topics PAge
#'
#' Retrieves filtered data to be used in bar chart visualizations for various topics.
#' Data is processed differently depending on the specified topic and filtering parameter.
#'
#' @param topic Character string; specifies the topic for which data is needed. Options include:
#'   - `"tech_use"`: Technology use data.
#'   - `"comm_channels"`: Communication channels data.
#'   - `"medical"`: Medical service data.
#'   - `"dental"`: Dental service data.
#'   - `"behavioral"`: Behavioral service data.
#'   - `"chachip"`: Community Health Assessment and Community Health Improvement data.
#'   - `"c12q260"`: Advocacy data for specified groups.
#'   - `"Newc12q502"`: New advocacy data.
#'   - `"m9q240"`: CHIP data.
#'   - `"work"`: Partnerships data from the specified year.
#' @param filtering Character string; specifies the filter criteria, such as `"All LHDs"`. Defaults to `NA`.
#'
#' @details
#' This function dynamically prepares data for bar charts based on the `topic` and `filtering` values.
#' When `filtering` is `"All LHDs"`, it selects data for all Local Health Departments (LHDs), while other filtering values
#' narrow the data accordingly. Each topic undergoes specific data manipulation to ensure it is formatted for visualization.
#'
#' @return A processed data frame containing `x`, `y`, `group`, and `category` columns, ready for bar chart visualization.
#'
#' @export
#'
#' @examples
#' # Retrieve data for technology use across all LHDs
#' tech_data <- get_bar_data("tech_use", filtering = "All LHDs")
#'

get_bar_data <- function(topic, filtering = "All LHDs") {


  # Helper function to aggregate data
  calculate_aggregates <- function(data, category_col, group_col = NULL, extra_cols = NULL) {
    group_vars <- c(category_col, group_col)
    group_vars <- group_vars[!is.null(group_vars)]

    data %>%
      dplyr::filter(
        (as.character(filter) == as.character(filtering)) # %in% TRUE # Handles non-atomic `filter`
      ) %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::summarize(
        Responses = sum(Responses, na.rm = TRUE),
        across(all_of(extra_cols), ~ unique(.x)[1], .names = "{.col}"), # Retain additional columns
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        x = dplyr::dense_rank(!!sym(category_col))
      )
  }

  # Default handling
  if (topic == "tech_use") {


    tech_data_all <- tech_use_data %>%
      dplyr::filter(filter == "All LHDs")%>%
      dplyr::group_by(tech_type) %>%
      dplyr::mutate(Responses = sum(Responses)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(category %in% c("Implemented")) %>%
      dplyr::select(y, group, subtitle, Responses, filter, tech_type ) %>%
      dplyr::rename(category = tech_type)


    data_filtered <- tech_use_data %>%
      dplyr::filter(filter == filtering)%>%
      dplyr::group_by(tech_type) %>%
      dplyr::mutate(Responses = sum(Responses)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(category %in% c("Implemented")) %>%
      dplyr::select(y, group, subtitle, Responses, filter, tech_type) %>%
      dplyr::rename(category = tech_type)

    if (topic == "All LHDs") {

      data <- teach_data_all
    } else {

      data <- dplyr::full_join(tech_data_all, data_filtered)
    }


  } else if (topic == "comm_channels") {
    data <- comms_use_data %>%
      calculate_aggregates(
        category_col = "comms_type",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      ) %>%
      dplyr::rename(category = comms_type)

  } else if (topic == "medical") {
    data <- med_data %>%
      calculate_aggregates(
        category_col = "category",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      )%>%
      dplyr::group_by(group) %>%
      dplyr::mutate(tt = dplyr::case_when(
        (category == "Medical Services Not Provided" & filter == "All LHDs") ~ " all LHDs do not ",
        (category == "Medical Services Provided" & filter == "All LHDs") ~ " all LHDs ",
        (category == "Medical Services Not Provided" & filter == "censusregion") ~ paste0(" ", tolower(group),"ern LHDs do not "),
        (category == "Medical Services Provided" & filter == "censusregion") ~ paste0(" ", tolower(group),"ern LHDs "),
        (category == "Medical Services Not Provided" & !(filter %in% c("All LHDs", "censusregion"))) ~ paste0(" ", tolower(group)," LHDs do not "),
        (category == "Medical Services Provided" & !(filter %in% c("All LHDs", "censusregion"))) ~ paste0(" ", tolower(group)," LHDs ")
      ),
      Responses = sum(Responses))%>%
      dplyr::ungroup()

  } else if (topic == "dental") {
    data <- dental_data %>%
      calculate_aggregates(
        category_col = "category",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      ) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(tt = dplyr::case_when(
        (category == "Dental Services Not Provided" & filter == "All LHDs") ~ " all LHDs do not ",
        (category == "Dental Services Provided" & filter == "All LHDs") ~ " all LHDs ",
        (category == "Dental Services Not Provided" & filter == "censusregion") ~ paste0(" ", tolower(group),"ern LHDs do not "),
        (category == "Dental Services Provided" & filter == "censusregion") ~ paste0(" ", tolower(group),"ern LHDs "),
        (category == "Dental Services Not Provided" & !(filter %in% c("All LHDs", "censusregion"))) ~ paste0(" ", tolower(group)," LHDs do not "),
        (category == "Dental Services Provided" & !(filter %in% c("All LHDs", "censusregion"))) ~ paste0(" ", tolower(group)," LHDs ")
        ),
        Responses = sum(Responses))%>%
      dplyr::ungroup()

  } else if (topic == "behavioral") {
    data <- behav_data %>%
      calculate_aggregates(
        category_col = "category",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      )%>%
      dplyr::group_by(group) %>%
      dplyr::mutate(tt = dplyr::case_when(
        (category == "Behavioral Services Not Provided" & filter == "All LHDs") ~ " all LHDs do not ",
        (category == "Behavioral Services Provided" & filter == "All LHDs") ~ " all LHDs ",
        (category == "Behavioral Services Not Provided" & filter == "censusregion") ~ paste0(" ", tolower(group),"ern LHDs do not "),
        (category == "Behavioral Services Provided" & filter == "censusregion") ~ paste0(" ", tolower(group),"ern LHDs "),
        (category == "Behavioral Services Not Provided" & !(filter %in% c("All LHDs", "censusregion"))) ~ paste0(" ", tolower(group)," LHDs do not "),
        (category == "Behavioral Services Provided" & !(filter %in% c("All LHDs", "censusregion"))) ~ paste0(" ", tolower(group)," LHDs ")
      ),
      Responses = sum(Responses))%>%
      dplyr::ungroup()


     } else if (topic %in% c("chachip", "c7q149", "c7q147")) {

       data <- if (filtering == "All LHDs" || is.null(filtering)) {
         return_survey_weighted_counts(topic)
       } else {
         return_survey_weighted_counts(topic, filtering)
       }

       if (topic == "chachip") {
         data <- data %>%
           dplyr::mutate(
             category = dplyr::recode(
               category,
               "Yes" = "Completed both in past 5 years",
               "No" = "Completed one or none in past 5 years"
             ),
             tt = dplyr::case_when(
               category == "Completed both in past 5 years" & filtering == "All LHDs" ~
                 " all LHDs completed both within the last 5 years.",
               category == "Completed one or none in past 5 years" & filtering == "All LHDs" ~
                 " all LHDs have completed one or none in the past 5 years.",

               category == "Completed both in past 5 years" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs completed both within the last 5 years."),
               category == "Completed one or none in past 5 years" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed one or none in the past 5 years."),

               category == "Completed both in past 5 years" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs completed both within the last 5 years."),
               category == "Completed one or none in past 5 years" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed one or none in the past 5 years."),
               TRUE ~ NA_character_
             )
           )
       } else if (topic == "c7q147") {
         data <- data %>%
           dplyr::mutate(
             category = dplyr::case_when(
               stringr::str_detect(category, "Yes, Within") ~
                 "Completed CHIP within last 3 years",
               stringr::str_detect(category, "Yes, > 3") ~
                 "Completed CHIP within the last 3 to 5 years",
               stringr::str_detect(category, "Yes, > 5") ~
                 "Completed CHIP more than 5 years ago",
               TRUE ~ "Has not yet completed CHIP"
             )
           ) %>%
           dplyr::group_by(category, group) %>%
           dplyr::reframe(
             y = sum(y),
             Responses = sum(Responses),
             group = unique(group),
             category = unique(category)
           ) %>%
           dplyr::mutate(
             tt = dplyr::case_when(
               category == "Completed CHIP within last 3 years" & filtering == "All LHDs" ~
                 " all LHDs completed a CHIP within the last 3 years.",
               category == "Completed CHIP within the last 3 to 5 years" & filtering == "All LHDs" ~
                 " all LHDs have completed a CHIP within the last 3 to 5 years.",
               category == "Completed CHIP more than 5 years ago" & filtering == "All LHDs" ~
                 " all LHDs have completed a CHIP more than 5 years ago.",
               category == "Has not yet completed CHIP" & filtering == "All LHDs" ~
                 " all LHDs have not completed a CHIP.",

               category == "Completed CHIP within last 3 years" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed a CHIP within the last 3 years."),
               category == "Completed CHIP within the last 3 to 5 years" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed a CHIP within the last 3 to 5 years."),
               category == "Completed CHIP more than 5 years ago" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed a CHIP more than 5 years ago."),
               category == "Has not yet completed CHIP" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have not completed a CHIP."),

               category == "Completed CHIP within last 3 years" &  !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed a CHIP within the last 3 years."),
               category == "Completed CHIP within the last 3 to 5 years" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed a CHIP within the last 3 to 5 years."),
               category == "Completed CHIP more than 5 years ago" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed a CHIP more than 5 years ago."),
               category == "Has not yet completed CHIP" &  !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have not completed a CHIP."),


               TRUE ~ NA_character_
             )
           )
       } else {
         data <- data %>%
           dplyr::mutate(
             category = dplyr::case_when(
               stringr::str_detect(category, "Yes, Within") ~
                 "Completed CHA within last 3 years",
               stringr::str_detect(category, "Yes, > 3") ~
                 "Completed CHA within the last 3 to 5 years",
               stringr::str_detect(category, "Yes, > 5") ~
                 "Completed CHA more than 5 years ago",
               TRUE ~ "Has not yet completed CHA"
             )
           ) %>%
           dplyr::group_by(category, group) %>%
           dplyr::reframe(
             y = sum(y),
             Responses = sum(Responses),
             group = unique(group),
             category = unique(category)
           ) %>%
           dplyr::mutate(
             tt = dplyr::case_when(
               category == "Completed CHA within last 3 years" & filtering == "All LHDs" ~
                 " all LHDs completed a CHA within the last 3 years.",
               category == "Completed CHA within the last 3 to 5 years" & filtering == "All LHDs" ~
                 " all LHDs have completed a CHA within the last 3 to 5 years.",
               category == "Completed CHA more than 5 years ago" & filtering == "All LHDs" ~
                 " all LHDs have completed a CHA more than 5 years ago.",
               category == "Has not yet completed CHA" & filtering == "All LHDs" ~
                 " all LHDs have not completed a CHA.",


               category == "Completed CHA within last 3 years" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed a CHA within the last 3 years."),
               category == "Completed CHA within the last 3 to 5 years" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed a CHA within the last 3 to 5 years."),
               category == "Completed CHA more than 5 years ago" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have completed a CHA more than 5 years ago."),
               category == "Has not yet completed CHA" & filtering == "censusregion" ~
                 paste0(" ", tolower(group),"ern LHDs have not completed a CHA."),

               category == "Completed CHA within last 3 years" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed a CHA within the last 3 years."),
               category == "Completed CHA within the last 3 to 5 years" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed a CHA within the last 3 to 5 years."),
               category == "Completed CHA more than 5 years ago" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have completed a CHA more than 5 years ago."),
               category == "Has not yet completed CHA" & !(filtering %in% c("censusregion", "All LHDs")) ~
                 paste0(" ", tolower(group)," LHDs have not completed a CHA."),


               TRUE ~ NA_character_
             )
           )
       }


data <- data %>%
  dplyr::mutate(Responses = sum(Responses))


  } else if (topic == "c12q260") {
    data <- advocacy_data %>%
      calculate_aggregates(
        category_col = "advocacy_type",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      ) %>%
      dplyr::rename(category = advocacy_type)

  } else if (topic == "Newc12q502") {
    data <- new_advocacy_data %>%
      calculate_aggregates(
        category_col = "new_advocacy_type",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      ) %>%
      dplyr::rename(category = new_advocacy_type)

  } else if (topic == "m9q240") {
    data <- chip_data %>%
      calculate_aggregates(
        category_col = "action",
        group_col = "group",
        extra_cols = c("Question", "subtitle", "y", "filter")
      ) %>%
      dplyr::rename(category = action)

  } else if (topic %in% c("health_part", "comm_part", "gov_part")) {





    data <- partnerships %>%
      dplyr::filter(partner_type == topic,
                    filter %in% c(filtering, "All LHDs"),
                    suffix == "work3"

      ) %>%

      dplyr::group_by(partner, group) %>%
      # dplyr::group_by(group) %>%
      # dplyr::group_by(Question) %>%
      dplyr::mutate(Responses = sum(Responses)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(category = dplyr::case_when(
        category == "No Relationship, Exchange Info Or Meetings," ~ "Unchecked",
        TRUE ~ "Checked")) %>%
      dplyr::filter(category == "Checked")   %>%
      dplyr::select(y, group, subtitle, Responses, filter, partner)%>%
      dplyr::rename(category = partner)



    # %>%
    #   calculate_aggregates(
    #     category_col = "category",
    #     group_col = "group",
    #     extra_cols = c("y", "filter")
    #   )
  }

  # Ensure the 'category' column exists in the final dataset
  if (!"category" %in% colnames(data)) {
    stop("Error in get_bar_data: 'category' column is missing in the output.")
  }

  return(data)
}



#' Process Classification Data
#'
#' This function processes classification data based on the selected topic, classifications, and filtering options.
#'
#' @param topic A string representing the current topic (e.g., "a", "b").
#' @param classifications A character vector of selected classifications (e.g., `c("c5q63", "c5q52")`).
#' @param filtering A string representing the filtering option (e.g., "All LHDs", "Urban LHDs").
#' @param q_remap A named character vector mapping classification questions to display-friendly titles.
#'                For example, `c("c5q63" = "Question 63", "c5q52" = "Question 52")`.
#'
#' @return A data frame containing the processed classification data. The returned data frame includes:
#'         - Filtered rows matching the selected classifications and filtering option.
#'         - Mutated columns for user-friendly category and group values.
#'
#' @examples
#' \dontrun{
#' # Define inputs
#' topic <- "a"
#' classifications <- c("c5q63", "c5q52")
#' filtering <- "Urban LHDs"
#' q_remap <- c("c5q63" = "Question 63", "c5q52" = "Question 52")
#'
#' # Process data
#' processed_data <- process_classification_data(topic, classifications, filtering, q_remap)
#'
#' # View results
#' head(processed_data)
#' }
#'
#' @importFrom dplyr filter mutate case_when
#' @export
process_classification_data <- function(topic, classifications, filtering) {

  if (!is.null(classifications) && length(classifications) >= 1) {
    selected_vars <- paste0(classifications, topic)
    selected_data <- get(paste0("classification_data_", topic))

    data_all_class <- selected_data %>%
      dplyr::filter(Question %in% selected_vars) %>%
      dplyr::filter(category == "Yes", filter == "All LHDs") %>%
      dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(Question)]),
                    group = dplyr::case_when(group %in% c("13", "All Lhds") ~ "All LHDs", TRUE ~ group))

    if (topic == "a") {

      data_all_class_n <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::group_by(Question, group)%>%
        dplyr::reframe(Question = unique(Question),
                       Responses = sum(Responses),
                       group = unique(group))%>%
        dplyr::select(Responses, Question, group)


      data_all_class <- data_all_class %>%
      dplyr::mutate(filter = filtering,
                    y = round_up(Percentage, 0))%>%
        dplyr::select(-Responses) %>%
        dplyr::left_join(data_all_class_n, by = c("Question", "group"))


    } else {
      data_all_class <- data_all_class %>%
        dplyr::mutate(filter = filtering,
                      y = round_up(Total, -2))

    }

    data_all_class <- data_all_class %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(x = seq_along(category) - 1) %>%
      dplyr::ungroup()

    if (topic == "a") {

      data_class_n <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::group_by(Question, group)%>%
        dplyr::reframe(Question = unique(Question),
                       Responses = sum(Responses),
                       group = unique(group))%>%
        dplyr::select(Responses, Question, group)

    data_filtered <- selected_data %>%
      dplyr::filter(Question %in% selected_vars) %>%
      dplyr::filter(category == "Yes", filter == filtering) %>%
      dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(.data$Question)]),
                    y = round_up(Percentage, 0)) %>%
      dplyr::select(-Responses) %>%
      dplyr::left_join(data_class_n, by = c("Question", "group"))


    } else {
      data_filtered <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::filter(category == "Yes", filter == filtering) %>%
        dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(.data$Question)]),
                      y = round_up(Total, -2))
    }

    data <- bind_rows(data_all_class, data_filtered) %>%
      dplyr::mutate(filter = filtering) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(x = seq_along(category) - 1) %>%
      dplyr::ungroup()

    if (filtering == "All LHDs") {
      return(data_all_class)
    } else {
      return(data)
    }

  } else {

    return(NULL)
  }
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

  if (nrow(data) == 0) {
    stop("No data available for proxy_to_single.")
  }

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
      mapping = highcharter::hcaes(x, y, group = Group, names = category)
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
        offset = 0,
        labels = list(
          pointPadding = .25
        )
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
    highcharter::hcpxy_remove_series(id = "all_lhds_series") %>%
    highcharter::hcpxy_remove_series(id = "filtered_series") %>%
    highcharter::hcpxy_add_series(
      data = data,
      "column",
      mapping = highcharter::hcaes(x, y, group = group),
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
          text = meta$y_label,
          style = list(
            fontSize = "1.125em"
          )
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
                                x_max = NULL,
                                ...) {

  if (is.null(x_max )){
    x_max <- max(data$x)}


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
      chart = list(
        panning = TRUE,
        zoomType = "x"
      ),
      series = list(
        visible = TRUE,
        groupPadding = 0,
        pointPadding = 0,
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
        max = x_max,
        min = 0,
        title = list(
          text = unique(data$x_label)
        )
        # ,
        # labels = list(
        #   step = 1,
        #   format = "{value:,.0f}",
        #   style = list(
        #     fontSize = "1.125em",
        #     textOverflow = "none"
        #   )
        # )
      ),
      tooltip = list(
        pointFormat = meta$tooltip
      ),
      legend = list(
        enabled = FALSE
      )
      # ,
      # events = list(
      #   selection = highcharter::JS(
      #     "function() {
      #       const chart = this;
      #       function updateStep() {
      #         const width = chart.chartWidth;
      #         let stepSize;
      #         if (width > 1200) {
      #           stepSize = 10;
      #         } else if (width > 600) {
      #           stepSize = 30;
      #         } else {
      #           stepSize = 50;
      #         }
      #         chart.xAxis[0].update({
      #           labels: {
      #             step: stepSize
      #           }
      #         });
      #       }
      #       updateStep();
      #       window.addEventListener('resize', updateStep);
      #     }"
      #   )
      # )
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
                             x_max = NULL,
                             ...) {

  if (is.null(x_max )){
    x_max <- max(data$x)}

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
      chart = list(
        panning = TRUE,
        zoomType = "x"
      ),
      series = list(
        visible = TRUE,
        groupPadding = 0,
        pointPadding = 0,
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
        max = x_max,
        categories = data$category,
        title = list(
          text = unique(data$x_label)
        )
        # ,
        # labels = list(
        #   step = 20,
        #   format = "{value:,.0f}",
        #   style = list(
        #     fontSize = "1.125em",
        #     textOverflow = "none"
        #   )
        # )
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
                                 size = "1.125em",
                                 overflow = "none",
                                 responses = sum(data$Responses),
                                 theme = naccho.viz::basic,
                                 ...) {


    x_max <- max(data$x)


  hc %>%
    # Attempt to remove all possible series by specifying their IDs
    highcharter::hcpxy_remove_series(id = c("Urban", "Rural", "Small (<50,000)",
                                            "Medium (50,000-499,999)", "Large (500,000+)",
                                            "State", "Local", "Shared",
                                            "Midwest", "South", "West", "Northeast",
                                            "All LHDs", "HistogramSeries", "CategoricalSeries")) %>%

    highcharter::hcpxy_update(
      chart = list(
        panning = FALSE,
        zoomType = ""
      )
    ) %>%

    # Set new data directly as a replacement, not as an additional series
    highcharter::hcpxy_set_data(
      "column",
      data = data %>% dplyr::mutate(x = as.character(x)),  # Ensure x is categorical
      mapping = highcharter::hcaes(x = x, y = y)
    ) %>%

    highcharter::hcpxy_update(
      series = list(
        visible = TRUE,
        pointPadding = .25,
        type = "column",  # Enforce column type
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
            fontSize = "1.125em"
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

      # xAxis settings for categorical interpretation
      xAxis = list(
        type = "category",  # Set x-axis type to categorical
        categories = as.list(data$category), # Provide categories explicitly
        # label =
        tickInterval = 1,  # Ensure labels are spaced as intended
        min = 0,
        max = x_max,
        title = list(
          text = unique(data$x_label)
        ),
        labels = list(
          pointPadding = .25,
          # step = 1,
          # rotate = rotate,
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
      ),
      theme = theme
    )

    # View(data)
    # Force a redraw to apply all changes
    # highcharter::hcpxy_redraw()
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
                                size = "1.5625em",
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
        mapping = highcharter::hcaes(x, y, group = group),
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
              fontSize = "1.125em"
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
        mapping = highcharter::hcaes(x, y, group = group),
        id = id,
        color = colors
      ) %>%
      highcharter::hcpxy_set_data(
        "column",
        data = data_all,
        mapping = highcharter::hcaes(x, y, group = group)
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
              fontSize = "1.125em"
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
                         max_one,
                         max_two,
                         meta,
                         name_one = "Jobs Lost",
                         name_two = "Jobs Added",
                         y_label = NULL,
                         ...) {


  max_combined <- sum(max_one, max_two)

  # Define the "pretty" intervals in a vector
  pretty_intervals <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 1500, 2000, 2500)

  # Calculate one-tenth of max_combined
  rough_interval <- round_up(max_combined / 10)

  # Find the largest "pretty" interval that is less than or equal to rough_interval
  interval <- max(pretty_intervals[pretty_intervals <= rough_interval])

  ticks_one <- ceiling(max_one/interval)
  ticks_two <- ceiling(max_two/interval)

  totalTicks <- sum(ticks_two, ticks_one)

  axis_one <- round_up((ticks_one/totalTicks), 2)

  axis_two <- round_up((ticks_two/totalTicks), 2)

  # message("TICK, AXIS, INTERVAL CHECKs-----")
  #
  # print(paste("max_one =", max_one))
  # print(paste("max_two =", max_two))
  # print(paste("ticks_one =", ticks_one))
  # print(paste("ticks_two =", ticks_two))
  # print(paste("axis_one =", axis_one))
  # print(paste("axis_two =", axis_two))
  # print(paste("interval =", interval))


  # Update max_one and max_two to be multiples of interval
  max_one <- ticks_one * interval
  max_two <- ticks_two * interval

  # axis_one <- round_up((max_one/(max_one + max_two)), digits = 2)
  #
  # axis_two <- 1 - round_up((max_one/(max_one + max_two)), digits = 2)
#
#   print(paste("max_one's updated value is:", max_one))
#   print(paste("max_two's updated value is:", max_two))

  # print(paste("updated axis_one =", axis_one))
  # print(paste("updated axis_two =", axis_two))
  #



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
          width = paste((100*axis_one), "%"),
          max = max_one,
          tickWidth = 0,
          lineWidth = .5,
          tickInterval = interval,
          tickAmount = ticks_one + 1
        ),
        list(
          width = paste((100*axis_two), "%"),
          left = paste((100*axis_one), "%"),
          title = list(text = y_label,
                       y = 20),
          max = max_two,
          tickWidth = 0,
          lineWidth = .5,
          tickInterval = interval,
          tickAmount = ticks_two + 1
        )
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
        pointFormat = "<b>Mean:</b> ${point.y:.0f}",
        fontSize = "0.9375em"
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
        mapping = highcharter::hcaes(x = x, y = y, group = group, name = service_type)
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
        mapping = highcharter::hcaes(x = x, y = y, group = group, name = service_type),
        id = id,
        color = colors
      ) %>%
    highcharter::hcpxy_set_data(
      data = data_all,
      type = "scatter",
      mapping = highcharter::hcaes(x = x, y = y, group = group, name = service_type)
    ) %>%

    highcharter::hcpxy_update(
      chart = list(
        zoomType = "xy"
      ),
      series = list(
        visible = FALSE
      ),
      tooltip = list(
        useHTML = TRUE,
      # headerFormat = '<span style="font-size: 1.125em; font-weight: bold">{series.name}</span><br>',
      pointFormat = '<span style="font-size: 1em; line-height: 0.625em">{point.y}% of {point.Group} LHDS have expanded {point.name} services and {point.x}% have reduced them.</span>',
      style = list(
        fontFamily = "Heebo",
        fontSize = "1.0625em",
        # lineHeight = "0.9375em",
        padding = 5,
        textOutline = "none"
      )
    )
  )
}
