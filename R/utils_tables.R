#landscape tables function------

#' Create a Custom Non-Time Table
#'
#' This function creates a customized non-time table using the `reactable` package. It allows for grouping data by categories such as Rurality, Governance, LHD size, and Region, and applying custom themes and color schemes.
#'
#' @param data A data frame containing the data to be displayed in the table. The first column is expected to contain category names, and the remaining columns should contain numeric values.
#' @param category A character string representing the category to group by. Valid options are "Rurality", "Governance", "LHD size", and "Region".
#' @param colors A character vector of colors to be used for the data bars in the table. The length of the vector should match the number of columns in the selected category.
#'
#' @details
#' The function dynamically creates a table with columns corresponding to the selected category, adds a custom theme, and displays data bars within the table cells to visualize the values.
#'
#' @return A reactable table object that can be rendered in a Shiny app or RMarkdown document.
#'
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(
#'   category = c("Category 1", "Category 2", "Category 3"),
#'   y_Urban = c(50, 70, 60),
#'   y_Rural = c(30, 40, 20),
#'   y_All_LHDs = c(40, 60, 50),
#'   subtitle = c("Subtitle 1", "Subtitle 2", "Subtitle 3")
#' )
#'
#' # Define colors for the data bars
#' colors <- c("#FF5733", "#33FF57", "#3357FF")
#'
#' # Create the table
#' create_non_time_table(data, category = "Rurality", colors = colors)
#' }
#'
#' @export
# categorical tables--------

create_non_time_table <- function(data, category, colors, topic, subtopic) {


data <- data %>%
  dplyr::select(category, everything())  # Move 'category' to the first position

#
# if ("y_All LHDs" %in% colnames(data)) {
#   message("y_All LHDs column exists")
# } else {
#   message("y_All LHDs column does not exist")
# }



#number formatting------------------
 num_format <- function(x) {
    rounded_value <- round(x, 1)  # Round to 1 decimal place


    if (topic == "tenure") {
      paste0(rounded_value, " years (mean)")
    } else if (subtopic == "jobs_total") {
      scales::label_comma()(rounded_value)
    } else if (subtopic == "b") {
      paste0(rounded_value, " FTEs")
    } else if (subtopic == "fte_bypop") {
      paste0(rounded_value, " FTE per capita (mean)")
    } else if (subtopic == "c5q37") {
      paste0(rounded_value, " Total FTE (mean)")
    } else if (subtopic == "employee_bypop") {
      paste0(rounded_value, " Employees per capita (mean)")
    } else if (subtopic == "c5q36") {
      paste0(rounded_value, " Total Employees (mean)")
    } else {
      scales::percent_format(scale = 1)(rounded_value)  # Format as percentage with rounded value
    }
  }


  # Define theme----------
  options(
    reactable.theme = reactable::reactableTheme(
      color = "#3d4242",
      backgroundColor = "#F9F8F7",
      borderColor = NULL,
      stripedColor = NULL,
      highlightColor = "#EEEAE4",
      inputStyle = list(
        backgroundColor = "white"
      ),
      selectStyle = list(
        backgroundColor = "white"
      ),
      pageButtonHoverStyle = list(
        backgroundColor = ""
      ),
      pageButtonActiveStyle = list(
        backgroundColor = ""
      ),
      headerStyle = list(
        borderWidth = "3px",
        paddingTop = "10px",
        verticalAlign = "bottom",
        textAlign = "center",
        background = "white",
        textTransform = "uppercase",
        borderColor = "#3d4242",
        color = "#3d4242",
        fontSize = "13px",
        fontWeight = "normal"
      ),
      groupHeaderStyle = list(
        "&:not(:empty)" = list(
          paddingBottom = "2px",
          verticalAlign = "bottom",
          borderWidth = "3px",
          borderColor = "#3d4242",
          textAlign = "center",
          backgroundColor = "#ffffff",
          textTransform = "uppercase",
          fontSize = "13px",
          fontWeight = "bold",
          color = "#3d4242"
        )
      ),
      cellPadding = "15px 25px",
      style = list(
        fontFamily = "Heebo",
        fontSize = "12px"  # Define general font size for table cells here
      )
    )
  )

  # Check if y_All_LHDs column exists in the data
  if ("y_All LHDs" %in% colnames(data)) {
    # Common column definition for y_All_LHDs-------------
    all_lhds_col <- reactable::colDef(
      name = "All LHDs",
      align = "center",
      header = with_tooltip("All LHDs", paste0("n = ", sum(data['Responses_All LHDs']))),
      cell = reactablefmtr::data_bars(data,
                       text_position = "outside-end",
                       max_value = ifelse(topic == "tenure", 8,
                                           ifelse(subtopic == "jobs_total", 15000,
                                                  ifelse(subtopic == "b", 21000,
                                                  ifelse(subtopic == "jobs_standard", 10, 100)))),
                       fill_color = "#008A96",
                       background = 'none',
                       number_fmt = num_format
                    ),
      style = list(
        borderLeft = "3px solid #3d4242"
      ),
      headerStyle = list(
        fontWeight = "bolder"
      )
    )

  } else {
    all_lhds_col <- NULL
  }



#generate table if all LHDS -------------
  # Check if the data is for "All LHDs" a

  if (category == "All LHDs" & topic != "jobs"
      # & subtopic == "no subtopic"
      ) {

    # Define column definitions for All LHDs------------
    column_definitions <- list(
      category = reactable::colDef(
        name = "Category",
        align = "center",
        style = list(
          fontFamily = "Heebo",
          borderRight = "3px solid #3d4242"
        ),
        headerStyle = list(
          fontWeight = "bold"
        )
      )
    )

    if (!is.null(all_lhds_col)) {
      column_definitions$'y_All LHDs' <- all_lhds_col
    }

    # Hide the extra column "Responses_All_LHDs"
    hidden_columns <- setdiff(colnames(data), c("category", "y_All LHDs"))
    hidden_col_definitions <- setNames(
      lapply(hidden_columns, function(col) {
        reactable::colDef(show = FALSE)
      }),
      hidden_columns
    )

    # Merge with hidden columns
    column_definitions <- c(column_definitions, hidden_col_definitions)


    # Create the All LHDs reactable table--------
    reactable::reactable(
      data = data,
      columns = column_definitions,
      filterable = FALSE,
      showPageSizeOptions = TRUE,
      striped = FALSE,
      highlight = TRUE,
      searchable = FALSE,
      bordered = FALSE,
      resizable = TRUE
    )

  } else {
    #reactable for other cases--------
    # Handle other cases with the existing logic
    group_definitions <- list(
      Rurality = list(columns = c("y_Urban", "y_Rural")),
      Governance = list(columns = c("y_Local", "y_Shared", "y_State")),
      `LHD size` = list(columns = c("y_Small (<50,000)", "y_Medium (50,000-499,999)", "y_Large (500,000+)")),
      Region = list(columns = c("y_Midwest", "y_Northeast", "y_South", "y_West"))
    )

    # If the selected category is defined in group_definitions
    if (category %in% names(group_definitions)) {
      selected_columns <- group_definitions[[category]]$columns

    } else {
      # Select all columns that start with 'y_' except 'y_All LHDs'
      selected_columns <- grep("^y_", colnames(data), value = TRUE)
      selected_columns <- setdiff(selected_columns, "y_All LHDs")
    }


#
#     selected_columns <- group_definitions[[category]]$columns
#     selected_columns <- selected_columns[selected_columns %in% colnames(data)] # Filter out non-existing columns

    # Ensure y_All_LHDs is included if it exists
    if ("y_All LHDs" %in% colnames(data)) {
      selected_columns <- c(selected_columns, "y_All LHDs")
    }

    column_definitions <- list(
      category = reactable::colDef(
        name = "Category",
        align = "center",
        style = list(
          fontFamily = "Heebo",
          borderRight = "3px solid #3d4242"
        ),
        headerStyle = list(
          fontWeight = "bold"
        )
      )
    )

    if (!is.null(all_lhds_col)) {
      column_definitions$'y_All LHDs' <- all_lhds_col
    }

    # Handle selected columns for non-All LHDs cases
    column_definitions <- c(
      column_definitions,
      setNames(
        lapply(seq_along(selected_columns), function(i) {
          col <- selected_columns[i]
          col_name <- sub("^y_", "", col)
          reactable::colDef(
            name = stringr::str_replace_all(col_name, "_", " "),
            align = "center",
            header = with_tooltip(
              col_name,
              paste0("n = ", sum(data[[paste0("Responses_", col_name)]]))
            ),
            cell = reactablefmtr::data_bars(data,
                             text_position = "outside-end",
                             max_value = ifelse(topic == "tenure", 8,
                                                ifelse(subtopic == "jobs_total", 15000,
                                                       ifelse(subtopic == "b", 21000,
                                                              ifelse(subtopic == "jobs_standard", 10, 100)))),
                             fill_color = colors[i],
                             background = 'none',
                             number_fmt = num_format
                          )

          )
        }),
        selected_columns
      )
    )



    # Hide the extra columns that are not needed
    hidden_columns <- setdiff(colnames(data), c("category", "y_All LHDs", selected_columns ))
    hidden_col_definitions <- setNames(
      lapply(hidden_columns, function(col) {
        reactable::colDef(show = FALSE)
      }),
      hidden_columns
    )

    column_definitions <- c(column_definitions, hidden_col_definitions)


    reactable::reactable(
      data = data,
      columns = column_definitions,
      columnGroups = if (category %in% names(group_definitions)) {
        list(
          reactable::colGroup(
            name = category,
            columns = setdiff(selected_columns, "y_All LHDs")  # Exclude "y_All LHDs" from the group
          )
        )
      } else {
        NULL  # No groups if not in group_definitions
      },

      # columnGroups = if ("subtitle" %in% colnames(data)) {
      #   list(
      #     reactable::colGroup(
      #       name = unique(data$subtitle),
      #       columns = setdiff(selected_columns, "y_All LHDs")  # Exclude "y_All LHDs" from the group
      #     )
      #   )
      # } else {
      #   NULL  # No groups if "subtitle" is not present
      # },
      filterable = FALSE,
      showPageSizeOptions = TRUE,
      striped = FALSE,
      highlight = TRUE,
      searchable = FALSE,
      bordered = FALSE,
      resizable = TRUE
    )
  }
}

# Helper function to add tooltips
with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy::tippy(value, tooltip, ...))
}





# create_non_time_table <- function(data, category, colors) {
#
#   # Extract the relevant Responses columns
#   response_columns <- grep("^Responses_", colnames(data), value = TRUE)
#
#   # Calculate total N counts for each corresponding Responses column
#   total_counts <- sapply(response_columns, function(col) sum(data[[col]], na.rm = TRUE))
#
#   # Define theme
#   options(
#     reactable.theme = reactableTheme(
#       color = "#3d4242",
#       backgroundColor = "#F9F8F7",
#       borderColor = NULL,
#       stripedColor = NULL,
#       highlightColor = "#EEEAE4",
#       inputStyle = list(
#         backgroundColor = "white"
#       ),
#       selectStyle = list(
#         backgroundColor = "white"
#       ),
#       pageButtonHoverStyle = list(
#         backgroundColor = ""
#       ),
#       pageButtonActiveStyle = list(
#         backgroundColor = ""
#       ),
#       headerStyle = list(
#         borderWidth = "3px",
#         paddingTop = "10px",
#         verticalAlign = "bottom",
#         textAlign = "center",
#         background = "white",
#         textTransform = "uppercase",
#         borderColor = "#3d4242",
#         color = "#3d4242",
#         fontSize = "14px",
#         fontWeight = "normal"
#       ),
#       groupHeaderStyle = list(
#         "&:not(:empty)" = list(
#           paddingBottom = "2px",
#           verticalAlign = "bottom",
#           borderWidth = "3px",
#           borderColor = "#3d4242",
#           textAlign = "center",
#           backgroundColor = "#ffffff",
#           textTransform = "uppercase",
#           fontSize = "14px",
#           fontWeight = "bold",
#           color = "#3d4242"
#         )
#       ),
#       cellPadding = "15px 25px",
#       style = list(
#         fontFamily = "Heebo",
#         fontSize = "12px"  # Define general font size for table cells here
#       )
#     )
#   )
#
#   # Define the groups and their corresponding columns
#   group_definitions <- list(
#     Rurality = list(columns = c("y_Urban", "y_Rural")),
#     Governance = list(columns = c("y_Local", "y_Shared", "y_State")),
#     `LHD size` = list(columns = c("y_Small (<50,000)", "y_Medium (50,000-499,999)", "y_Large (500,000+)")),
#     Region = list(columns = c("y_Midwest", "y_Northeast", "y_South", "y_West"))
#   )
#
#   # Get the columns for the selected category
#   selected_columns <- group_definitions[[category]]$columns
#   selected_columns <- selected_columns[selected_columns %in% colnames(data)] # Filter out non-existing columns
#
#   # Dynamically create column definitions
#   column_definitions <- setNames(
#     lapply(seq_along(selected_columns), function(i) {
#       col <- selected_columns[i]
#       col_name <- sub("^y_", "", col)
#       response_col <- paste0("Responses_", col_name)  # Get the corresponding Responses column
#       count <- total_counts[[response_col]]  # Get the count for the current column
#
#       colDef(
#         name = stringr::str_replace_all(col_name, "_", " "),
#         align = "center",
#         header = with_tooltip(col_name, paste0("n = ", count)),
#         cell = reactablefmtr::data_bars(data,
#                          text_position = "outside-end",
#                          max_value = 100,
#                          fill_color = colors[i],
#                          background = 'none',
#                          number_fmt = scales::percent_format(scale = 1))
#       )
#     }),
#     selected_columns
#   )
#
#   # Add the All LHDs column definition if it exists in the data
#   if ("y_All LHDs" %in% colnames(data)) {
#     column_definitions <- c(
#       list(
#         category = colDef(
#           name = stringr::str_to_title(colnames(data)[1]),
#           align = "center",
#           style = list(
#             fontFamily = "Heebo",
#             borderRight = "3px solid #3d4242"
#           ),
#           headerStyle = list(
#             fontWeight = "bold"
#           )
#         ),
#         `y_All LHDs` = colDef(
#           name = "All LHDs",
#           align = "center",
#           header = with_tooltip("All LHDs", paste0("n = ", total_counts[["Responses_All LHDs"]])),
#           cell = reactablefmtr::data_bars(data,
#                            text_position = "outside-end",
#                            max_value = 100,
#                            fill_color = "#008A96",
#                            background = 'none',
#                            number_fmt = scales::percent_format(scale = 1)),
#           style = list(
#             borderRight = "3px solid #3d4242"
#           ),
#           headerStyle = list(
#             fontWeight = "bolder"
#           )
#         )
#       ),
#       column_definitions
#     )
#   }
#
#   # Add the subtitle column definition to hide it
#   column_definitions$subtitle <- colDef(show = FALSE)
#
#   # Add hidden columns for Responses_*
#   response_col_definitions <- setNames(
#     lapply(response_columns, function(col) {
#       colDef(show = FALSE)
#     }),
#     response_columns
#   )
#
#   # Combine the main column definitions with the hidden Response columns
#   column_definitions <- c(column_definitions, response_col_definitions)
#
#   # Create the reactable table
#   reactable(
#     data = data,
#     columns = column_definitions,
#     columnGroups = list(
#       reactable::colGroup(name = unique(data$subtitle),
#                columns = selected_columns)
#     ),
#     filterable = FALSE,
#     showPageSizeOptions = TRUE,
#     striped = FALSE,
#     highlight = TRUE,
#     searchable = FALSE,
#     bordered = FALSE,
#     resizable = TRUE
#   )
# }
#
# # Helper function to add tooltips
# with_tooltip <- function(value, tooltip, ...) {
#   div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
#       tippy(value, tooltip, ...))
# }





#   # Get the columns for the selected category
#   selected_columns <- group_definitions[[category]]$columns
#   selected_columns <- selected_columns[selected_columns %in% colnames(data)] # Filter out non-existing columns
#
#   # Dynamically create column definitions
#   column_definitions <- setNames(
#     lapply(seq_along(selected_columns), function(i) {
#       col <- selected_columns[i]
#       col_name <- sub("^y_", "", col)
#       response_col <- paste0("Responses_", col_name)  # Get the corresponding Responses column
#       count <- total_counts[[response_col]]  # Get the count for the current column
#
#       colDef(
#         name = stringr::str_replace_all(col_name, "_", " "),
#         align = "center",
#         header = with_tooltip(col_name, paste0("n = ", count)),
#         cell = reactablefmtr::data_bars(data,
#                          text_position = "outside-end",
#                          max_value = 100,
#                          fill_color = colors[i],
#                          background = 'none',
#                          number_fmt = scales::percent_format(scale = 1))
#       )
#     }),
#     selected_columns
#   )
#
#   # Add the All LHDs column definition if not already included
#   if ("y_All LHDs" %in% colnames(data)) {
#     column_definitions <- c(
#       list(
#         category = colDef(
#           name = stringr::str_to_title(colnames(data)[1]),
#           align = "center",
#           style = list(
#             fontFamily = "Heebo",
#             borderRight = "3px solid #3d4242"
#           ),
#           headerStyle = list(
#             fontWeight = "bold"
#           )
#         ),
#         `y_All LHDs` = colDef(
#           name = "All LHDs",
#           align = "center",
#           header = with_tooltip("All LHDs", paste0("n = ", total_counts[["Responses_All LHDs"]])),
#           cell = reactablefmtr::data_bars(data,
#                            text_position = "outside-end",
#                            max_value = 100,
#                            fill_color = "#008A96",
#                            background = 'none',
#                            number_fmt = scales::percent_format(scale = 1)),
#           style = list(
#             borderRight = "3px solid #3d4242"
#           ),
#           headerStyle = list(
#             fontWeight = "bolder"
#           )
#         )
#       ),
#       column_definitions
#     )
#   }
#
#   # Add hidden columns for Responses_*
#   response_col_definitions <- setNames(
#     lapply(response_columns, function(col) {
#       colDef(show = FALSE)
#     }),
#     response_columns
#   )
#
#   # Combine the main column definitions with the hidden Response columns
#   column_definitions <- c(column_definitions, response_col_definitions)
#
#   # Create the reactable table
#   reactable(
#     data = data,
#     columns = column_definitions,
#     columnGroups = list(
#       reactable::colGroup(name = unique(data$subtitle),
#                columns = selected_columns)
#     ),
#     filterable = FALSE,
#     showPageSizeOptions = TRUE,
#     striped = FALSE,
#     highlight = TRUE,
#     searchable = FALSE,
#     bordered = FALSE,
#     resizable = TRUE
#   )
# }
