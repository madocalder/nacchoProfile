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

create_non_time_table <- function(data, category, colors) {

  #Define theme
  options(
    reactable.theme = reactableTheme(
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
        fontSize = "14px",
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
          fontSize = "14px",
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


  # Define the groups and their corresponding columns
  group_definitions <- list(
    Rurality = list(columns = c("y_Urban", "y_Rural")),
    Governance = list(columns = c("y_Local", "y_Shared", "y_State")),
    `LHD size` = list(columns = c("y_Small (<50,000)", "y_Medium (50,000-499,999)", "y_Large (500,000+)")),
    Region = list(columns = c("y_Midwest", "y_Northeast", "y_South", "y_West"))
  )

  # Get the columns for the selected category
  selected_columns <- group_definitions[[category]]$columns

  # Dynamically create column definitions
  column_definitions <- setNames(
    lapply(seq_along(selected_columns), function(i) {
      col <- selected_columns[i]
      col_name <- sub("^y_", "", col)


      colDef(
        name = stringr::str_replace_all(col_name, "_", " "),
        align = "center",
        cell = data_bars(data,
                         text_position = "outside-end",
                         max_value = 100,
                         fill_color = colors[i],
                         background = 'none')
      )
    }),
    selected_columns
  )

  # Add the All LHDs column definition
  column_definitions <- c(
    list(
      category = colDef(
        name = stringr::str_to_title(colnames(data)[1]),
        align = "center",
        style = list(
          fontFamily = "Heebo",
          borderRight = "3px solid #3d4242"
        ),
        headerStyle = list(
          fontWeight = "bold"
        )
      ),
      `y_All LHDs` = colDef(
        name = "All LHDs",
        align = "center",
        cell = data_bars(data,
                         text_position = "outside-end",
                         max_value = 100,
                         fill_color = "#008A96",
                         background = 'none'),
        style = list(
          borderRight = "3px solid #3d4242"
        ),
        headerStyle = list(
          fontWeight = "bolder"
        )
      ),
      subtitle = colDef(
        show = FALSE  # Hide the "Subtitle" column
      )
    ),
    column_definitions
  )

  # Create the reactable table
  reactable(
    data = data,
    columns = column_definitions,
    columnGroups = list(
      colGroup(name = unique(data$subtitle),
               columns = selected_columns)
    ),
    filterable = FALSE,
    showPageSizeOptions = TRUE,
    striped = FALSE,
    highlight = TRUE,
    searchable = FALSE,
    bordered = FALSE,
    resizable = TRUE
  )
}


