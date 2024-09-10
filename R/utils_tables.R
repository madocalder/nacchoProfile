# Helper to handle common pivoting logic when names from group -------
pivot_data_group <- function(data) {

  data %>%
    dplyr::select(category, group, y, Responses, dplyr::any_of(c("subtitle", "total_employees"))) %>%
    tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))

}


# Helper to handle common pivoting logic when names from category---------
pivot_data_cat <- function(data) {

  data %>%
    dplyr::select(category, group, y, Responses, dplyr::any_of(c("subtitle", "total_employees" ))) %>%
    tidyr::pivot_wider(names_from = category, values_from = c(y, Responses))%>%
    dplyr::rename(category = group)


}

# Helper to mutate and rename categories based on "All Lhds"
# rename_category <- function(data, category_col = "category") {
#   data %>%
#     dplyr::mutate(!!category_col := dplyr::case_when(
#       .[[category_col]] == "All Lhds" ~ "All LHDs",
#       TRUE ~ .[[category_col]]
#     ))
# }

# Helper to calculate overall LHD data ---------
add_all_ldh_count <- function(data) {
  dplyr::left_join(data,
                   data_all %>% dplyr::select(category, `y_All LHDs`, `Responses_All LHDs`),
                   by = "category")
}

# Helper to process weighted statistics ---------
process_weighted_stats <- function(topic, filter = NULL) {
  return_survey_weighted_statistics(topic, filter) %>%
    dplyr::mutate(subtitle = paste0("By ",
                                    names(landscape_filters[landscape_filters == filter]))) %>%
    dplyr::select(category, group, y, subtitle, Responses)
}



# Helper to process jobs-related data -------
process_jobs_data <- function(jobs_data, is_standardized = FALSE) {
  if (is_standardized) {
    jobs_data %>%
      dplyr::mutate(
        group = dplyr::case_when(group == "13" ~ "All LHDs", TRUE ~ group)
      ) %>%
      dplyr::filter(filter == "All LHDs") %>%
      dplyr::select(category, group, y, Responses, total_employees) %>%
      pivot_data_cat() %>%
      dplyr::mutate(
        `y_Jobs Added (Standardized)` = round((`y_Jobs Added` / total_employees) * 100, 1),
        `y_Jobs Lost (Standardized)` = round((`y_Jobs Lost` / total_employees) * 100, 1),
        `Responses_Jobs Added (Standardized)` = `Responses_Jobs Added`,
        `Responses_Jobs Lost (Standardized)` = `Responses_Jobs Lost`
      ) %>%
      dplyr::select(category, `y_Jobs Added (Standardized)`,`y_Jobs Lost (Standardized)`,
                    `Responses_Jobs Added (Standardized)`, `Responses_Jobs Lost (Standardized)`)
  } else {

    jobs_data %>%
      dplyr::mutate(
        group = dplyr::case_when(group == "13" ~ "All LHDs", TRUE ~ group)
      ) %>%
      dplyr::filter(filter == "All LHDs") %>%
      dplyr::select(category, group, y, Responses, total_employees) %>%
      pivot_data_group()
  }
}

#Function to filter data for tables--------
process_data <- function(topic, filter) {
  # Handle All LHDs cases
  if (filter == "All LHDs") {

    if (topic %in% c("FTE_numcat", "employee_numcat")) {
      data <- return_survey_weighted_counts(topic) %>%
        pivot_data_group()

    } else if (topic %in% c("c5q37", "fte_bypop")) {

      data <- process_weighted_stats("c5q37") %>%
        pivot_data_group()


    } else if (topic %in% c("c5q36", "employee_bypop")) {
      data <- process_weighted_stats(topic)


    } else if (topic == "jobs_total") {
      data <- process_jobs_data(jobs_data)


    } else if (topic == "jobs_standard") {
      data <- process_jobs_data(jobs_data, is_standardized = TRUE)

    } else if (input$topic == "field") {
      data <- spec_data_all %>%
        dplyr::select(category, y, subtitle, Responses) %>%
        dplyr::summarise(category = category,
                         'y_All LHDs' = y,
                         'Responses_All LHDs' = sum(Responses),
                         subtitle = subtitle)

    } else {
      data <- return_survey_weighted_counts(input$topic)

      # Calculate total responses and 'y_All LHDs'
      total_responses <- sum(data$Responses, na.rm = TRUE)
      data <- data %>%
        dplyr::mutate(
          y = 100 * (Responses / total_responses),
          Responses = total_responses
        ) %>%
        dplyr::select(category, group, y, Responses) %>%
        dplyr::left_join(., add_all_counts_long(input$topic), by = "category")

    }

  } else {

    # Handle filters other than All LHDs
    if (topic %in% c("FTE_numcat", "employee_numcat")) {

      data_all <- return_survey_weighted_counts(topic) %>%
        pivot_data_group()

      data <- return_survey_weighted_counts(topic, filter) %>%
        pivot_data_group()  %>%
        add_all_ldh_count()


    } else if (topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop")) {
      data <- process_weighted_stats(topic, filter) %>%
        pivot_data_cat()

      data_all <- process_weighted_stats(topic) %>%
        pivot_data_cat()

      data <- process_weighted_stats(topic, filter) %>%
        pivot_data_cat() %>%
        add_all_ldh_count()


    } else if (topic == "occupation_class") {
      req(length(input$classifications) >= 1)
      selected_vars <- c(paste0("c5q63", topic))
      selected_data <- get(paste0("classification_data_", "occupation_class"))


      data <- selected_data %>%
        dplyr::filter(Question %in% selected_vars, filter %in% input$filter) %>%
        dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(.data$Question)])) %>%
        calculate_overall_lhd("category") %>%
        pivot_data_cat()

    } else if (input$topic == "field") {

      data <- spec_data %>% dplyr::filter(filter == input$filter) %>%
        dplyr::select(category, group, y, subtitle, Responses) %>%
        dplyr::group_by(category) %>%
        tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))

      # Calculate overall y and Responses for "All LHDs"
      overall_data <- spec_data %>%
        dplyr::filter(filter == input$filter) %>%
        dplyr::group_by(category) %>%
        dplyr::summarize(
          'y_All LHDs' = sum(y * Responses, na.rm = TRUE) / sum(Responses, na.rm = TRUE),
          'Responses_All LHDs' = sum(Responses, na.rm = TRUE)
        )

      # Join the overall data to the pivoted data
      data <- data %>%
        dplyr::left_join(overall_data, by = "category")

    } else if (input$topic == "tenure") {
      data <- return_survey_weighted_statistics("tenure", input$filter) %>%
        dplyr::mutate(
          'y_All LHDs' = mean(y, na.rm = TRUE),  # Adjust calculation as needed
          'Responses_All LHDs' = sum(Responses, na.rm = TRUE)
        )  %>%
        dplyr::select(category, group, y, subtitle, Responses, 'y_All LHDs', 'Responses_All LHDs') %>%
        tidyr::pivot_wider(names_from = category, values_from = c(y, Responses)) %>%
        dplyr::rename(category = group)



    } else if (input$topic == "totpop") {
      data <- master_data %>%
        haven::as_factor() %>%
        dplyr::mutate(category = stringr::str_to_title(get(input$filter)),
                      fig_value = get("totpop")) %>%
        dplyr::group_by(category)%>%
        dplyr::summarise(Responses = n(),
                         fig_value = sum(fig_value))%>%
        dplyr::mutate(y = round(fig_value/sum(fig_value)*100,1),
                      subtitle = paste0("By ", names(landscape_filters[landscape_filters == input$filter]))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(group = "All LHDs") %>%
        dplyr::select(category, group, y, subtitle, Responses) %>%
        tidyr::pivot_wider(names_from = category, values_from = c(y, Responses))

      overall_totals <- master_data %>%
        dplyr::summarise('Responses_All LHDs' = n(),
                         fig_value_All_LHDs = sum(get("totpop"))) %>%
        dplyr::mutate('y_All LHDs' = round(fig_value_All_LHDs / sum(fig_value_All_LHDs) * 100, 1))

      # Add the calculated columns to your data
      data <- data %>%
        dplyr::mutate('y_All LHDs' = overall_totals$'y_All LHDs',
                      'Responses_All LHDs' = overall_totals$'Responses_All LHDs')%>%
        dplyr::rename(category = group)

    } else {
      # Process data as usual for other filters
      data <- return_survey_weighted_counts(input$topic, input$filter) %>%
        dplyr::select(category, group, y, subtitle, Responses) %>%
        tidyr::pivot_wider(names_from = group, values_from = c(y, Responses)) %>%
        dplyr::left_join(., add_all_counts_long(input$topic), by = "category") %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
        )
    }

  } #closes not All LHDs filter else


  return(data)
}


# Tables function--------

create_non_time_table <- function(data, category, colors, topic) {


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
    } else if (topic == "jobs_total") {
      scales::label_comma()(rounded_value)
    } else if (topic == "b") {
      paste0(rounded_value, " FTEs")
    } else if (topic == "fte_bypop") {
      paste0(rounded_value, " FTE per capita (mean)")
    } else if (topic == "c5q37") {
      paste0(rounded_value, " Total FTE (mean)")
    } else if (topic == "employee_bypop") {
      paste0(rounded_value, " Employees per capita (mean)")
    } else if (topic == "c5q36") {
      paste0(rounded_value, " Total Employees (mean)")
    } else {
      scales::percent_format(scale = 1)(rounded_value)  # Format as percentage with rounded value
    }
  }


  # Define theme----------
  options(
    reactable.theme = reactable::reactableTheme(
      color = "#3d4242",
      backgroundColor = "",
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
        fontSize = "12px",
        borderTop = "1px solid #212F5B",
        borderBottom = "2px solid #212F5B"
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
                                           ifelse(topic == "jobs_total", 15000,
                                                  ifelse(topic == "b", 21000,
                                                  ifelse(topic == "jobs_standard", 10, 100)))),
                       fill_color = naccho.viz::green,
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

 if (category == "All LHDs" && !(topic %in% c("jobs_standard", "jobs_total")))
   {

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
                                            ifelse(topic == "jobs_total", 15000,
                                                ifelse(topic == "b", 21000,
                                                      ifelse(topic == "jobs_standard", 10, 100)))),
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

      filterable = FALSE,
      showPageSizeOptions = TRUE,
      striped = FALSE,
      highlight = TRUE,
      searchable = FALSE,
      bordered = FALSE,
      resizable = TRUE
    )
  }

 # message("Column definitions:")
 # print(column_definitions)

 # message("the topic is:")
 # print(topic)
 #
 # message("These are the data:")
 # print(data)


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
