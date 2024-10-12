#' Prepare CHIP Activities Data with Filters
#'
#' This script prepares a dataset related to Community Health Improvement Plan (CHIP) activities carried out by Local Health Departments (LHDs).
#' The dataset includes various actions taken as part of CHIP implementation and is filtered by urban-rural classification, government category, population category, and census region.
#'
#' @details
#' The script performs the following steps:
#' - Retrieve variables related to CHIP activities (e.g., addressing differential access, developing relationships with community partners).
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using the `return_survey_weighted_counts` function.
#' - Combine filtered and unfiltered datasets.
#' - Categorize the actions taken as part of CHIP activities, such as "Addressed differential access", "Developed performance measures", or "No action".
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars A character vector containing the CHIP-related questions (e.g., "m9q240q", "m9q240r").
#' @param filters A character vector specifying the filters to be applied (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the dataset `chip_data` as an `.rda` file in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save CHIP activities data
#' vars <- c("m9q240q", "m9q240r", "m9q240s", "m9q240i", "m9q240k", "m9q240b")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

# Code to prepare `chip_data` dataset ----
devtools::load_all()

vars <- c("m9q240q", "m9q240r", "m9q240s",
          "m9q240i", "m9q240k", "m9q240b",
          "m9q240g", "m9q240l", "m9q240h",
          "m9q240p", "m9q240d", "m9q240c",
          "m9q240o", "m9q240m", "m9q240n")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

chip_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  # Use pmap to iterate without combining yet
  data_list <- purrr::pmap(args, return_survey_weighted_counts)

  # Ensure consistency in the 'Answer' column
  data_list <- lapply(data_list, function(data) {
    if ("Answer" %in% colnames(data)) {
      data <- data %>%
        mutate(Answer = as.character(Answer))  # Or use as.numeric, depending on the case
    }
    return(data)
  })

  # Bind the rows after ensuring consistent types
  data <- dplyr::bind_rows(data_list) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  # Store in the result list
  chip_list[[i]] <- data
}

# Bind the data generated in loop
chip_with_filters <- do.call(rbind, chip_list)

vars <- c("m9q240q", "m9q240r", "m9q240s",
          "m9q240i", "m9q240k", "m9q240b",
          "m9q240g", "m9q240l", "m9q240h",
          "m9q240p", "m9q240d", "m9q240c",
          "m9q240o", "m9q240m", "m9q240n")

# Create data with no filter
chip_no_filter <- map_dfr(vars, function(var) {
  data <- return_survey_weighted_counts(var)  # Apply the function to get the data

  # Ensure consistency in the 'Answer' column
  if ("Answer" %in% colnames(data)) {
    data <- data %>%
      mutate(Answer = as.character(Answer))
  }

  return(data)
}) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
chip_data <- rbind(chip_with_filters, chip_no_filter) %>%
  dplyr::mutate(action = dplyr::case_when(
    Question %in% c("m9q240q") ~ "Addressed differential access for specific communities or population groups",
    Question %in% c("m9q240r") ~ "Addressed differential health outcomes for specific communities or population groups",
    Question %in% c("m9q240s") ~ "Addressed ineffective or unjust community systems",
    Question %in% c("m9q240i") ~ "Developed/strengthened relationships with community partners",
    Question %in% c("m9q240k") ~ "Developed performance measures to measure plan implementation",
    Question %in% c("m9q240b") ~ "Established/reaffirmed priorities for LHD",
    Question %in% c("m9q240g") ~ "Increased LHD funding for 1+ priorities",
    Question %in% c("m9q240l") ~ "Measured progress to HP 2020 goals",
    Question %in% c("m9q240h") ~ "Participated in coalition(s) to address 1+ priorities",
    Question %in% c("m9q240p") ~ "Reported on plan implementation progress",
    Question %in% c("m9q240d") ~ "Set/revisited goals for community health outcomes",
    Question %in% c("m9q240c") ~ "Set/revisited goals for LHD performance",
    Question %in% c("m9q240o") ~ "Worked with community partners to advance related policy changes",
    Question %in% c("m9q240m") ~ "Other action",
    Question %in% c("m9q240n") ~ "No action"
  ))

# Save the chip_data dataset
usethis::use_data(chip_data, overwrite = TRUE)
