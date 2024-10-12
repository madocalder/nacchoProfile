#' Prepare Budget Changes Data with Filters
#'
#' This script prepares a dataset that tracks budget changes for Local Health Departments (LHDs).
#' The dataset records whether the current year's budget is less than or the same/higher than the previous year's budget,
#' and it is filtered by urban-rural classification, government category, population category, and census region.
#'
#' @details
#' The script performs the following steps:
#' - Recodes the `c10q301` variable to classify budget changes into "Less Than Previous Year's Budget" and "The Same or Higher Than Last Year's Budget".
#' - Applies filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generates survey-weighted counts using the `return_survey_weighted_counts` function.
#' - Combines filtered and unfiltered datasets.
#' - Saves the resulting dataset using `usethis::use_data()`.
#'
#' @param vars A character vector containing the budget-related variable (`budget_changes`).
#' @param filters A character vector specifying the filters to be applied (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the dataset `budget_data` as an `.rda` file in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save budget changes data
#' vars <- c("budget_changes")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

devtools::load_all()

start_time <- Sys.time() # get start time

# Recoding budget changes based on `c10q301`
master_data <- master_data %>%
  dplyr::mutate(budget_changes = dplyr::case_when(
    c10q301 == 1 ~ "Less Than Previous Year's Budget",
    c10q301 == 2 | c10q301 == 3 ~ "The Same or Higher Than Last Year's Budget",
    c10q301 == 4 ~ NA_character_
  ))

# Set variables and filters
vars <- c("budget_changes")
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
budget_list <- list() # create an empty list

# Loop to create dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]], wtype = "weights_static")

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    rename(filter = all_of(filters[i])) %>%  # Rename the filter column to a common name
    select(-14)  # Adjust to exclude column 14 if needed

  budget_list[[i]] <- data
}

# Bind the data generated in loop
budget_changes_with_filters <- do.call(rbind, budget_list)

# Create the non-filtered dataset
budget_changes_no_filters <- return_survey_weighted_counts("budget_changes", wtype = "weights_static") %>%
  dplyr::mutate(filter = "All LHDs")

# Remove the extra "budget_changes" column from budget_changes_no_filters
budget_changes_no_filters <- budget_changes_no_filters %>%
  dplyr::select(-budget_changes)

# Final dataset combining filtered and unfiltered data
budget_data <- rbind(budget_changes_with_filters, budget_changes_no_filters)

# Print elapsed time
end_time <- Sys.time() - start_time # calculate time difference
print(end_time)

# Save the budget_data dataset
usethis::use_data(budget_data, overwrite = TRUE)

# Document the changes
devtools::document()
