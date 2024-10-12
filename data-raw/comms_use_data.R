#' Prepare Communication Use Data with Filters
#'
#' This script prepares a dataset related to the communication channels used by Local Health Departments (LHDs).
#' It applies various filters (e.g., urban-rural classification, government category, population category, census region),
#' generates survey-weighted counts for different communication methods, and combines filtered and unfiltered data into the final dataset.
#'
#' @details
#' The script performs the following steps:
#' - Retrieve variables related to communication use (e.g., social media platforms, email, phone apps).
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using the `return_survey_weighted_counts` function.
#' - Combine filtered and unfiltered datasets.
#' - Categorize communication types into descriptive labels (e.g., "Facebook", "Email", "Broadcast media").
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars A character vector containing the communication-related questions (e.g., "m4q501l", "m4q501a", "m4q501b").
#' @param filters A character vector specifying the filters to be applied (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the dataset `comms_use_data` as an `.rda` file in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save communication use data
#' vars <- c("m4q501l", "m4q501a", "m4q501b", "m4q501c", "m4q501e", "m4q501h",
#'           "m4q501k", "m4q501n", "m4q501o", "m4q501p", "m4q501q", "m4q501r",
#'           "m4q501u", "m4q501v", "m4q501w")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

# Code to prepare `comms_use_data` dataset ----
devtools::load_all()

vars <- c("m4q501l", "m4q501a", "m4q501b", "m4q501c", "m4q501e", "m4q501h",
          "m4q501k", "m4q501n", "m4q501o", "m4q501p", "m4q501q", "m4q501r",
          "m4q501u", "m4q501v", "m4q501w")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

comms_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  comms_list[[i]] <- data
}

# Bind the data generated in loop
comms_use_with_filters <- do.call(rbind, comms_list)

# Create data with no filter
comms_use_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
comms_use_data <- rbind(comms_use_with_filters, comms_use_no_filter) %>%
  dplyr::mutate(comms_type = dplyr::case_when(
    Question %in% c("m4q501l") ~ "Automated phone calling",
    Question %in% c("m4q501a") ~ "Blogs",
    Question %in% c("m4q501b") ~ "Facebook",
    Question %in% c("m4q501c") ~ "LinkedIn",
    Question %in% c("m4q501e") ~ "Twitter",
    Question %in% c("m4q501h") ~ "Other social media",
    Question %in% c("m4q501k") ~ "Text messaging",
    Question %in% c("m4q501n") ~ "Fax broadcast/fax blast",
    Question %in% c("m4q501o") ~ "Hotline/Call Center",
    Question %in% c("m4q501p") ~ "Broadcast media",
    Question %in% c("m4q501q") ~ "Print media",
    Question %in% c("m4q501r") ~ "Email",
    Question %in% c("m4q501u") ~ "LHD website",
    Question %in% c("m4q501v") ~ "Health alert network",
    Question %in% c("m4q501w") ~ "Custom app for phone/tablet"
  ))

# Save the comms_use_data dataset
usethis::use_data(comms_use_data, overwrite = TRUE)
