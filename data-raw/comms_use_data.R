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

vars <- c("newm4q501l", "newm4q501a", "newm4q501b", "newm4q501c", "newm4q501e", "newm4q501h",
          "newm4q501k", "newm4q501n", "newm4q501o", "newm4q501p", "newm4q501q", "newm4q501r",
          "newm4q501u", "newm4q501v", "newm4q501w",

          "newm4q502l", "newm4q502a", "newm4q502b", "newm4q502c", "newm4q502e", "newm4q502h",
          "newm4q502k", "newm4q502n", "newm4q502o", "newm4q502p", "newm4q502q", "newm4q502r",
          "newm4q502u", "newm4q502v", "newm4q502w",

          "m4q308l", "m4q308a", "m4q308b", "m4q308c", "m4q308e", "m4q308h",
          "m4q308k", "m4q308n", "m4q308o", "m4q308p", "m4q308q", "m4q308r",
          "m4q308u", "m4q308v", "m4q308w")

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
    Question %in% c("newm4q501l", "newm4q502l", "m4q308l") ~ "Automated phone calling",
    Question %in% c("newm4q501a", "newm4q502a","m4q308a") ~ "Blogs",
    Question %in% c("newm4q501b", "newm4q502b", "m4q308b") ~ "Facebook",
    Question %in% c("newm4q501c", "newm4q502c", "m4q308c") ~ "LinkedIn",
    Question %in% c("newm4q501e", "newm4q502e", "m4q308e") ~ "Twitter",
    Question %in% c("newm4q501h", "newm4q502h", "m4q308h") ~ "Other social media",
    Question %in% c("newm4q501k", "newm4q502k", "m4q308k") ~ "Text messaging",
    Question %in% c("newm4q501n", "newm4q502n", "m4q308n") ~ "Fax broadcast/fax blast",
    Question %in% c("newm4q501o", "newm4q502o", "m4q308o") ~ "Hotline/Call Center",
    Question %in% c("newm4q501p", "newm4q502p", "m4q308p") ~ "Broadcast media",
    Question %in% c("newm4q501q", "newm4q502q", "m4q308q") ~ "Print media",
    Question %in% c("newm4q501r", "newm4q502r", "m4q308r") ~ "Email",
    Question %in% c("newm4q501u", "newm4q502u", "m4q308u") ~ "LHD website",
    Question %in% c("newm4q501v", "newm4q502v", "m4q308v") ~ "Health alert network",
    Question %in% c("newm4q501w", "newm4q502w", "m4q308w") ~ "Custom app for phone/tablet"
  ),
  comms_use = dplyr::case_when(
    stringr::str_detect(Question, "newm4q501") ~ "General Announcements",
    stringr::str_detect(Question, "newm4q502") ~ "Emergency Response",
    stringr::str_detect(Question, "m4q308") ~ "Any Use"

  )
  )



# Save the comms_use_data dataset
usethis::use_data(comms_use_data, overwrite = TRUE)
