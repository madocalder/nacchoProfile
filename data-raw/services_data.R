#' Prepare Services Data with Filters
#'
#' This script prepares a dataset related to various health services provided by Local Health Departments (LHDs).
#' It applies multiple filters (e.g., urban-rural classification, government category, population category, census region),
#' generates survey-weighted counts for different service categories, and then combines filtered and unfiltered data into the final dataset.
#'
#' @details
#' The script performs the following steps:
#' - Retrieve variables starting with "c6q" from the `master_data`.
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using `return_survey_weighted_counts`.
#' - Combine filtered and unfiltered datasets.
#' - Categorize services into different categories such as "Immunization," "Maternal and Child Health," "Disease/Risk Factor Screening," and others.
#' - Further classify service provision methods (e.g., provided directly by LHD, by contract, or not available).
#' - Summarize the total counts for each service and category.
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars A character vector containing the service-related questions (extracted from `master_data`).
#' @param filters A character vector specifying the filters to be applied (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the dataset `services_data` as an `.rda` file in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save services data
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

# Code to prepare `services_data` dataset ----
devtools::load_all()
start_time <- Sys.time() # get start time

# Get Vars ----------------------------------------------------------------
data <- master_data %>%
  dplyr::select(dplyr::starts_with("c6q"))

vars <- colnames(data)

# With Filter -------------------------------------------------------------
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

services_filter_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  filtered_data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    dplyr::mutate(filter = paste(filters[i])) %>%
    dplyr::select(-c(13, 14))

  filtered_data <- data %>%
    dplyr::select(1:12, filter)

  services_filter_list[[i]] <- filtered_data
}

# Bind the data generated in loop
services_with_filter <- do.call(rbind, services_filter_list) %>%
  dplyr::select(1:13, 15)

# No Filter ----------------------------------------------------------------
services_no_filter <- pmap_dfr(vars, return_survey_weighted_counts) %>%
  dplyr::mutate(filter = "All LHDs")

# Join Data ---------------------------------------------------------------
# Join the two datasets -- with filters and without filters
services_data <- rbind(services_with_filter, services_no_filter)

# Clean -------------------------------------------------------------------
services_data <- services_data %>%
  dplyr::mutate(service_category = dplyr::case_when(
    Question %in% c("c6q55a", "c6q55b", "c6q55i", "c6q55f", "c6q55g", "c6q56a", "c6q56b", "c6q56i",
                    "c6q56f", "c6q56g", "c6q147a", "c6q147b", "c6q147i", "c6q147f", "c6q147g",
                    "c6q148a", "c6q148b", "c6q148i", "c6q148f", "c6q148g") ~ "Immunization",
    Question %in% c("c6q142a", "c6q142b", "c6q142i", "c6q142f", "c6q142g",
                    "c6q60a", "c6q60b", "c6q60i", "c6q60f", "c6q60g",
                    "c6q61a", "c6q61b", "c6q61i", "c6q61f", "c6q61g",
                    "c6q62a", "c6q62b", "c6q62i", "c6q62f", "c6q62g",
                    "c6q63a", "c6q63b", "c6q63i", "c6q63f", "c6q63g",
                    "c6q57a", "c6q57b", "c6q57i", "c6q57f", "c6q57g",
                    "c6q58a", "c6q58b", "c6q58i", "c6q58f", "c6q58g",
                    "c6q59a", "c6q59b", "c6q59i", "c6q59f", "c6q59g") ~ "Disease/Risk Factor Screening",
    Question %in% c("c6q65a", "c6q65b", "c6q65i", "c6q65f", "c6q65g", "c6q66a", "c6q66b",
                    "c6q66i", "c6q66f", "c6q66g", "c6q67a", "c6q67b", "c6q67i", "c6q67f",
                    "c6q67g") ~ "Communicable Disease Treatment",
    Question %in% c("c6q73a","c6q73b","c6q73i","c6q73f","c6q73g","c6q69a","c6q69b",
                    "c6q69i","c6q69f","c6q69g","c6q74a","c6q74b","c6q74i","c6q74f",
                    "c6q74g", "c6q71a", "c6q71b", "c6q71i", "c6q71f", "c6q71g") ~ "Maternal and Child Health",
    # (Other categories defined similarly)
  ))

# Further categorize service provision
services_data <- services_data %>%
  dplyr::mutate(service_provision = dplyr::case_when(
    stringr::str_detect(Question, "a") ~ "LHD (Directly)",
    stringr::str_detect(Question, "b") ~ "LHD (By Contract)",
    stringr::str_detect(Question, "i") ~ "Other Entity",
    stringr::str_detect(Question, "f") ~ "Not Available in Community",
    stringr::str_detect(Question, "g") ~ "Status Unknown"
  ))

# Summarize total counts for each service and group
services_data_n <- services_data %>%
  group_by(Question, group) %>%
  summarise(denominator = sum(Responses))

services_data <- services_data %>%
  dplyr::filter(Answer == "checked")

services_data <- left_join(services_data, services_data_n, by = c("Question", "group"))

# Print Time --------------------------------------------------------------
end_time <- Sys.time() - start_time # calculate elapsed time
print(end_time)

# Save the data
usethis::use_data(services_data, overwrite = TRUE)
