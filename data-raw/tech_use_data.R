#' Prepare Technology Use Data with Filters
#'
#' This script prepares a dataset related to the use of different technology systems in Local Health Departments (LHDs).
#' It applies various filters (e.g., urban-rural classification, government category, population category, census region),
#' generates survey-weighted counts, and combines both filtered and unfiltered data into a final dataset.
#'
#' @details
#' This script performs the following steps:
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using the `return_survey_weighted_counts` function.
#' - Combine filtered and unfiltered data.
#' - Assign descriptive labels to the different types of technologies used by LHDs based on specific survey questions.
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars Character vector specifying the technology-related questions (e.g., "m4q300", "m4q401").
#' @param filters Character vector specifying the filters applied to the data (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the dataset `tech_use_data` as an `.rda` file in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save technology use data
#' vars <- c("m4q300", "m4q401", "m4q301", "m4q402", "m4q302", "m4q304")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

# Code to prepare `tech_use_data` dataset ----
devtools::load_all()

vars <- c("m4q300", "m4q401", "m4q301", "m4q402", "m4q302", "m4q304")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

tech_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  tech_list[[i]] <- data
}

# Bind the data generated in loop
tech_use_with_filters <- do.call(rbind, tech_list)

vars <- list("m4q300", "m4q401", "m4q301", "m4q402", "m4q302", "m4q304")

# Create data with no filter
tech_use_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
tech_use_data <- rbind(tech_use_with_filters, tech_use_no_filter) %>%
  dplyr::mutate(tech_type = dplyr::case_when(
    Question == "m4q300" ~ "Electronic Case Reporting (eCR)",
    Question == "m4q401" ~ "Electronic Disease Reporting System",
    Question == "m4q301" ~ "Electronic Health Records",
    Question == "m4q402" ~ "Electronic Lab Reporting",
    Question == "m4q302" ~ "Health Information Exchange",
    Question == "m4q304" ~ "Immunization Registry"
  ))

# Save the tech_use_data
usethis::use_data(tech_use_data, overwrite = TRUE)
