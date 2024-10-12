#' Prepare Revenue Percentages Data
#'
#' This script prepares datasets related to revenue sources for Local Health Departments (LHDs) and calculates
#' revenue percentages based on different revenue categories. The dataset includes overall revenue percentages and
#' revenue percentages by specific filters such as urban-rural classification, government category, population category,
#' and census region.
#'
#' @details
#' The script performs the following steps:
#' - Calculate the total revenue and responses for each revenue source (e.g., local, state, federal, Medicare, etc.).
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion" to calculate revenue percentages by each group.
#' - Calculate revenue percentages both overall and by filter.
#' - Categorize revenue sources into descriptive types (e.g., "Local", "State", "Federal Direct", "Private Foundation", etc.).
#' - Save the resulting datasets using `usethis::use_data()`.
#'
#' @param args A character vector containing the revenue-related questions (e.g., "c3q17p", "c3q17c", "c3q17d").
#' @param filters A character vector specifying the filters to be applied (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves two datasets:
#' - `revenue_percentages`: overall revenue percentages for each revenue type.
#' - `revenue_percentages_filter`: revenue percentages for each revenue type by different filters.
#'
#' @seealso [return_survey_weighted_statistics()] for the function used to generate the weighted statistics.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save revenue percentages data
#' args <- c("c3q17p", "c3q17c", "c3q17d", "c3q17e", "c3q17r", "c3q17i", "c3q17j", "c3q17v", "c3q17h", "c3q17m")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

# Code to prepare `revenue_percentages` dataset ----
devtools::load_all()

args <- c("c3q17p", "c3q17c", "c3q17d", "c3q17e", "c3q17r", "c3q17i", "c3q17j", "c3q17v", "c3q17h", "c3q17m")

# Loop through each argument in 'args' to calculate revenue percentages
revenue_percentages <- do.call(rbind, lapply(args, function(i) {
  data <- return_survey_weighted_statistics(i, wtype = "weights_static")

  data.frame(
    revenue_total = sum(data$Total, na.rm = TRUE),
    Responses = sum(data$Responses, na.rm = TRUE),
    group = data$group,
    subtitle = data$subtitle,
    revenue_type = i
  )
}))

# Categorize the revenue sources
revenue_percentages <- revenue_percentages %>%
  dplyr::mutate(revenue_type = dplyr::case_when(
    revenue_type == "c3q17p" ~ "Local",
    revenue_type == "c3q17c" ~ "State",
    revenue_type == "c3q17d" ~ "Federal Pass Through",
    revenue_type == "c3q17e" ~ "Federal Direct",
    revenue_type == "c3q17r" ~ "Medicare or Medicaid",
    revenue_type == "c3q17i" ~ "Private Health Insurance",
    revenue_type == "c3q17j" ~ "Patient Fees",
    revenue_type == "c3q17v" ~ "Non-Clinical Fees/Fines",
    revenue_type == "c3q17h" ~ "Private Foundation",
    revenue_type == "c3q17m" ~ "Other"
  )) %>%
  dplyr::mutate(percentage = revenue_total / sum(revenue_total))

# Save the revenue_percentages dataset
usethis::use_data(revenue_percentages, overwrite = TRUE)

# Code to prepare `revenue_percentages_filter` dataset ----
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

revenue_by_filter_percentages <- list()

# Loop through each filter to calculate revenue percentages by filter
for (i in 1:length(filters)) {
  args_filters <- list(args, filters[[i]], wtype = "weights_static")
  data <- pmap_dfr(args_filters, return_survey_weighted_statistics) %>%
    dplyr::mutate(filter = paste(filters[i]))

  revenue_by_filter_percentages[[i]] <- data
}

# Bind the data generated in loop and wrangle
revenue_percentages_filter <- do.call(rbind, revenue_by_filter_percentages) %>%
  dplyr::select(Question, Group, N, Responses, Total, x, category, subtitle, filter) %>%
  dplyr::group_by(Group) %>%
  dplyr::mutate(overall_total = sum(Total)) %>%
  dplyr::mutate(revenue_type = dplyr::case_when(
    Question == "c3q17p" ~ "Local",
    Question == "c3q17c" ~ "State",
    Question == "c3q17d" ~ "Federal Pass Through",
    Question == "c3q17e" ~ "Federal Direct",
    Question == "c3q17r" ~ "Medicare or Medicaid",
    Question == "c3q17i" ~ "Private Health Insurance",
    Question == "c3q17j" ~ "Patient Fees",
    Question == "c3q17v" ~ "Non-Clinical Fees/Fines",
    Question == "c3q17h" ~ "Private Foundation",
    Question == "c3q17m" ~ "Other"
  )) %>%
  dplyr::mutate(percentage = Total / overall_total) %>%
  ungroup() %>%
  dplyr::select(revenue_type, percentage, x, category, subtitle, filter, Responses)

# Save the revenue_percentages_filter dataset
usethis::use_data(revenue_percentages_filter, overwrite = TRUE)
