#' Prepare Services Change Data with Filters
#'
#' This script prepares a dataset that tracks changes in the provision of various health services
#' by Local Health Departments (LHDs). It applies multiple filters (e.g., urban-rural classification,
#' government category, population category, census region), generates survey-weighted counts,
#' and categorizes service changes such as reduced, expanded, or no change in service.
#'
#' @details
#' The script performs the following steps:
#' - Retrieve variables starting with "c10q" and ending with "f" from the `master_data`.
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using the `return_survey_weighted_counts` function.
#' - Combine filtered and unfiltered datasets.
#' - Categorize services into different types (e.g., Blood Lead Screening, Diabetes Screening, Environmental Health).
#' - Categorize service provision changes (e.g., Reduced Service, Expanded Service, No Change).
#' - Calculate percentage changes for each service type.
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars A character vector containing the service-related questions (extracted from `master_data`).
#' @param filters A character vector specifying the filters to be applied (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the dataset `services_change_data` as an `.rda` file in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save services change data
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' @export
NULL

# Code to prepare `services_change_data` dataset ----
devtools::load_all()
start_time <- Sys.time() # get start time

# Get Vars ----------------------------------------------------------------
data <- master_data %>%
  dplyr::select(dplyr::starts_with("c10q") & dplyr::ends_with("f"))

vars <- colnames(data)

print(vars)


# With Filter -------------------------------------------------------------
# Test `pmap_dfr` with debugging
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

services_change_filter_list <- list() # Create an empty list

for (i in seq_along(filters)) {
  cat("Processing filter:", filters[i], "\n")

  args <- tibble(
    q_label = vars,
    grps = filters[i]
  )

  # Debugging `pmap_dfr`
  filtered_data <- purrr::pmap_dfr(
    .l = args,
    .f = function(q_label, grps) {
      cat("Processing q_label:", q_label, "with grps:", grps, "\n")

      # Call the function and handle edge cases
      result <- return_survey_weighted_counts(q_label = q_label, grps = grps)
      if (is.null(result) || nrow(result) == 0) {
        warning(paste("No data returned for q_label =", q_label, "and grps =", grps))
        return(tibble())  # Return an empty tibble
      }

      return(result %>% dplyr::mutate(filter = grps))
    }
  )

  # Append to the list
  services_change_filter_list[[i]] <- filtered_data
}


# Define the standard set of columns
standard_columns <- c(
  "Question", "Group", "Answer", "NA.RM", "N", "SE", "Percentage", "x", "y",
  "category", "group", "subtitle", "x_label", "Responses", "filter"
)

# Standardize column names for each data frame in the list
for (i in seq_along(services_change_filter_list)) {
  services_change_filter_list[[i]] <- services_change_filter_list[[i]] %>%
    dplyr::select(any_of(standard_columns))  # Select only the standard columns
}

for (i in seq_along(services_change_filter_list)) {
  services_change_filter_list[[i]] <- services_change_filter_list[[i]] %>%
    dplyr::mutate(across(where(is.factor), as.character))
}

# Remove any "names" attribute from the `group` column
for (i in seq_along(services_change_filter_list)) {
  if ("group" %in% colnames(services_change_filter_list[[i]])) {
    attr(services_change_filter_list[[i]]$group, "names") <- NULL
  }
}


# Combine all the data frames into one
services_change_with_filter <- dplyr::bind_rows(services_change_filter_list)

print(head(services_change_with_filter))


# Bind the data generated in loop
services_change_with_filter <- do.call(rbind, services_change_filter_list)

# No Filter ---------------------------------------------------------------
services_change_no_filter <- purrr::map_dfr(
  vars,
  function(q_label) {
    cat("Processing without filters for q_label:", q_label, "\n")

    result <- return_survey_weighted_counts(q_label = q_label)  # Call without filters
    if (is.null(result) || nrow(result) == 0) {
      warning(paste("No data returned for q_label =", q_label, "without filters"))
      return(tibble())  # Return an empty tibble if no data
    }

    result %>%
      dplyr::mutate(filter = "All LHDs") %>%  # Add a uniform filter column for unfiltered data
      dplyr::select(-filter) %>%  # Ensure consistent structure (filter will be added back below)
      dplyr::mutate(filter = "All LHDs")
  }
)


# Join Data ---------------------------------------------------------------
# Combine filtered and unfiltered data
services_change_data <- dplyr::bind_rows(services_change_with_filter, services_change_no_filter)

print(head(services_change_data))

# Categorize services
services_change_data <- services_change_data %>%
  dplyr::mutate(service_type = dplyr::case_when(
    Question == "c10q415f" ~ "Blood Lead Screening",
    Question == "c10q422f" ~ "Chronic Disease",
    Question == "c10q406f" ~ "Communicable Disease Screening or Treatment",
    Question == "c10q417f" ~ "Diabetes Screening",
    Question == "c10q411f" ~ "Emergency Preparedness",
    Question == "c10q420f" ~ "Environmental Health Including Food Safety",
    Question == "c10q405f" ~ "Epidemiology and Surveillance",
    Question == "c10q416f" ~ "High Blood Pressure Screening",
    Question == "c10q404f" ~ "Immunization",
    Question == "c10q408f" ~ "Maternal and Child Health Services",
    Question == "c10q421f" ~ "Mental Health",
    Question == "c10q418f" ~ "Obesity Prevention",
    Question == "c10q419f" ~ "Substance Use Prevention"
  ))




# Categorize service provision changes
services_change_data <- services_change_data %>%
  dplyr::mutate(service_provision = dplyr::case_when(
    Answer == "N/A-did not provide service" ~ "Service Not Provided",
    Answer == "reduced services" ~ "Reduced Service",
    Answer == "little/no change in services" ~ "No Change",
    Answer == "expanded services" ~ "Expanded Service",
    Answer == "don't know" ~ "Unknown"
  )) %>%
  dplyr::filter(service_provision %in% c("Reduced Service", "No Change", "Expanded Service"))

# Recalculate percentages excluding Don't Know and N/A --------------------

services_change_data <- services_change_data %>%
  dplyr::group_by(Question, group, filter) %>%
  dplyr::mutate(
    denominator = sum(N, na.rm = TRUE),
    Percentage = (N / denominator) * 100,
    y = Percentage
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Percentage = round_up(Percentage),
    y = round_up(y)
  )

# # Summarize total counts for each service and group
# services_change_data_n <- services_change_data %>%
#   group_by(Question, group) %>%
#   summarise(denominator = sum(Responses))
#
# services_change_data <- left_join(services_change_data, services_change_data_n, by = c("Question", "group"))
#
# # Calculate percentage changes for each service type
# services_change_data <- services_change_data %>%
#   group_by(Question, group) %>%
#   dplyr::mutate(new_y = (N / sum(N) * 100)) %>%
#   ungroup()
#
# # Rename columns
# services_change_data <- services_change_data %>%
#   dplyr::rename(old_y = y, y = new_y)
#
# # Round percentages
# services_change_data <- services_change_data %>%
#   dplyr::mutate(y = round_up(y))

# Save the data
usethis::use_data(services_change_data, overwrite = TRUE)
