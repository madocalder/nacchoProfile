#' Prepare Medical, Dental, and Behavioral Data with Filters
#'
#' This script prepares datasets for medical, dental, and behavioral services by applying
#' various filters (e.g., urban-rural classification, government category, population category,
#' census region) and generating survey-weighted counts for Local Health Departments (LHDs).
#' It creates filtered and unfiltered versions of each dataset and binds them together.
#' The datasets are saved for further use.
#'
#' @details
#' This script performs the following steps for each dataset (medical, dental, behavioral):
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using `return_survey_weighted_counts` function.
#' - Combine filtered and unfiltered data.
#' - Add category labels such as "Services Provided" or "Services Not Provided".
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars Character vector specifying the type of service, such as "medical", "dental", "behavioral".
#' @param filters Character vector specifying the filters applied to each dataset.
#'
#' @return Saves the datasets (`med_data`, `dental_data`, `behav_data`) as `.rda` files in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save medical data
#' vars <- c("medical")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#' # (Similar process for dental and behavioral data)
#'
#' @export
NULL

# Medical services data ------
vars <- c("m14q301a",
  "m14q302a",
  "m14q303a",
  "m14q304a",
  "m14q401a"
  )

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

medical_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
    dplyr::mutate(filter = paste(filters[i]))

  medical_list[[i]] <- data
}
# Bind the data generated in loop
medical_with_filters <- do.call(rbind, medical_list)

# View the data
# View(medical_with_filters)

# Create data with no filter
med_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
med_data <- rbind(medical_with_filters, med_no_filter) %>%
  mutate(category = case_when(
    Question == "m14q301a" ~ "Assessed the gaps in access to services",

    Question == "m14q302a" ~ "Addressed gaps through direct provision of clinical services",

    Question == "m14q303a" ~ "Implemented strategies to increase accessibility of existing services",

    Question == "m14q304a" ~ "Implemented strategies to target healthcare needs of underserved populations",

    Question == "m14q401a" ~ "Evaluated strategies to target healthcare needs of underserved populations",

  ))

med_data_n <- med_data %>%
  dplyr::group_by(category, group)%>%
  dplyr::reframe(category = unique(category),
                 Responses = sum(Responses))

med_data <- med_data %>%
  dplyr::select(-Responses) %>%
  dplyr::left_join(med_data_n, by = c("category", "group"))%>%
  dplyr::filter(Answer == "checked")
#
# View(med_data)

# Save the med_data
usethis::use_data(med_data, overwrite = TRUE)

# Dental services ------
vars <- c("m14q301b",
          "m14q302b",
          "m14q303b",
          "m14q304b",
          "m14q401b")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

dental_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
    dplyr::mutate(filter = paste(filters[i]))

  dental_list[[i]] <- data
}

# Bind the data generated in loop
dental_with_filters <- do.call(rbind, dental_list)

# View the data
# View(dental_with_filters)

# Create data with no filter
dental_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
dental_data <- rbind(dental_with_filters, dental_no_filter) %>%
  mutate(category = case_when(
     Question == "m14q301b" ~ "Assessed the gaps in access to services",

   Question == "m14q302b" ~ "Addressed gaps through direct provision of clinical services",

    Question == "m14q303b" ~ "Implemented strategies to increase accessibility of existing services",

   Question == "m14q304b" ~ "Implemented strategies to target healthcare needs of underserved populations",

    Question == "m14q401b" ~ "Evaluated strategies to target healthcare needs of underserved populations",

  ))

dental_data_n <- dental_data %>%
  dplyr::group_by(category, group)%>%
  dplyr::reframe(category = unique(category),
                 Responses = sum(Responses))

dental_data <- dental_data %>%
  dplyr::select(-Responses) %>%
  dplyr::left_join(dental_data_n, by = c("category", "group"))%>%
  dplyr::filter(Answer == "checked")

# View and save the dental_data
View(dental_data)
usethis::use_data(dental_data, overwrite = TRUE)

# Behavioral services ------
vars <- c("m14q301c",
          "m14q302c",
          "m14q303c",
          "m14q304c",
          "m14q401c")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

behav_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
    dplyr::mutate(filter = paste(filters[i]))

  behav_list[[i]] <- data
}

# Bind the data generated in loop
behav_with_filters <- do.call(rbind, behav_list)

# # View the data
# View(behav_with_filters)

# Create data with no filter
behav_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
behav_data <- rbind(behav_with_filters, behav_no_filter) %>%
  mutate(category = case_when(
    Question == "m14q301c" ~ "Assessed the gaps in access to services",

    Question == "m14q302c" ~ "Addressed gaps through direct provision of clinical services",

    Question == "m14q303c" ~ "Implemented strategies to increase accessibility of existing services",

    Question == "m14q304c" ~ "Implemented strategies to target healthcare needs of underserved populations",

    Question == "m14q401c" ~ "Evaluated strategies to target healthcare needs of underserved populations",

  ))

behav_data_n <- behav_data %>%
  dplyr::group_by(category, group)%>%
  dplyr::reframe(category = unique(category),
                 Responses = sum(Responses))

behav_data <- behav_data %>%
  dplyr::select(-Responses) %>%
  dplyr::left_join(behav_data_n, by = c("category", "group"))%>%
  dplyr::filter(Answer == "checked")

# # View and save the behav_data
# View(behav_data)
usethis::use_data(behav_data, overwrite = TRUE)
