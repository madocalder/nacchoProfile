#' Prepare Advocacy and New Advocacy Datasets with Filters
#'
#' This script prepares datasets for advocacy activities by applying various filters (e.g., urban-rural classification,
#' government category, population category, census region) and generating survey-weighted counts for Local Health Departments (LHDs).
#' It creates filtered and unfiltered versions of each dataset and binds them together. The datasets are saved for further use.
#'
#' @details
#' This script performs the following steps for each dataset (advocacy, new advocacy):
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate survey-weighted counts using the `return_survey_weighted_counts` function.
#' - Combine filtered and unfiltered data.
#' - Add category labels for each advocacy type based on specific survey questions.
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param vars Character vector specifying the advocacy-related questions (e.g., "c12q260x" for advocacy, "newc12q502x" for new advocacy).
#' @param filters Character vector specifying the filters applied to each dataset (e.g., "urbanrural2022", "c0govcat").
#'
#' @return Saves the datasets (`advocacy_data`, `new_advocacy_data`) as `.rda` files in the package.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save advocacy data
#' vars <- c("c12q260x", "c12q260aa", "c12q260n", "c12q260o", "c12q260e", "c12q260y")
#' filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#'
#' # Prepare new advocacy data similarly
#'
#' @export
NULL

# Code to prepare `advocacy_data` dataset -------
devtools::load_all()

vars <- c("c12q260x", "c12q260aa",
          "c12q260n", "c12q260o",
          "c12q260e", "c12q260y",
          "c12q260ac", "c12q260w",
          "c12q260p", "c12q260f",
          "c12q260q", "c12q260h",
          "c12q260r", "c12q260s",
          "c12q260af", "c12q260ag",
          "c12q260t", "c12q260u",
          "c12q260v", "c12q260z",
          "c12q260j", "c12q260k"
)

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

advocacy_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
    mutate(filter = paste(filters[i]))

  advocacy_list[[i]] <- data
}

# Bind the data generated in loop
advocacy_with_filters <- do.call(rbind, advocacy_list)

# Create data with no filter
advocacy_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
advocacy_data <- rbind(advocacy_with_filters, advocacy_no_filter) %>%
  dplyr::mutate(advocacy_type = dplyr::case_when(
    Question %in% c("c12q260x") ~ "Climate Change",
    Question %in% c("c12q260aa") ~ "COVID-19 Emergency preparedness and response",
    Question %in% c("c12q260n") ~ "Non-COVID-19 Emergency Preparedness and response",
    Question %in% c("c12q260o") ~ "Food safety",
    Question %in% c("c12q260e") ~ "Funding for access to healthcare",
    Question %in% c("c12q260y") ~ "Funding for local public health",
    Question %in% c("c12q260ac") ~ "COVID-19 Infectious disease",
    Question %in% c("c12q260w") ~ "Non-COVID-19 Infectious disease",
    Question %in% c("c12q260p") ~ "Injury/violence prevention",
    Question %in% c("c12q260f") ~ "Land use",
    Question %in% c("c12q260q") ~ "Mental health",
    Question %in% c("c12q260h") ~ "Occupational health and safety",
    Question %in% c("c12q260r") ~ "Obesity/physical activity",
    Question %in% c("c12q260s") ~ "Oral health",
    Question %in% c("c12q260af") ~ "Reforms related to community policing",
    Question %in% c("c12q260ag") ~ "Planning external resource allocation using an equity lens",
    Question %in% c("c12q260t") ~ "Safe and healthy housing",
    Question %in% c("c12q260u") ~ "Tobacco, alcohol, and other drugs",
    Question %in% c("c12q260v") ~ "Wastewater/sanitation",
    Question %in% c("c12q260z") ~ "Other environmental health",
    Question %in% c("c12q260j") ~ "Other policy areas",
    Question %in% c("c12q260k") ~ "None"
  ))

# Save the advocacy_data
usethis::use_data(advocacy_data, overwrite = TRUE)

# Code to prepare `new_advocacy_data` dataset ----
devtools::load_all()

vars <- c("newc12q502x", "newc12q502aa",
          "newc12q502b", "newc12q502ab",
          "newc12q502o", "newc12q502i",
          "newc12q502y", "newc12q502ac",
          "newc12q502r", "newc12q502m",
          "newc12q502s", "newc12q502q",
          "newc12q502j", "newc12q502l",
          "newc12q502p", "newc12q502t",
          "newc12q502k", "newc12q502v",
          "newc12q502z", "newc12q502f"
)

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

new_advocacy_list <- list() # create an empty list

# Loop to create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  # Use pmap to iterate without combining yet
  data_list <- purrr::pmap(args, return_survey_weighted_counts)

  # Check each data frame's Answer column and convert it to a consistent type
  data_list <- lapply(data_list, function(data) {
    if ("Answer" %in% colnames(data)) {
      data <- data %>%
        mutate(Answer = as.character(Answer))  # Or use as.numeric, depending on the case
    }
    return(data)
  })

  # Now bind the rows after ensuring consistent types
  data <- dplyr::bind_rows(data_list) %>%
    select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
    mutate(filter = paste(filters[i]))

  # Store in the result list
  new_advocacy_list[[i]] <- data
}

# Bind the data generated in loop
new_advocacy_with_filters <- do.call(rbind, new_advocacy_list)

# Create data with no filter
new_advocacy_no_filter <- map_dfr(vars, function(var) {
  data <- return_survey_weighted_counts(var)  # Apply the function to get the data

  # Ensure that 'Answer' is consistently a character
  if ("Answer" %in% colnames(data)) {
    data <- data %>%
      mutate(Answer = as.character(Answer))  # Convert 'Answer' to character
  }

  return(data)
}) %>%
  select(1:12, all_of("Responses")) %>%  # Use all_of() for future-proofing
  mutate(filter = paste("All LHDs"))

# Join the two datasets -- with filters and without filters
new_advocacy_data <- rbind(new_advocacy_with_filters, new_advocacy_no_filter) %>%
  dplyr::mutate(new_advocacy_type = dplyr::case_when(
    Question %in% c("newc12q502x") ~ "Climate Change",
    Question %in% c("newc12q502aa") ~ "COVID-19 Emergency preparedness and response",
    Question %in% c("newc12q502b") ~ "Non-COVID-19 Emergency preparedness and response",
    Question %in% c("newc12q502ab") ~ "Declared racism a threat to public health or an emergency",
    Question %in% c("newc12q502o") ~ "Food safety",
    Question %in% c("newc12q502i") ~ "Funding for access to healthcare",
    Question %in% c("newc12q502y") ~ "Funding for local public health",
    Question %in% c("newc12q502ac") ~ "COVID-19 Infectious disease",
    Question %in% c("newc12q502r") ~ "Non-COVID-19 Infectious disease",
    Question %in% c("newc12q502m") ~ "Injury/violence prevention",
    Question %in% c("newc12q502s") ~ "Land use planning",
    Question %in% c("newc12q502q") ~ "Mental health",
    Question %in% c("newc12q502j") ~ "Occupational health and safety",
    Question %in% c("newc12q502l") ~ "Obesity/physical activity",
    Question %in% c("newc12q502p") ~ "Oral health",
    Question %in% c("newc12q502t") ~ "Safe and healthy housing",
    Question %in% c("newc12q502k") ~ "Tobacco, alcohol, and other drugs",
    Question %in% c("newc12q502v") ~ "Wastewater/sanitation",
    Question %in% c("newc12q502z") ~ "Other environmental health",
    Question %in% c("newc12q502f") ~ "Other area"
  ))


engagement_data <- tibble::tribble(
  ~filter,                  ~group,            ~y, ~Responses, ~category, ~new_advocacy_type, ~Question,
  "All LHDs",              "All LHDs",           33, 923, "Checked", "None", "newc12q502",
  "c0popcat3", "Small (<50,000)", 27, 923, "Checked", "None",  "newc12q502",
  "c0popcat3", "Medium (50,000~499,999)", 39, 923, "Checked", "None",  "newc12q502",
  "c0popcat3", "Large (500,000+)", 64, 923, "Checked", "None",  "newc12q502",
  "c0govcat",    "State",              19, 923, "Checked", "None",  "newc12q502",
  "c0govcat",    "Local",              36, 923, "Checked", "None",  "newc12q502",
  "c0govcat",    "Shared",             38, 923, "Checked", "None", "newc12q502",
  "censusregion",         "Northeast",          45, 923, "Checked", "None", "newc12q502",
  "censusregion",         "Midwest",            30, 923, "Checked", "None", "newc12q502",
  "censusregion",         "South",              24, 923, "Checked", "None", "newc12q502",
  "censusregion",         "West",               49, 923, "Checked", "None", "newc12q502",
  "urbanrural2022","Urban",              47, 923, "Checked", "None", "newc12q502",
  "urbanrural2022","Rural",              21, 923, "Checked", "None", "newc12q502"
)

# Calculate "No engagement" as 100 - Engagement
engagement_data <- engagement_data %>%
  dplyr::mutate(
    y = 100 - y
  )

new_advocacy_data <- new_advocacy_data %>%
  dplyr::bind_rows(new_advocacy_data, engagement_data)

new_advocacy_data <- new_advocacy_data %>%
dplyr::mutate(
  new_advocacy_type = factor(new_advocacy_type, levels = c("None", "Climate Change",
                                         "COVID-19 Emergency preparedness and response",
                                         "Non-COVID-19 Emergency preparedness and response",
                                         "Declared racism a threat to public health or an emergency",
                                         "Food safety",
                                         "Funding for access to healthcare",
                                         "Funding for local public health",
                                         "COVID-19 Infectious disease",
                                         "Non-COVID-19 Infectious disease",
                                         "Injury/violence prevention",
                                         "Land use planning",
                                         "Mental health",
                                         "Occupational health and safety",
                                         "Obesity/physical activity",
                                         "Oral health",
                                         "Safe and healthy housing",
                                         "Tobacco, alcohol, and other drugs",
                                         "Wastewater/sanitation",
                                         "Other environmental health",
                                         "Other area"))  # List all categories
) %>%
  # Arrange rows to reflect the factor order
  dplyr::arrange(new_advocacy_type)



# Save the new_advocacy_data
usethis::use_data(new_advocacy_data, overwrite = TRUE)

