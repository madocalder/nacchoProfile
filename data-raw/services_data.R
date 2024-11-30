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
start_time <- Sys.time()  # Get start time


case_when_text <- "
stringr::str_detect(Question, \"c6q55\") ~ \"Routine Adult Immunizations\",
stringr::str_detect(Question, \"c6q56\") ~ \"Routine Childhood Immunizations\",
stringr::str_detect(Question, \"c6q147\") ~ \"COVID-19 Adult Immunizations\",
stringr::str_detect(Question, \"c6q148\") ~ \"COVID-19 Child Immunizations\",
stringr::str_detect(Question, \"c6q142\") ~ \"BMI Screening\",
stringr::str_detect(Question, \"c6q60\") ~ \"Cancer Screening\",
stringr::str_detect(Question, \"c6q61\") ~ \"Cardiovascular Disease Screening\",
stringr::str_detect(Question, \"c6q62\") ~ \"Diabetes Screening\",
stringr::str_detect(Question, \"c6q63\") ~ \"High Blood Pressure Screening\",
stringr::str_detect(Question, \"c6q57\") ~ \"HIV/AIDs Screening\",
stringr::str_detect(Question, \"c6q58\") ~ \"Other STDs Screening\",
stringr::str_detect(Question, \"c6q59\") ~ \"Tuberculosis Screening\",
stringr::str_detect(Question, \"c6q65\") ~ \"HIV/AIDs\",
stringr::str_detect(Question, \"c6q66\") ~ \"Other STDs\",
stringr::str_detect(Question, \"c6q67\") ~ \"Tuberculosis\",
stringr::str_detect(Question, \"c6q73\") ~ \"EPSDT\",
stringr::str_detect(Question, \"c6q69\") ~ \"Prenatal Care\",
stringr::str_detect(Question, \"c6q74\") ~ \"Well-Child Clinic\",
stringr::str_detect(Question, \"c6q71\") ~ \"WIC\",
stringr::str_detect(Question, \"c6q78\") ~ \"Behavioral/Mental Health Services\",
stringr::str_detect(Question, \"c6q75\") ~ \"Comprehensive Primary Care\",
stringr::str_detect(Question, \"c6q76\") ~ \"Home Health Care\",
stringr::str_detect(Question, \"c6q77\") ~ \"Oral Health\",
stringr::str_detect(Question, \"c6q79\") ~ \"Substance Abuse Services\",
stringr::str_detect(Question, \"c6q83\") ~ \"Behavioral Risk Factors\",
stringr::str_detect(Question, \"c6q81\") ~ \"Chronic Disease\",
stringr::str_detect(Question, \"c6q80\") ~ \"Communicable Infectious Disease\",
stringr::str_detect(Question, \"c6q82\") ~ \"Injury\",
stringr::str_detect(Question, \"c6q84\") ~ \"Environmental Health\",
stringr::str_detect(Question, \"c6q86\") ~ \"Maternal and Child Health\",
stringr::str_detect(Question, \"c6q85\") ~ \"Syndromic Surveillance\",
stringr::str_detect(Question, \"c6q89\") ~ \"Chronic Disease Programs\",
stringr::str_detect(Question, \"c6q87\") ~ \"Injury\",
stringr::str_detect(Question, \"c6q95\") ~ \"Mental Illness\",
stringr::str_detect(Question, \"c6q90a\") ~ \"Nutrition\",
stringr::str_detect(Question, \"c6q145\") ~ \"Opioids\",
stringr::str_detect(Question, \"c6q91\") ~ \"Physical Activity\",
stringr::str_detect(Question, \"c6q146\") ~ \"Substance Abuse Other Than Opioids\",
stringr::str_detect(Question, \"c6q93\") ~ \"Tobacco\",
stringr::str_detect(Question, \"c6q105\") ~ \"Body Art (Tattoos, Piercings)\",
stringr::str_detect(Question, \"c6q97\") ~ \"Campgrounds and RVs\",
stringr::str_detect(Question, \"c6q103\") ~ \"Children's Camps\",
stringr::str_detect(Question, \"c6q110\") ~ \"Food Processing\",
stringr::str_detect(Question, \"c6q114\") ~ \"Food Service Establishments\",
stringr::str_detect(Question, \"c6q115\") ~ \"Health-Related Facilities\",
stringr::str_detect(Question, \"c6q101\") ~ \"Hotels and Motels\",
stringr::str_detect(Question, \"c6q116\") ~ \"Housing\",
stringr::str_detect(Question, \"c6q109\") ~ \"Lead Inspections\",
stringr::str_detect(Question, \"c6q111\") ~ \"Milk Processing\",
stringr::str_detect(Question, \"c6q113\") ~ \"Private Drinking Water\",
stringr::str_detect(Question, \"c6q112\") ~ \"Public Drinking Water\",
stringr::str_detect(Question, \"c6q143\") ~ \"Recreational Water (Pools, Lakes)\",
stringr::str_detect(Question, \"c6q102\") ~ \"Schools or Daycares\",
stringr::str_detect(Question, \"c6q100\") ~ \"Septic Systems\",
stringr::str_detect(Question, \"c6q107\") ~ \"Tobacco Retailers\",
stringr::str_detect(Question, \"c6q127\") ~ \"Air Pollution\",
stringr::str_detect(Question, \"c6q118\") ~ \"Food Safety Education\",
stringr::str_detect(Question, \"c6q124\") ~ \"Hazmat Response\",
stringr::str_detect(Question, \"c6q117\") ~ \"Indoor Air Quality\",
stringr::str_detect(Question, \"c6q121\") ~ \"Land Use Planning\",
stringr::str_detect(Question, \"c6q128\") ~ \"Noise Pollution\",
stringr::str_detect(Question, \"c6q144\") ~ \"Public Health Nuisance Abatement\",
stringr::str_detect(Question, \"c6q119\") ~ \"Radiation Control\",
stringr::str_detect(Question, \"c6q120\") ~ \"Vector Control\",
stringr::str_detect(Question, \"c6q131\") ~ \"Animal Control\",
stringr::str_detect(Question, \"c6q130\") ~ \"Emergency Medical Services\",
stringr::str_detect(Question, \"c6q134\") ~ \"Laboratory Services\",
stringr::str_detect(Question, \"c6q137\") ~ \"School Health\",
stringr::str_detect(Question, \"c6q136\") ~ \"School-Based Clinics\"
"

# Assuming `case_when_text` contains the full case_when block as a string
case_when_lines <- stringr::str_split(case_when_text, "\n")[[1]]  # Split into individual lines

# Extract the mappings for base_variable and service_type
case_when_mappings <- tibble(
  base_variable = stringr::str_extract(case_when_lines, "c6q\\d+"),  # Extract base variable (e.g., "c6q55")
  service_type = stringr::str_match(case_when_lines, "~\\s*\"(.*?)\"")[,2]  # Extract the text inside quotes after '~'
) %>%
  filter(!is.na(base_variable) & !is.na(service_type))  # Remove rows with NA values

# View(case_when_mappings)

#Get Relevant Columns from master_data
relevant_columns <- unique(case_when_mappings$base_variable)
data <- master_data %>%
  select(matches(paste0("^(", paste(relevant_columns, collapse = "|"), ")[a-z]?$")))

vars <- colnames(data)


# Process Data with Filters --------------------------------------
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
services_filter_list <- list()

for (filter in filters) {
  cat("Processing filter:", filter, "\n")

  args <- tibble(
    q_label = vars,
    grps = filter
  )

  filtered_data <- purrr::pmap_dfr(
    .l = args,
    .f = function(q_label, grps) {
      cat("Processing q_label:", q_label, "with grps:", grps, "\n")
      result <- tryCatch(
        return_survey_weighted_counts(q_label = q_label, grps = grps),
        error = function(e) {
          warning(paste("Error in return_survey_weighted_counts for q_label =", q_label, "and grps =", grps, ":", e$message))
          return(NULL)
        }
      )
      if (is.null(result) || !is.data.frame(result) || nrow(result) == 0) {
        warning(paste("No data returned for q_label =", q_label, "and grps =", grps))
        return(tibble())
      }
      result %>% mutate(filter = grps)
    }
  )

  services_filter_list[[filter]] <- filtered_data
}

services_with_filter <- bind_rows(services_filter_list)

# Process Data Without Filters -----------------------------------
services_no_filter <- purrr::map_dfr(vars, function(q_label) {
  cat("Processing without filters for q_label:", q_label, "\n")

  # Call return_survey_weighted_counts with the current q_label
  result <- tryCatch(
    return_survey_weighted_counts(q_label = q_label),
    error = function(e) {
      warning(paste("Error in return_survey_weighted_counts for q_label =", q_label, ":", e$message))
      return(NULL)
    }
  )

  # Validate the result
  if (is.null(result) || !is.data.frame(result) || nrow(result) == 0) {
    warning(paste("No data returned for q_label =", q_label))
    return(tibble())
  }

  # Return the result
  result
})

services_no_filter$filter <- "All LHDs"

# Combine Filtered and Unfiltered Data -----------------------------------
services_data <- bind_rows(services_with_filter, services_no_filter)

#  Assign Service Type and Service Provision ----------------------
services_data <- services_data %>%
  mutate(
    base_variable = stringr::str_extract(Question, "c6q\\d+"),  # Extract base variable
    suffix = stringr::str_extract(Question, "[a-z]$")           # Extract suffix
  ) %>%
  left_join(case_when_mappings, by = "base_variable") %>%  # Add service_type
  mutate(
    service_provision = case_when(
      suffix == "a" ~ "LHD (Directly)",
      suffix == "b" ~ "LHD (By Contract)",
      suffix == "i" ~ "Other Entity",
      suffix == "f" ~ "Not Available in Community",
      suffix == "g" ~ "Status Unknown",
      TRUE ~ NA_character_
    )
  )

# Filter and Keep Relevant Columns -------------------------------
services_data <- services_data %>%
  dplyr::select(all_of(c("Question", "Group", "Answer", "NA.RM", "N", "SE",
                         "Percentage", "x", "y", "category", "group", "subtitle",
                         "x_label", "Responses", "filter", "service_type",
                         "service_provision"))) %>%
  dplyr::mutate(service_classification = case_when(
    # Immunization
    stringr::str_detect(Question, "c6q55|c6q56|c6q147|c6q148") ~ "Immunization",

    # Disease/Risk Factor Screening
    stringr::str_detect(Question, "c6q142|c6q60|c6q61|c6q62|c6q63|c6q57|c6q58|c6q59") ~ "Disease/Risk Factor Screening",

    # Communicable Disease Treatment
    stringr::str_detect(Question, "c6q65|c6q66|c6q67") ~ "Communicable Disease Treatment",

    # Maternal and Child Health
    stringr::str_detect(Question, "c6q73|c6q69|c6q74|c6q71") ~ "Maternal and Child Health",

    # Other Clinical Services
    stringr::str_detect(Question, "c6q75|c6q76|c6q77|c6q78|c6q79") ~ "Other Clinical Services",

    # Epidemiology and Surveillance
    stringr::str_detect(Question, "c6q83|c6q81|c6q80|c6q82|c6q84|c6q86|c6q85") ~ "Epidemiology and Surveillance",

    # Population-Based Prevention
    stringr::str_detect(Question, "c6q89|c6q87|c6q95|c6q90a|c6q145|c6q91|c6q146|c6q93") ~ "Population-Based Prevention",

    # Regulation, Inspection, Licensing
    stringr::str_detect(Question, "c6q105|c6q97|c6q103|c6q110|c6q114|c6q115|c6q101|c6q116|c6q109|c6q111|c6q113|c6q112|c6q143|c6q102|c6q100|c6q107") ~ "Regulation, Inspection, Licensing",

    # Other Environmental Health
    stringr::str_detect(Question, "c6q127|c6q118|c6q124|c6q117|c6q121|c6q128|c6q144|c6q119|c6q120") ~ "Other Environmental Health",

    # Other Population-Based Services
    stringr::str_detect(Question, "c6q131|c6q130|c6q134|c6q137|c6q136") ~ "Other Population-Based Services",

    # Default category if no match found
    TRUE ~ "Uncategorized"
  ))


# %>%
#   filter(category == "Checked")

# Save and Print Elapsed Time ---------------------------------------------
end_time <- Sys.time() - start_time  # Calculate elapsed time
print(end_time)

usethis::use_data(services_data, overwrite = TRUE)


