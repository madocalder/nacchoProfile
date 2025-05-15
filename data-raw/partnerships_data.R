#' Formal Local Health Department (LHD) Partnerships and Collaborations Dataset
#'
#' A dataset containing information on formal partnerships and collaborations of Local Health Departments (LHDs)
#' with various healthcare, community-based, and government organizations over time. The data reflects the
#' proportion of LHDs reporting formal collaborations with different types of organizations across selected years.
#'
#' @format A data frame with 88 rows and 4 variables:
#' \describe{
#'   \item{group}{Type of organization that LHDs partnered with. One of three values:
#'                   \code{"Healthcare Partners"}, \code{"Community-Based Partners"}, or \code{"Government Partners"}.}
#'   \item{category}{The specific type of organization within each category, e.g., \code{"Emergency responders"},
#'                       \code{"K-12 schools"}, \code{"Local planning"}, etc.}
#'   \item{year}{Year of reported partnership data. Available years are \code{2008}, \code{2016}, \code{2019}, and \code{2022}.}
#'   \item{y}{Percentage of LHDs reporting a formal partnership with each organization type in the given year.
#'                     Values are in percentage form, and some may be \code{NA} if data for that year/partner type is missing.}
#' }
#' @details
#' This dataset captures the evolution of formal collaborations (those with shared resources, personnel, or formal
#' agreements) that LHDs had with various organizations from 2008 to 2022. The data shows trends across three main
#' partner categories:
#' \itemize{
#'   \item Healthcare Partners: Includes organizations like emergency responders, hospitals, and mental health providers.
#'   \item Community-Based Partners: Includes schools, universities, non-profits, and faith communities.
#'   \item Government Partners: Includes local planning departments, criminal justice systems, and transportation agencies.
#' }
#' @source Extracted from visual data in a provided chart.
#'
#' @examples
#' # Load the dataset
#' data(partnerships)
#'
#' # View the first few rows of the dataset
#' head(partnerships)
#'
#' # Summary of partnerships by category
#' summary(partnerships$category)
#'
#' @keywords data
#' @name partnerships
"partnerships"

#
# ## Complete data frame with all years
# partnerships <- data.frame(
#   group = c(
#     # Healthcare Partners
#     rep("Healthcare Partnerships", 7 * 4),
#     # Community-Based Partners
#     rep("Community-Based Partnerships", 8 * 4),
#     # Government Partners
#     rep("Government Partnerships", 7 * 4)
#   ),
#   category = c(
#     # Healthcare Partners
#     rep("Emergency responders", 4),
#     rep("Hospitals", 4),
#     rep("Physician practices/medical groups", 4),
#     rep("Community health centers", 4),
#     rep("Mental health/substance abuse providers", 4),
#     rep("Veterinarians", 4),
#     # Community-Based Partners
#     rep("K-12 schools", 4),
#     rep("Community-based non-profits", 4),
#     rep("Media", 4),
#     rep("Businesses", 4),
#     rep("Faith communities", 4),
#     rep("Colleges or universities", 4),
#     rep("Libraries", 4),
#     # Government Partners
#     rep("Criminal justice system", 4),
#     rep("Economic and community development", 4),
#     rep("Local planning", 4),
#     rep("Housing", 4),
#     rep("Parks and recreation", 4),
#     rep("Transportation", 4),
#     rep("Tribal government", 4)
#   ),
#   year = rep(c(2008, 2016, 2019, 2022), times = 6 + 7 + 7),
#   y = c(
#     # Healthcare Partners
#     61, 39, 44, 52,
#     57, 35, 47, 55,
#     38, 26, 42, 43,
#     38, 21, 30, 34,
#     24, 23, 32, 32,
#     NA, 19, 34, 20,
#     # Community-Based Partners
#     59, 38, 49, 55,
#     45, 30, 53, 52,
#     34, 18, 39, 41,
#     28, 10, 23, 22,
#     19, 8, 19, 25,
#     18, 7, 26, 30,
#     13, 8, 12, 15,
#     # Government Partners
#     26, 10, 18, 18,
#     19, 14, 25, 30,
#     17, 9, 17, 20,
#     11, 9, 12, 15,
#     11, 7, 15, 18,
#     NA, 7, 15, 21,
#     NA, 7, 12, 15
#   ),
#   Percent_Informal = c(
#     # Healthcare Partners
#     rep(NA, 3), 98, rep(NA, 3), 98, rep(NA, 3), 95,
#     rep(NA, 3), 93, rep(NA, 3), 91, rep(NA, 3), 76,
#     # Community-Based Partners
#     rep(NA, 3), 99, rep(NA, 3), 97, rep(NA, 3), 95,
#     rep(NA, 3), 94, rep(NA, 3), 93, rep(NA, 3), 89,
#     rep(NA, 3), 83,
#     # Government Partners
#     rep(NA, 3), 88, rep(NA, 3), 88, rep(NA, 3), 83,
#     rep(NA, 3), 81, rep(NA, 3), 79, rep(NA, 3), 71,
#     rep(NA, 3), 47
#   ),
#   Percent_Formal = c(
#     # Healthcare Partners
#     rep(NA, 3), 79, rep(NA, 3), 75, rep(NA, 3), 46,
#     rep(NA, 3), 56, rep(NA, 3), 53, rep(NA, 3), 22,
#     # Community-Based Partners
#     rep(NA, 3), 78, rep(NA, 3), 64, rep(NA, 3), 28,
#     rep(NA, 3), 49, rep(NA, 3), 40, rep(NA, 3), 64,
#     rep(NA, 3), 33,
#     # Government Partners
#     rep(NA, 3), 46, rep(NA, 3), 41, rep(NA, 3), 40,
#     rep(NA, 3), 35, rep(NA, 3), 33, rep(NA, 3), 29,
#     rep(NA, 3), 20
#   )
# )
#
# partnerships <- partnerships %>%
#   mutate(variable = case_when(
#     group == "Healthcare Partnerships" ~ "health_part",
#     group == "Community-Based Partnerships" ~ "comm_part",
#     group == "Government Partnerships" ~ "gov_part"  # Assign NA for non-Partnerships categorys
#   ))

case_when_text <- "
stringr::str_detect(Question, \"m13q281\") ~ \"Business (community-based)\",
stringr::str_detect(Question, \"m13q280\") ~ \"Colleges or universities\",
stringr::str_detect(Question, \"m13q265\") ~ \"Community health centers\",
stringr::str_detect(Question, \"m13q286\") ~ \"Community-based nonprofits\",
stringr::str_detect(Question, \"m13q274\") ~ \"Cooperative extensions\",
stringr::str_detect(Question, \"m13q284\") ~ \"Criminal justice system\",
stringr::str_detect(Question, \"m13q270\") ~ \"Economic and community development agencies\",
stringr::str_detect(Question, \"m13q268\") ~ \"Emergency responders\",
stringr::str_detect(Question, \"m13q278\") ~ \"Faith communities\",
stringr::str_detect(Question, \"m13q267\") ~ \"Health insurers\",
stringr::str_detect(Question, \"m13q263\") ~ \"Hospitals\",
stringr::str_detect(Question, \"m13q271\") ~ \"Housing agencies\",
stringr::str_detect(Question, \"m13q275\") ~ \"K-12 schools\",
stringr::str_detect(Question, \"m13q279\") ~ \"Libraries\",
stringr::str_detect(Question, \"m13q269\") ~ \"Local planning agencies\",
stringr::str_detect(Question, \"m13q282\") ~ \"Media\",
stringr::str_detect(Question, \"m13q288\") ~ \"Mental health/substance abuse providers\",
stringr::str_detect(Question, \"m13q276\") ~ \"Parks and recreation\",
stringr::str_detect(Question, \"m13q264\") ~ \"Physician practices/medical groups\",
stringr::str_detect(Question, \"m13q277\") ~ \"Transportation\",
stringr::str_detect(Question, \"m13q283\") ~ \"Tribal government agencies\",
stringr::str_detect(Question, \"m13q289\") ~ \"Veterinarians\"
"
case_when_lines <- stringr::str_split(case_when_text, "\n")[[1]]  # Split into individual lines


case_when_mappings <- tibble(
  base_variable = stringr::str_extract(case_when_lines, "m13q\\d+"),  # Extract base variable (e.g., "c6q55")
  collab_type = stringr::str_match(case_when_lines, "~\\s*\"(.*?)\"")[,2]  # Extract the text inside quotes after '~'
) %>%
  filter(!is.na(base_variable) & !is.na(collab_type))  # Remove rows with NA values


relevant_columns <- unique(case_when_mappings$base_variable)
data <- master_data %>%
  select(matches(paste0("^(", paste(relevant_columns, collapse = "|"), ")([a-z]|work|work2|work3)?$")))


vars <- colnames(data)

#filtered data ----
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
collab_filter_list <- list()

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

  collab_filter_list[[filter]] <- filtered_data
}

collab_with_filter <- bind_rows(collab_filter_list)

# unfiltered data ----
# Process Data Without Filters -----------------------------------
collab_no_filter <- purrr::map_dfr(vars, function(q_label) {
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

collab_no_filter$filter <- "All LHDs"

# Combine Filtered and Unfiltered Data -----------------------------------
collab_data <- bind_rows(collab_with_filter, collab_no_filter)

partnerships <- collab_data %>%
  mutate(
    base_variable = stringr::str_extract(Question, "m13q\\d+"),  # Extract base variable
    suffix = stringr::str_remove(Question, "m13q\\d+") %>%      # Remove base variable
      stringr::str_extract("([a-z]|work|work2|work3)$")         # Extract suffix
  ) %>%
  left_join(case_when_mappings, by = "base_variable") %>%  # Add service_type
  mutate(
    collab_type = case_when(
      suffix == "a" ~ "Shared personnel/resources",
      suffix == "b" ~ "Written agreement",
      suffix == "c" ~ "Regularly scheduled meetings",
      suffix == "d" ~ "Exchange information",
      suffix == "e" ~ "No relationship",
      suffix == "f" ~ "NA",
      suffix == "work" ~ "working with organization in any way",
      suffix == "work2" ~ "regularly scheduling meetings, have written agreements, or share personnel/resources with organization",
      suffix == "work3" ~ "Formal",
      TRUE ~ NA_character_
    ),
    partner_type = dplyr::case_when(base_variable %in% c("m13q268", "m13q263",
                                                         "m13q264", "m13q265",
                                                        "m13q288", "m13q289",
                                                        "m13q267") ~ "Healthcare",
                                    base_variable %in% c("m13q275", "m13q286",
                                                         "m13q282", "m13q281",
                                                         "m13q278",  "m13q280",
                                                         "m13q279", "m13q274") ~ "Community",
                                    TRUE ~ "Government"
                                    ),
    partner = dplyr::case_when(
      base_variable == "m13q281" ~ "Business (community-based)",
      base_variable == "m13q280" ~ "Colleges or universities",
      base_variable == "m13q265" ~ "Community health centers",
      base_variable == "m13q286" ~ "Community-based nonprofits",
      base_variable == "m13q274" ~ "Cooperative extensions",
      base_variable == "m13q284" ~ "Criminal justice system",
      base_variable == "m13q270" ~ "Economic and community development agencies",
      base_variable == "m13q268" ~ "Emergency responders",
      base_variable == "m13q278" ~ "Faith communities",
      base_variable == "m13q267" ~ "Health insurers",
      base_variable == "m13q263" ~ "Hospitals",
      base_variable == "m13q271" ~ "Housing agencies",
      base_variable == "m13q275" ~ "K-12 schools",
      base_variable == "m13q279" ~ "Libraries",
      base_variable == "m13q269" ~ "Local planning agencies",
      base_variable == "m13q282" ~ "Media",
      base_variable == "m13q288" ~ "Mental health/substance abuse providers",
      base_variable == "m13q276" ~ "Parks and recreation",
      base_variable == "m13q264" ~ "Physician practices/medical groups",
      base_variable == "m13q277" ~ "Transportation",
      base_variable == "m13q283" ~ "Tribal government agencies",
      base_variable == "m13q289" ~ "Veterinarians"


    )
  )%>%
  dplyr::select(Question:filter, c0govcat:partner)



# Save the processed data for package use
usethis::use_data(partnerships, overwrite = TRUE)

