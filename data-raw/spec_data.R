#' Prepare Specialized Data with Demographic Filters
#'
#' This script prepares datasets that analyze survey responses for Local Health Departments (LHDs)
#' based on various demographic groupings. The datasets are aggregated and filtered by population size,
#' rurality, governance type, and census region.
#'
#' @details
#' The script performs the following steps:
#' - Filters survey responses where `Answer` is `"checked"`.
#' - Recodes the `Answer` and `Group` fields to generate descriptive labels (`AnswerLabel` and `GroupLabel`).
#' - Categorizes data into demographic groups such as "By LHD Size", "By Rurality", "By Governance", and "By Region".
#' - Uses the `return_survey_weighted_counts()` function to generate survey-weighted counts.
#' - Combines datasets grouped by demographic categories into a single dataset (`spec_data`).
#' - Saves the final datasets using `usethis::use_data()`.
#'
#' The following demographic groupings are applied:
#' - Population Size: `c0popcat3`
#' - Rurality: `urbanrural2022`
#' - Governance Type: `c0govcat`
#' - Census Region: `censusregion`
#'
#' @param questions A character vector of survey question identifiers (e.g., `c("c4q502a", "c4q502b", "c4q502c", "c4q502d")`).
#' @param groupings A list of demographic grouping variables, with corresponding population categories:
#'   - `args1`: Groups by `c0popcat3` (population size).
#'   - `args2`: Groups by `urbanrural2022` (rurality classification).
#'   - `args3`: Groups by `c0govcat` (governance type).
#'   - `args4`: Groups by `censusregion` (census regions).
#'
#' @return Saves the following datasets as `.rda` files in the package:
#' - `spec_data_all`: Aggregated dataset across all LHDs.
#' - `spec_data`: Aggregated and grouped dataset by demographic filters.
#'
#' @seealso [return_survey_weighted_counts()] for the function used to generate the survey-weighted counts.
#'
#' @examples
#' # Example usage:
#' devtools::load_all()
#'
#' # Define arguments
#' args1 <- list(
#'   c("c4q502a", "c4q502b", "c4q502c", "c4q502d"),
#'   grps = "c0popcat3",
#'   popcat = "c0popcat7"
#' )
#'
#' args2 <- list(
#'   c("c4q502a", "c4q502b", "c4q502c", "c4q502d"),
#'   grps = "urbanrural2022",
#'   popcat = "c0popcat7"
#' )
#'
#' # Prepare and save datasets
#' spec_data_population <- pmap_dfr(args1, return_survey_weighted_counts)
#' spec_data_rurality <- pmap_dfr(args2, return_survey_weighted_counts)
#'
#' spec_data <- rbind(spec_data_population, spec_data_rurality)
#' usethis::use_data(spec_data, overwrite = TRUE)
#' usethis::use_data(spec_data_all, overwrite = TRUE)
#'
#' @export

global_levels <- c("Medical",
                   "Nursing",
                   "Public Health")
#popsize
args1 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "c0popcat3",
  popcat = "c0popcat7"
)
#rurality
args2 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "urbanrural2022",
  popcat = "c0popcat7"
)
#governance
args3 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "c0govcat",
  popcat = "c0popcat7"
)
#region
args4 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "censusregion",
  popcat = "c0popcat7"
)



spec_data_all <- map_dfr(args1[[1]], return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    AnswerLabel = factor(q_remap[.data$Question], levels = global_levels),
    GroupLabel = label_remap[as.character(.data$Group)],
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = sapply(AnswerLabel, custom_str_to_title),
    subtitle = paste0("Estimated Across All LHDs")
    )



spec_data_population <- pmap_dfr(args1, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    filter = "c0popcat3",
    AnswerLabel = factor(q_remap[.data$Question], levels = global_levels),
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = sapply(AnswerLabel, custom_str_to_title),
    subtitle = paste0("By LHD Size")
  ) %>%
  select(-c0popcat3)



spec_data_rurality <- pmap_dfr(args2, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    filter = "urbanrural2022",
    AnswerLabel = factor(q_remap[.data$Question], levels = global_levels),
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = sapply(AnswerLabel, custom_str_to_title),
    subtitle = paste0("By Rurality")
  ) %>%
  select(-urbanrural2022)



spec_data_governance <- pmap_dfr(args3, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    filter = "c0govcat",
    AnswerLabel = factor(q_remap[.data$Question], levels = global_levels),
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = sapply(AnswerLabel, custom_str_to_title),
    subtitle = paste0("By Governance")
  ) %>%
  select(-c0govcat)



spec_data_region <- pmap_dfr(args4, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    filter = "censusregion",
    AnswerLabel = factor(q_remap[.data$Question], levels = global_levels),
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = sapply(AnswerLabel, custom_str_to_title),
    subtitle = paste0("By Region")
  ) %>%
  select(-censusregion)



spec_data <- rbind(
  spec_data_population,
  spec_data_rurality,
  spec_data_governance,
  spec_data_region
)


spec_data <- spec_data %>%
  mutate(AnswerLabel = factor(AnswerLabel, levels = global_levels))

usethis::use_data(spec_data, overwrite = TRUE)
usethis::use_data(spec_data_all, overwrite = TRUE)

devtools::document()
