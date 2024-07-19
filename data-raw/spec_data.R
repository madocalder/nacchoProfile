## code to prepare `spec_data` dataset goes here

args1 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "c0popcat3",
  popcat = "c0popcat7"
)

args2 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "urbanrural2022",
  popcat = "c0popcat7"
)

args3 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "c0govcat",
  popcat = "c0popcat7"
)

args4 <- list(
  c("c4q502a", "c4q502b", "c4q502c"),
  grps = "censusregion",
  popcat = "c0popcat7"
)



spec_data_all <- map_dfr(args1[[1]], return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    AnswerLabel = q_remap[.data$Question],
    GroupLabel = label_remap[as.character(.data$Group)],
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = stringr::str_to_title(AnswerLabel),
    subtitle = paste0("Across All LHDs")
    )



spec_data_population <- pmap_dfr(args1, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    AnswerLabel = q_remap[.data$Question],
    filter = "c0popcat3",
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = stringr::str_to_title(AnswerLabel),
    subtitle = paste0("By LHD Size")
    )



spec_data_rurality <- pmap_dfr(args2, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    AnswerLabel = q_remap[.data$Question],
    filter = "urbanrural2022",
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = stringr::str_to_title(AnswerLabel),
    subtitle = paste0("By Rurality")
    )



spec_data_governance <- pmap_dfr(args3, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    AnswerLabel = q_remap[.data$Question],
    filter = "c0govcat",
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = stringr::str_to_title(AnswerLabel),
    subtitle = paste0("By Governance")
    )



spec_data_region <- pmap_dfr(args4, return_survey_weighted_counts) %>%
  filter(.data$Answer == "checked") %>%
  mutate(
    AnswerLabel = q_remap[.data$Question],
    filter = "censusregion",
    x = as.numeric(as.factor(AnswerLabel)) -1,
    category = stringr::str_to_title(AnswerLabel),
    subtitle = paste0("By Region")
    )


spec_data <- rbind(
  spec_data_population,
  spec_data_rurality,
  spec_data_governance,
  spec_data_region
)


usethis::use_data(spec_data, overwrite = TRUE)
usethis::use_data(spec_data_all, overwrite = TRUE)
