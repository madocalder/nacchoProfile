## code to prepare `revenue_percentages` dataset goes here
devtools::load_all()


args <- c("c3q17p", "c3q17c", "c3q17d", "c3q17e",
          "c3q17r", "c3q17i", "c3q17j", "c3q17v",
          "c3q17h", "c3q17m")

# Loop through each argument in 'args' to calculate revenue percentages
revenue_percentages <- do.call(rbind, lapply(args, function(i) {
  data <- return_survey_weighted_statistics(i, wtype = "weights_static")

  data.frame(
    revenue_total = sum(data$Total, na.rm = TRUE),
    revenue_type = i
  )
}))

revenue_percentages <- revenue_percentages %>%
  dplyr::mutate(revenue_type = dplyr::case_when(revenue_type == "c3q17p" ~ "Local",
                                                revenue_type == "c3q17c" ~ "State",
                                                revenue_type == "c3q17d" ~ "Federal Pass Through",
                                                revenue_type == "c3q17e" ~ "Federal Direct",
                                                revenue_type == "c3q17r" ~ "Medicare or Medicaid",
                                                revenue_type == "c3q17i" ~ "Private Health Insurance",
                                                revenue_type == "c3q17j" ~ "Patient Fees",
                                                revenue_type == "c3q17v" ~ "Non-Clinical Fees/Fines",
                                                revenue_type == "c3q17h" ~ "Private Foundation",
                                                revenue_type == "c3q17m" ~ "Other")) %>%
  dplyr::mutate(percentage = revenue_total / sum(revenue_total))





## code to prepare `revenue_percentages` dataset goes here
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

revenue_by_filter_percentages <- list()

# Loop through each argument in 'args' to calculate revenue percentages
for (i in 1:length(filters)) {

  args_filters <- list(args, filters[[i]], wtype = "weights_static")
  data <- pmap_dfr(args_filters, return_survey_weighted_statistics) %>%
    dplyr::mutate(filter = paste(filters[i]))


  revenue_by_filter_percentages[[i]] <- data
}

# bind the data generated in loop and wrangle
revenue_percentages_filter <- do.call(rbind, revenue_by_filter_percentages) %>%
  dplyr::select(Question, Group, N, Responses, Total, x, category, subtitle, filter) %>%
  dplyr::group_by(Group) %>%
  dplyr::mutate(overall_total = sum(Total))%>%
  dplyr::mutate(revenue_type = dplyr::case_when(Question == "c3q17p" ~ "Local",
                                                Question == "c3q17c" ~ "State",
                                                Question == "c3q17d" ~ "Federal Pass Through",
                                                Question == "c3q17e" ~ "Federal Direct",
                                                Question == "c3q17r" ~ "Medicare or Medicaid",
                                                Question == "c3q17i" ~ "Private Health Insurance",
                                                Question == "c3q17j" ~ "Patient Fees",
                                                Question == "c3q17v" ~ "Non-Clinical Fees/Fines",
                                                Question == "c3q17h" ~ "Private Foundation",
                                                Question == "c3q17m" ~ "Other")) %>%
  dplyr::mutate(percentage = Total / overall_total) %>%
  ungroup() %>%
  dplyr::select(revenue_type, percentage, x, category, subtitle, filter)



## Below would give you the percentage of the total, not by the group
# revenue_percentages_filter <- do.call(rbind, revenue_by_filter_percentages) %>%
#   dplyr::select(Question, Group, N, Responses, Total, x, category, subtitle) %>%
#   dplyr::group_by(Question, Group) %>%
#   dplyr::mutate(overall_total = sum(Total))%>%
#   dplyr::mutate(revenue_type = dplyr::case_when(Question == "c3q17p" ~ "Local",
#                                                 Question == "c3q17c" ~ "State",
#                                                 Question == "c3q17d" ~ "Federal Pass Through",
#                                                 Question == "c3q17e" ~ "Federal Direct",
#                                                 Question == "c3q17r" ~ "Medicare or Medicaid",
#                                                 Question == "c3q17i" ~ "Private Health Insurance",
#                                                 Question == "c3q17j" ~ "Patient Fees",
#                                                 Question == "c3q17v" ~ "Non-Clinical Fees/Fines",
#                                                 Question == "c3q17h" ~ "Private Foundation",
#                                                 Question == "c3q17m" ~ "Other"))%>%
#   dplyr::mutate(percentage = Total / overall_total)


usethis::use_data(revenue_percentages_filter, overwrite = TRUE)
usethis::use_data(revenue_percentages, overwrite = TRUE)
