## code to prepare `spec_data` dataset goes here

spec_governance <- master_data %>%
  dplyr::mutate(group = c0govcat) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), group) %>%
  reshape2::melt(id = c("group")) %>%
  tidyr::drop_na(value) %>%
  dplyr::mutate(category = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(group, category) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(y = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "c0govcat") %>%
  haven::as_factor() %>%
  dplyr::mutate(group = as.character(group))




spec_rurality <- master_data %>%
  dplyr::mutate(group = urbanrural2022) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), group) %>%
  reshape2::melt(id = c("group")) %>%
  tidyr::drop_na(value) %>%
  tidyr::drop_na(group) %>%
  tidyr::drop_na(variable) %>%
  dplyr::mutate(category = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(group, category) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(y = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "urbanrural2022") %>%
  haven::as_factor() %>%
  dplyr::mutate(group = as.character(group))



spec_size <- master_data %>%
  dplyr::mutate(group = c0popcat3) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), group) %>%
  reshape2::melt(id = c("group")) %>%
  tidyr::drop_na(value) %>%
  tidyr::drop_na(group) %>%
  tidyr::drop_na(variable) %>%
  dplyr::mutate(category = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(group, category) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(y = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "c0popcat3") %>%
  haven::as_factor() %>%
  dplyr::mutate(group = as.character(group))



spec_region <- master_data %>%
  dplyr::mutate(group = censusregion) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), group) %>%
  reshape2::melt(id = c("group")) %>%
  tidyr::drop_na(value) %>%
  tidyr::drop_na(group) %>%
  tidyr::drop_na(variable) %>%
  dplyr::mutate(category = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(group, category) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(y = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "censusregion") %>%
  haven::as_factor() %>%
  dplyr::mutate(group = as.character(group))


spec_data <- rbind(spec_governance, spec_rurality, spec_size, spec_region)


usethis::use_data(spec_data, overwrite = TRUE)
