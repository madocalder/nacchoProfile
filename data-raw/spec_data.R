## code to prepare `spec_data` dataset goes here

spec_governance <- master_data %>%
  dplyr::mutate(fig_filter = c0govcat) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), fig_filter) %>%
  reshape2::melt(id = c("fig_filter")) %>%
  tidyr::drop_na(value) %>%
  dplyr::mutate(fig_cat = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(fig_filter, fig_cat) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(n = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "c0govcat") %>%
  haven::as_factor() %>%
  dplyr::mutate(fig_filter = as.character(fig_filter))




spec_rurality <- master_data %>%
  dplyr::mutate(fig_filter = urbanrural2022) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), fig_filter) %>%
  reshape2::melt(id = c("fig_filter")) %>%
  tidyr::drop_na(value) %>%
  tidyr::drop_na(fig_filter) %>%
  tidyr::drop_na(variable) %>%
  dplyr::mutate(fig_cat = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(fig_filter, fig_cat) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(n = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "urbanrural2022") %>%
  haven::as_factor() %>%
  dplyr::mutate(fig_filter = as.character(fig_filter))



spec_size <- master_data %>%
  dplyr::mutate(fig_filter = c0popcat3) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), fig_filter) %>%
  reshape2::melt(id = c("fig_filter")) %>%
  tidyr::drop_na(value) %>%
  tidyr::drop_na(fig_filter) %>%
  tidyr::drop_na(variable) %>%
  dplyr::mutate(fig_cat = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(fig_filter, fig_cat) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(n = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "c0popcat3") %>%
  haven::as_factor() %>%
  dplyr::mutate(fig_filter = as.character(fig_filter))



spec_region <- master_data %>%
  dplyr::mutate(fig_filter = censusregion) %>%
  dplyr::select(tidyselect::starts_with("c4q5"), fig_filter) %>%
  reshape2::melt(id = c("fig_filter")) %>%
  tidyr::drop_na(value) %>%
  tidyr::drop_na(fig_filter) %>%
  tidyr::drop_na(variable) %>%
  dplyr::mutate(fig_cat = dplyr::case_when(variable == "c4q502a" ~ "Medical Degree",
                                           variable == "c4q502b" ~ "Nursing",
                                           variable == "c4q502c" ~ "Public Health",
                                           variable == "c4q502d" ~ "Other")) %>%
  dplyr::group_by(fig_filter, fig_cat) %>%
  dplyr::summarise(denominator = dplyr::n(),
                   fig_value = sum(value)) %>%
  dplyr::mutate(n = round(fig_value/denominator*100,1)) %>%
  dplyr::mutate(filter = "censusregion") %>%
  haven::as_factor() %>%
  dplyr::mutate(fig_filter = as.character(fig_filter))


spec_data <- rbind(spec_governance, spec_rurality, spec_size, spec_region)


usethis::use_data(spec_data, overwrite = TRUE)
