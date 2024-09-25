## code to prepare `med_data` dataset goes here

devtools::load_all()

#medical services------
vars <- c("medical")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

medical_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    dplyr::mutate(filter = paste(filters[i]))

  medical_list[[i]] <- data
}


# bind the data generated in loop
medical_with_filters <- do.call(rbind, medical_list)

View(medical_with_filters)

vars <- c("medical")

# create data with no filter
med_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# join the two datasets -- with filters and with no filter
med_data <- rbind(medical_with_filters, med_no_filter) %>%
  mutate(category = case_when(
  .data$category == 1 ~ "Medical Services Provided",
  .data$Answer == 0 ~ "Medical Services Not Provided"))

View(med_data)

usethis::use_data(med_data, overwrite = TRUE)

#dental services-----

vars <- c("dental")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

dental_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    dplyr::mutate(filter = paste(filters[i]))

  dental_list[[i]] <- data
}


# bind the data generated in loop
dental_with_filters <- do.call(rbind, dental_list)

View(dental_with_filters)

vars <- c("dental")

# create data with no filter
dental_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# join the two datasets -- with filters and with no filter
dental_data <- rbind(dental_with_filters, dental_no_filter) %>%
  mutate(category = case_when(
    .data$category == 1 ~ "Dental Services Provided",
    .data$Answer == 0 ~ "Dental Services Not Provided"))


View(dental_data)

usethis::use_data(dental_data, overwrite = TRUE)


#behavioral services-----

vars <- c("behavioral")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

behav_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    dplyr::mutate(filter = paste(filters[i]))

  behav_list[[i]] <- data
}


# bind the data generated in loop
behav_with_filters <- do.call(rbind, behav_list)

View(behav_with_filters)

vars <- c("behavioral")

# create data with no filter
behav_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# join the two datasets -- with filters and with no filter
behav_data <- rbind(behav_with_filters, behav_no_filter) %>%
  mutate(category = case_when(
    .data$category == 1 ~ "Behavioral Services Provided",
    .data$Answer == 0 ~ "Behavioral Services Not Provided"))


View(behav_data)

usethis::use_data(behav_data, overwrite = TRUE)


