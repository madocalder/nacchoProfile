## code to prepare `services_change_data` dataset goes here

devtools::load_all()
start_time <- Sys.time() # get start time


# Get Vars ----------------------------------------------------------------
data <- master_data %>%
  dplyr::select(dplyr::starts_with("c10q") & dplyr::ends_with("f"))

vars <- list(colnames(data))


# With Filter -------------------------------------------------------------
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

services_change_filter_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  filtered_data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    dplyr::mutate(filter = paste(filters[i]))

  filtered_data <- filtered_data %>%
    dplyr::select(1:12, 15, filter)

  services_change_filter_list[[i]] <- filtered_data
}

# bind the data generated in loop
services_change_with_filter <- do.call(rbind, services_change_filter_list)




# No Filter ---------------------------------------------------------------
services_change_no_filter <- pmap_dfr(vars, return_survey_weighted_counts) %>%
  dplyr::select(1:12, 14) %>%
  dplyr::mutate(filter = "All LHDs")


# Join Data ---------------------------------------------------------------
# join the two datasets -- with filters and with no filter
services_change_data <- rbind(services_change_with_filter, services_change_no_filter)



services_change_data <- services_change_data %>%
  dplyr::mutate(service_type = dplyr::case_when(Question == "c10q415f" ~ "Blood Lead Screening",
                                                Question == "c10q422f" ~ "Chronic Disease",
                                                Question == "c10q406f" ~ "Communicable Disease Screening or Treatment",
                                                Question == "c10q417f" ~ "Diabetes Screening",
                                                Question == "c10q411f" ~ "Emergency Preparedness",
                                                Question == "c10q420f" ~ "Environmental Health Including Food Safety",
                                                Question == "c10q405f" ~ "Epidemiology and Surveillance",
                                                Question == "c10q416f" ~ "High Blood Pressure Screening",
                                                Question == "c10q404f" ~ "Immunization",
                                                Question == "c10q408f" ~ "Maternal and Child Health Services",
                                                Question == "c10q421f" ~ "Mental Health",
                                                Question == "c10q418f" ~ "Obesity Prevention",
                                                Question == "c10q419f" ~ "Substance Use Prevention"))

services_change_data <- services_change_data %>%
  dplyr::mutate(service_provision = dplyr::case_when(Answer == "n/a" ~ "Service Not Provided",
                                                Answer == "reduced services" ~ "Reduced Service",
                                                Answer == "little/no change in service" ~ "No Change",
                                                Answer == "expanded services" ~ "Expanded Service",
                                                Answer == "don't know" ~ "Unknown")) %>%
  dplyr::filter(service_provision %in% c("Reduced Service", "No Change", "Expanded Service"))



services_change_data_n <- services_change_data %>%
  group_by(Question, group) %>%
  summarise(demoninator = sum(Responses))


services_change_data <- left_join(services_change_data, services_change_data_n, by = c("Question", "group"))


services_change_data <- services_change_data %>%
  group_by(Question, group) %>%
  dplyr::mutate(new_y = (N/sum(N) * 100)) %>%
  ungroup()


services_change_data <- services_change_data %>%
  dplyr::rename(old_y = y,
                y = new_y)


services_change_data <- services_change_data %>%
  dplyr::mutate(y = round(y, 1))





usethis::use_data(services_change_data, overwrite = TRUE)
