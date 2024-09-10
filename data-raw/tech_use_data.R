## code to prepare `tech_use_data` dataset goes here

devtools::load_all()

vars <- c("m4q300",
          "m4q401",
          "m4q301",
          "m4q402",
          "m4q302",
          "m4q304")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

tech_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  tech_list[[i]] <- data
}


# bind the data generated in loop
tech_use_with_filters <- do.call(rbind, tech_list)



vars <- list("m4q300",
                "m4q401",
                "m4q301",
                "m4q402",
                "m4q302",
                "m4q304")

# create data with no filter
tech_use_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))


# join the two datasets -- with filters and with no filter
tech_use_data <- rbind(tech_use_with_filters, tech_use_no_filter) %>%
  dplyr::mutate(tech_type = dplyr::case_when(Question == "m4q300" ~ "Electronic Case Reporting (eCR)",
                                             Question == "m4q401" ~ "Electronic Disease Reporting System",
                                             Question == "m4q301" ~ "Electronic Health Records",
                                             Question == "m4q402" ~ "Electronic Lab Reporting",
                                             Question == "m4q302" ~ "Health Information Exchange",
                                             Question == "m4q304" ~ "Immunization Registry"))


usethis::use_data(tech_use_data, overwrite = TRUE)
