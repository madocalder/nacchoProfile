## code to prepare `budget_data` dataset goes here

devtools::load_all()

start_time <- Sys.time() # get start time

master_data <- master_data %>%
  dplyr::mutate(budget_changes = dplyr::case_when(c10q301 == 1 ~ "Less Than Previous Year's Budget",
                                                  c10q301 == 2 | c10q301 == 3 ~ "The Same or Higher Than Last Year's Budget",
                                                  c10q301 == 4 ~ NA))

# set variables and filters
vars <- c("budget_changes")
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
budget_list <- list() # create an empty list


# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]], wtype = "weights_static")

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    mutate(filter = paste(filters[i])) %>%
    select(-14)

  budget_list[[i]] <- data

}


# bind the data generated in loop
budget_changes_with_filters <- do.call(rbind, budget_list)


# create the non-filtered dataset
budget_changes_no_filters <- return_survey_weighted_counts("budget_changes", wtype = "weights_static") %>%
  dplyr::mutate(filter = "All LHDs")


# almost final dataset
budget_data <- rbind(budget_changes_with_filters, budget_changes_no_filters)


# print elapsed time
end_time <- Sys.time() - start_time # calculate difference

# This shows how much time the script took
print(end_time) # 5 seconds


usethis::use_data(budget_data, overwrite = TRUE)
devtools::document()

