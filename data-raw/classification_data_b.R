## code to prepare `classification_data_b` dataset goes here
## This script needs to be run to create the datafile called
## "classification_data_b"

devtools::load_all()

start_time <- Sys.time() # get start time

vars <- c(
  "c5q63b", "c5q62b",
  "c5q52b", "c5q64b",
  "c5q61b", "c5q68b", "c5q55b",
  "c5q47b", "c5q48b",
  "c5q50b", "c5q60b",
  "c5q57b", "c5q58b",
  "c5q49b", "c5q65b",
  "c5q59b", "c5q56b",
  "c5q44b", "c5q51b",
  "c5q43b")

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

class_b_list <- list() # create an empty list


# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]], wtype = "weights_static")

  data <- pmap_dfr(args, return_survey_weighted_statistics) %>%
    mutate(filter = paste(filters[i]))

  class_b_list[[i]] <- data
}


# bind the data generated in loop
class_b_with_filter <- do.call(rbind, class_b_list)


# create data with no filter
class_b_no_filter <- class_data_b(
  list(
    c(
      "c5q63b", "c5q62b",
      "c5q52b", "c5q64b",
      "c5q61b", "c5q68b", "c5q55b",
      "c5q47b", "c5q48b",
      "c5q50b", "c5q60b",
      "c5q57b", "c5q58b",
      "c5q49b", "c5q65b",
      "c5q59b", "c5q56b",
      "c5q44b", "c5q51b",
      "c5q43b"),
    wtype = "weights_static"
  )
) %>%
  dplyr::mutate(subtitle = "Estimated Across All LHDs") %>% # change subtitle
  mutate(filter = "All LHDs")


# join the two datasets -- with filters and with no filter
classification_data_b <- rbind(class_b_with_filter, class_b_no_filter)


# clean up and prep for use in dashboard
classification_data_b <- classification_data_b %>%
  dplyr::mutate(Question = gsub("new", "", Question),
                category = "Yes",
                y = Total,
                group = sapply(Group, custom_str_to_title))
# %>%
#   dplyr::mutate(y = round_up(y, -2))


# print elapsed time
end_time <- Sys.time() - start_time # calculate difference
print(end_time) # 1.073727 mins


usethis::use_data(classification_data_b, overwrite = TRUE)
devtools::document()
