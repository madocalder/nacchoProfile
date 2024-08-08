## code to prepare `classification_data_b` dataset goes here
## This script needs to be run to create the datafile called
## "classification_data_b"

# devtools::load_all()
#
# start_time <- Sys.time() # get start time
#
# vars <- c(
#   "newc5q63b", "newc5q62b",
#   "newc5q52b", "newc5q64b",
#   "newc5q61b", "newc5q55b",
#   "newc5q47b", "newc5q48b",
#   "newc5q50b", "newc5q60b",
#   "newc5q57b", "newc5q58b",
#   "newc5q49b", "newc5q65b",
#   "newc5q59b", "newc5q56b",
#   "newc5q44b", "newc5q51b",
#   "newc5q43b")
#
# filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#
# class_b_list <- list() # create an empty list
#
#
# # loop used to shorten code and create the dataframes with each filter
# for (i in 1:length(filters)) {
#   args <- list(vars, filters[[i]], wtype = "weights_static")
#
#   data <- pmap_dfr(args, return_survey_weighted_statistics) %>%
#     mutate(filter = paste(filters[i]))
#
#   class_b_list[[i]] <- data
# }
#
#
# # bind the data generated in loop
# class_b_with_filter <- do.call(rbind, class_b_list)
#
#
# # create data with no filter
# class_b_no_filter <- class_data_b(
#   list(
#     c(
#       "newc5q63b", "newc5q62b",
#       "newc5q52b", "newc5q64b",
#       "newc5q61b", "newc5q55b",
#       "newc5q47b", "newc5q48b",
#       "newc5q50b", "newc5q60b",
#       "newc5q57b", "newc5q58b",
#       "newc5q49b", "newc5q65b",
#       "newc5q59b", "newc5q56b",
#       "newc5q44b", "newc5q51b",
#       "newc5q43b"),
#     wtype = "weights_static"
#   )
# ) %>%
#   dplyr::mutate(subtitle = "Across All LHDs") %>% # change subtitle
#   mutate(filter = "No Filter")
#
#
# # join the two datasets -- with filters and with no filter
# classification_data_b <- rbind(class_b_with_filter, class_b_no_filter)
#
#
# # clean up and prep for use in dashboard
# classification_data_b <- classification_data_b %>%
#   dplyr::mutate(Question = gsub("new", "", Question),
#                 category = "Yes",
#                 y = Total)
#
#
# # print elapsed time
# end_time <- Sys.time() - start_time # calculate difference
#
# # This shows how much time the script took and thus how much saved in load
# print(end_time) # saves 53 seconds
#
#
# # save the data
# openxlsx::write.xlsx(classification_data_b, 'data-raw/classification_data_b.xlsx')



## The reason I created this as a new dataset is for speed of the application.
## It would otherwise slow the load. If I rendered these dynamically when needed
## in the dashboard, it would be very, very slow because there is quite a bit
## of computation behind the creation of this dataset.
classification_data_b <- readxl::read_excel("data-raw/classification_data_b.xlsx")


usethis::use_data(classification_data_b, overwrite = TRUE)
