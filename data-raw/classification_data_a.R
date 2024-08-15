## code to prepare `classification_data_a` dataset goes here
## This script needs to be run to create the datafile called
## "classification_data_a"

# devtools::load_all()
#
# start_time <- Sys.time() # get start time
#
# vars <- c("c5q63a", "c5q62a", "c5q52a", "c5q64a",
#           "c5q61a", "c5q55a", "c5q47a", "c5q48a",
#           "c5q50a", "c5q60a", "c5q57a", "c5q58a",
#           "c5q49a", "c5q65a", "c5q59a", "c5q56a",
#           "c5q44a", "c5q51a", "c5q43a")
#
# filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#
# class_a_list <- list() # create an empty list
#
# # loop used to shorten code and create the dataframes with each filter
# for (i in 1:length(filters)) {
#   args <- list(vars, filters[[i]])
#
#   data <- pmap_dfr(args, return_survey_weighted_counts) %>%
#     select(1:12, "Responses") %>%
#     mutate(filter = paste(filters[i]))
#
#   class_a_list[[i]] <- data
# }
#
#
# # bind the data generated in loop
# class_a_with_filter <- do.call(rbind, class_a_list)
#
#
# # create data with no filter
# class_a_no_filter <- class_data(
#   c("c5q63a", "c5q62a", "c5q52a", "c5q64a",
#     "c5q61a", "c5q55a", "c5q47a", "c5q48a",
#     "c5q50a", "c5q60a", "c5q57a", "c5q58a",
#     "c5q49a", "c5q65a", "c5q59a", "c5q56a",
#     "c5q44a", "c5q51a", "c5q43a")) %>%
#   select(1:12, "Responses") %>%
#   mutate(filter = "All LHDs")
#
#
# # join the two datasets -- with filters and with no filter
# classification_data_a <- rbind(class_a_with_filter, class_a_no_filter)
#
#
# # print elapsed time
# end_time <- Sys.time() - start_time # calculate difference
# # This shows how much time the script took and thus how much saved in load
# print(end_time) # save 1.3 minutes
#
#
# # write the data
# openxlsx::write.xlsx(classification_data_a, 'data-raw/classification_data_a.xlsx')


## The reason I created this as a new dataset is for speed of the application.
## It would otherwise slow the load. If I rendered these dynmacially when needed
## in the dashboard, it would be very, very slow becasue there is quite a bit
## of computation behind the creation of this dataset.
classification_data_a <- readxl::read_excel("data-raw/classification_data_a.xlsx")

usethis::use_data(classification_data_a, overwrite = TRUE)
