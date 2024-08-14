# ## code to prepare `jobs_data` dataset goes here
# ## This script needs to be run to create the datafile called
# ## "jobs_data"
#
# devtools::load_all()
#
# start_time <- Sys.time() # get start time
#
#
# # only include those with non-missing data on both variables
# master_data <- master_data %>%
#   filter(joblossorgain == 1) # required for correct n
#
#
# # set variables and filters
# vars <- c("jobsgone", "jobsgain", "c5q36")
# filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
# jobs_list <- list() # create an empty list
#
#
# # loop used to shorten code and create the dataframes with each filter
# for (i in 1:length(filters)) {
#   args <- list(vars, filters[[i]], wtype = "weights_static")
#
#   data <- pmap_dfr(args, return_survey_weighted_statistics) %>%
#     mutate(filter = paste(filters[i]))
#
#   jobs_list[[i]] <- data
# }
#
#
# # bind the data generated in loop
# job_changes_with_filters <- do.call(rbind, jobs_list)
#
#
# # create the non-filtered dataset
# gone <- return_survey_weighted_statistics("jobsgone", wtype = "weights_static") #%>% dplyr::mutate(x = 0)
# gain <- return_survey_weighted_statistics("jobsgain", wtype = "weights_static") #%>% dplyr::mutate(x = 1)
# total <- return_survey_weighted_statistics("c5q36", wtype = "weights_static")
#
#
# job_changes_no_filters <- rbind(gone, gain, total) %>%
#   dplyr::mutate(filter = "No Filter")
#
#
# # almost final dataset
# jobs_data <- rbind(job_changes_with_filters, job_changes_no_filters)
#
#
# # split the data by question
# jobs_data_cats <- jobs_data %>%
#   dplyr::filter(Question != "c5q36")
#
# jobs_data_all <- jobs_data %>%
#   dplyr::filter(Question == "c5q36") %>%
#   dplyr::mutate(total_employees = Total) %>%
#   dplyr::select(Group, total_employees)
#
#
# # final data
# jobs_data <- left_join(jobs_data_cats, jobs_data_all, by = "Group")
#
# # raw total
# jobs_data <- jobs_data %>%
#   dplyr::mutate(group = stringr::str_to_title(Group),
#                 y = dplyr::case_when(Question == "jobsgone" ~ Total,
#                                      Question == "jobsgain" ~ Total),
#                 category = case_when(Question == "jobsgone" ~ "Jobs Lost",
#                                      Question == "jobsgain" ~ "Jobs Added"))
#
#
# # print elapsed time
# end_time <- Sys.time() - start_time # calculate difference
#
# # This shows how much time the script took and thus how much saved in load
# print(end_time) # saves 7 seconds
#
#
# # save the data
# openxlsx::write.xlsx(jobs_data, 'data-raw/jobs_data.xlsx')



## The reason I created this as a new dataset is for speed of the application.
## It would otherwise slow the load. If I rendered these dynamically when needed
## in the dashboard, it would be very, very slow because there is quite a bit
## of computation behind the creation of this dataset.
jobs_data <- readxl::read_excel("data-raw/jobs_data.xlsx")



usethis::use_data(jobs_data, overwrite = TRUE)
