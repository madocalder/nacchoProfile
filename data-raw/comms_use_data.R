## code to prepare `comms_use_data` dataset goes here

devtools::load_all()

vars <- c("m4q501l",
          "m4q501a",
          "m4q501b",
          "m4q501c",
          "m4q501e",
          "m4q501h",
          "m4q501k",
          "m4q501n",
          "m4q501o",
          "m4q501p",
          "m4q501q",
          "m4q501r",
          "m4q501u",
          "m4q501v",
          "m4q501w"
          #,
          # "m4q502l",
          # "m4q502a",
          # "m4q502b",
          # "m4q502c",
          # "m4q502e",
          # "m4q502h",
          # "m4q502k",
          # "m4q502n",
          # "m4q502o",
          # "m4q502p",
          # "m4q502q",
          # "m4q502r",
          # "m4q502u",
          # "m4q502v",
          # "m4q502w")
)

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

comms_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  comms_list[[i]] <- data
}


# bind the data generated in loop
comms_use_with_filters <- do.call(rbind, comms_list)

View(comms_use_with_filters)

vars <- c("m4q501l",
          "m4q501a",
          "m4q501b",
          "m4q501c",
          "m4q501e",
          "m4q501h",
          "m4q501k",
          "m4q501n",
          "m4q501o",
          "m4q501p",
          "m4q501q",
          "m4q501r",
          "m4q501u",
          "m4q501v",
          "m4q501w"
          #,
          # "m4q502l",
          # "m4q502a",
          # "m4q502b",
          # "m4q502c",
          # "m4q502e",
          # "m4q502h",
          # "m4q502k",
          # "m4q502n",
          # "m4q502o",
          # "m4q502p",
          # "m4q502q",
          # "m4q502r",
          # "m4q502u",
          # "m4q502v",
          # "m4q502w"
          )

# create data with no filter
comms_use_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))



# join the two datasets -- with filters and with no filter
comms_use_data <- rbind(comms_use_with_filters, comms_use_no_filter) %>%
  dplyr::mutate(comms_type = dplyr::case_when(Question %in% c("m4q501l") ~ "Automated phone calling",
                                             Question %in% c("m4q501a") ~ "Blogs",
                                             Question %in% c("m4q501b") ~ "Facebook",
                                             Question %in% c("m4q501c") ~ "LinkedIn",
                                             Question %in% c("m4q501e") ~ "Twitter",
                                             Question %in% c("m4q501h") ~ "Other social media",
                                             Question %in% c("m4q501k") ~ "Text messaging",
                                             Question %in% c("m4q501n") ~ "Fax broadcast/fax blast",
                                             Question %in% c("m4q501o") ~ "Hotline/Call Center",
                                             Question %in% c("m4q501p") ~ "Broadcast media",
                                             Question %in% c("m4q501q") ~ "Print media",
                                             Question %in% c("m4q501r") ~ "Email",
                                             Question %in% c("m4q501u") ~ "LHD website",
                                             Question %in% c("m4q501v") ~ "Health alert network",
                                             Question %in% c("m4q501w") ~ "Custom app for phone/table"
                                             ))


# Question %in% c("m4q501l", "m4q502l") ~ "Automated phone calling",
# Question %in% c("m4q501a", "m4q502a") ~ "Blogs",
# Question %in% c("m4q501b", "m4q502b") ~ "Facebook",
# Question %in% c("m4q501c", "m4q502c") ~ "LinkedIn",
# Question %in% c("m4q501e", "m4q502e") ~ "Twitter",
# Question %in% c("m4q501h", "m4q502h") ~ "Other social media",
# Question %in% c("m4q501k", "m4q502k") ~ "Text messaging",
# Question %in% c("m4q501n", "m4q502n") ~ "Fax broadcast/fax blast",
# Question %in% c("m4q501o", "m4q502o") ~ "Hotline/Call Center",
# Question %in% c("m4q501p", "m4q502p") ~ "Broadcast media",
# Question %in% c("m4q501q", "m4q502q") ~ "Print media",
# Question %in% c("m4q501r", "m4q502r") ~ "Email",
# Question %in% c("m4q501u", "m4q502u") ~ "LHD website",
# Question %in% c("m4q501v", "m4q502v") ~ "Health alert network",
# Question %in% c("m4q501w", "m4q502w") ~ "Custom app for phone/table"


usethis::use_data(comms_use_data, overwrite = TRUE)



