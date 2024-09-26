## code to prepare `advocacy_data` dataset goes here -------

devtools::load_all()

vars <- c("c12q260x", "c12q260aa",
          "c12q260n", "c12q260o",
          "c12q260e", "c12q260y",
          "c12q260ac", "c12q260w",
          "c12q260p", "c12q260f",
          "c12q260q", "c12q260h",
          "c12q260r", "c12q260s",
          "c12q260af", "c12q260ag",
          "c12q260t", "c12q260u",
          "c12q260v", "c12q260z",
          "c12q260j", "c12q260k"
          )

filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

advocacy_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  advocacy_list[[i]] <- data
}


# bind the data generated in loop
advocacy_with_filters <- do.call(rbind, advocacy_list)

# View(advocacy_with_filters)

vars <- c("c12q260x", "c12q260aa",
          "c12q260n", "c12q260o",
          "c12q260e", "c12q260y",
          "c12q260ac", "c12q260w",
          "c12q260p", "c12q260f",
          "c12q260q", "c12q260h",
          "c12q260r", "c12q260s",
          "c12q260af", "c12q260ag",
          "c12q260t", "c12q260u",
          "c12q260v", "c12q260z",
          "c12q260j", "c12q260k"
)

# create data with no filter
advocacy_no_filter <- map_dfr(vars, return_survey_weighted_counts) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))



# join the two datasets -- with filters and with no filter
advocacy_data <- rbind(advocacy_with_filters, advocacy_no_filter) %>%
  dplyr::mutate(advocacy_type = dplyr::case_when(Question %in% c("c12q260x") ~ "Climate Change",
                                                 Question %in% c("c12q260aa") ~ "Emergency preparedness and response",
                                                 Question %in% c("c12q260n") ~ "Emergency Preparedness Response",
                                                 Question %in% c("c12q260o") ~ "Food safety",
                                                 Question %in% c("c12q260e") ~ "Funding for access to healthcare",
                                                 Question %in% c("c12q260y") ~ "Funding for local public health",
                                                 Question %in% c("c12q260ac") ~ "COVID-19 Infectious disease",
                                                 Question %in% c("c12q260w") ~ "Infectious disease",
                                                 Question %in% c("c12q260p") ~ "Injury/violence prevention",
                                                 Question %in% c("c12q260f") ~ "Land use",
                                                 Question %in% c("c12q260q") ~ "Mental health",
                                                 Question %in% c("c12q260h") ~ "Occupational health and safety",
                                                 Question %in% c("c12q260r") ~ "Obesity/physical activity",
                                                 Question %in% c("c12q260s") ~ "Oral health",
                                                 Question %in% c("c12q260af") ~ "Reforms related to community policing",
                                                 Question %in% c("c12q260ag") ~ "Planning external resource allocation using an equity lens",
                                                 Question %in% c("c12q260t") ~ "Safe and healthy housing",
                                                 Question %in% c("c12q260u") ~ "Tobacco, alcohol, and other drugs",
                                                 Question %in% c("c12q260v") ~ "Wastewater/sanitation",
                                                 Question %in% c("c12q260z") ~ "Other environmental health",
                                                 Question %in% c("c12q260j") ~ "Other policy areas",
                                                 Question %in% c("c12q260k") ~ "None")
  )


usethis::use_data(advocacy_data, overwrite = TRUE)


# View(advocacy_data)


## code to prepare `new_advocacy_data` dataset goes here ----

devtools::load_all()


vars <- c("newc12q502x", "newc12q502aa",
          "newc12q502b", "newc12q502ab",
          "newc12q502o", "newc12q502i",
          "newc12q502y", "newc12q502ac",
          "newc12q502r", "newc12q502m",
          "newc12q502s", "newc12q502q",
          "newc12q502j", "newc12q502l",
          "newc12q502p", "newc12q502t",
          "newc12q502k", "newc12q502v",
          "newc12q502z", "newc12q502f"

)


filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

new_advocacy_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  # Use pmap to iterate without combining yet
  data_list <- purrr::pmap(args, return_survey_weighted_counts)

  # Check each data frame's Answer column and convert it to a consistent type
  data_list <- lapply(data_list, function(data) {
    if ("Answer" %in% colnames(data)) {
      data <- data %>%
        mutate(Answer = as.character(Answer))  # Or use as.numeric, depending on the case
    }
    return(data)
  })

  # Now bind the rows after ensuring consistent types
  data <- dplyr::bind_rows(data_list) %>%
    select(1:12, "Responses") %>%
    mutate(filter = paste(filters[i]))

  # Store in the result list
  new_advocacy_list[[i]] <- data
}


# bind the data generated in loop
new_advocacy_with_filters <- do.call(rbind, new_advocacy_list)

# View(new_advocacy_with_filters)

vars <- c("newc12q502x", "newc12q502aa",
          "newc12q502b", "newc12q502ab",
          "newc12q502o", "newc12q502i",
          "newc12q502y", "newc12q502ac",
          "newc12q502r", "newc12q502m",
          "newc12q502s", "newc12q502q",
          "newc12q502j", "newc12q502l",
          "newc12q502p", "newc12q502t",
          "newc12q502k", "newc12q502v",
          "newc12q502z", "newc12q502f"

)

# create data with no filter
new_advocacy_no_filter <- map_dfr(vars, function(var) {
  data <- return_survey_weighted_counts(var)  # Apply the function to get the data

  # Ensure that 'Answer' is consistently a character
  if ("Answer" %in% colnames(data)) {
    data <- data %>%
      mutate(Answer = as.character(Answer))  # Convert 'Answer' to character
  }

  return(data)
}) %>%
  select(1:12, "Responses") %>%
  mutate(filter = paste("All LHDs"))

# View(new_advocacy_no_filter)

# join the two datasets -- with filters and with no filter
new_advocacy_data <- rbind(new_advocacy_with_filters, new_advocacy_no_filter) %>%
  dplyr::mutate(new_advocacy_type = dplyr::case_when(
    Question %in% c("newc12q502x") ~ "Climate Change",
    Question %in% c("newc12q502aa") ~ "COVID-19 Emergency preparedness and response",
    Question %in% c("newc12q502b") ~ "Emergency preparedness and response",
    Question %in% c("newc12q502ab") ~ "Declared racism a threat to public health or an emergency",
    Question %in% c("newc12q502o") ~ "Food safety",
    Question %in% c("newc12q502i") ~ "Funding for access to healthcare",
    Question %in% c("newc12q502y") ~ "Funding for local public health",
    Question %in% c("newc12q502ac") ~ "COVID-19 Infectious disease",
    Question %in% c("newc12q502r") ~ "Infectious disease",
    Question %in% c("newc12q502m") ~ "Injury/violence prevention",
    Question %in% c("newc12q502s") ~ "Land use planning",
    Question %in% c("newc12q502q") ~ "Mental health",
    Question %in% c("newc12q502j") ~ "Occupational health and safety",
    Question %in% c("newc12q502l") ~ "Obesity/physical activity",
    Question %in% c("newc12q502p") ~ "Oral health",
    Question %in% c("newc12q502t") ~ "Safe and healthy housing",
    Question %in% c("newc12q502k") ~ "Tobacco, alcohol, and other drugs",
    Question %in% c("newc12q502v") ~ "Wastewater/sanitation",
    Question %in% c("newc12q502z") ~ "Other environmental health",
    Question %in% c("newc12q502f") ~ "Other area"
  )
  )

# View(new_advocacy_data)

usethis::use_data(new_advocacy_data, overwrite = TRUE)

