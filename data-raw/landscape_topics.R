## code to prepare `landscape_topics` dataset goes here
landscape_topics <- list(
  "Agency Characteristics" = list("Structure" = "c2q501",
                                  "Board of Health" = "lboh"),

  "Executive Characteristics" = list("Age" = "age_cat",
                                     "Race" = "race",
                                     "Ethnicity" = "ethnicity",
                                     "Gender" = "c4q29",
                                     "Tenure" = "tenure",
                                     "Highest Degree" = "c4q34",
                                     "Specialization" = "field"
                                     )
 )


usethis::use_data(landscape_topics, overwrite = TRUE)
