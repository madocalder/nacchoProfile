## code to prepare `workforce_topics` dataset goes here
workforce_topics <- list("Full-Time Equivalents" = "c5q37",
                             "Employees" = "c5q36",
                             "Jobs Lost and Gained" = "jobs",
                             "Occupation Classifications" = "occupation_class"
)


usethis::use_data(workforce_topics, overwrite = TRUE)
