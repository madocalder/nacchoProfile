## code to prepare `landscape_topics` dataset goes here
landscape_topics <- list(
  "Jurisdiction & Governance" = list("Population Served" = "totpop",
                                     "Geographies Served" = "geography",
                                     "Governance" = "c0govcatn",
                                     "Structure" = "c2q501",
                                     "Board of Health" = "lboh"),

  "Top Executives" = list("Age" = "age_cat",
                          "Race" = "race",
                          "Tenure" = "tenure",
                          "Highest Degree" = "c4q34",
                          "Specialization" = "field",
                          "Gender" = "c4q29")
)

usethis::use_data(landscape_topics, overwrite = TRUE)
