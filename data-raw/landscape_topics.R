## code to prepare `landscape_topics` dataset goes here
landscape_topics <- list(
  "Population Served" = "totpop",
  "Geographies Served",
  "Governance",
  "Structure",
  "Board of Health"
)

usethis::use_data(landscape_topics, overwrite = TRUE)
