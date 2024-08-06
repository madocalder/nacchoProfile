## code to prepare `workforce_classifications_measure` dataset goes here

workforce_classifications_measure <- c("% of LHDs with Classification" = "a",
                              "Total FTEs in Classification" = "b")

usethis::use_data(workforce_classifications_measure, overwrite = TRUE)
