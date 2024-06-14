## code to prepare `landscape_filters` dataset goes here
landscape_filters <- list(
  "Rurality" = "urbanrural2022",
  "Governance" = "c0govcat",
  "LHD Size" = "c0popcat3",
  "Region" = "censusregion"
)
usethis::use_data(landscape_filters, overwrite = TRUE)
