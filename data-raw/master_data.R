## code to prepare `master_data` dataset goes here

master_data <- haven::read_dta("data-raw/Profile2022_dashboarddataset.dta")


usethis::use_data(master_data, overwrite = TRUE)

