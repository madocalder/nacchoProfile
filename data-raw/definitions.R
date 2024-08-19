## code to prepare `definitions` dataset goes here

definitions <- readxl::read_excel("data-raw/definitions.xlsx")

usethis::use_data(definitions, overwrite = TRUE)

