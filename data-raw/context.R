## code to prepare `context` dataset goes here

context <- readxl::read_excel("data-raw/naccho_definitions.xlsx")

usethis::use_data(context, overwrite = TRUE)
