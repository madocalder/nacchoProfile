## code to prepare `notes` dataset goes here

notes <- readxl::read_excel("data-raw/notes.xlsx")

usethis::use_data(notes, overwrite = TRUE)
devtools::document()
