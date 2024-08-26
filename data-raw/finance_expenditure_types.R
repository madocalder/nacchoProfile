## code to prepare `finance_expenditure_types` dataset goes here
finance_expenditure_types <- list("Total Exp." = "c3q15",
                         "Per Capita Exp." = "totexp_percap",
                         "Grouped Total Exp." = "exp_total_numcat",
                         "Grouped Per Capita Exp." = "exp_percap_numcat")



usethis::use_data(finance_expenditure_types, overwrite = TRUE)
devtools::document()
