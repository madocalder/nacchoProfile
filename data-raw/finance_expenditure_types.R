## code to prepare `finance_expenditure_types` dataset goes here
finance_expenditure_types <- list("Total Expenditures" = "c3q15",
                         "Per Capita Expenditures" = "totexp_percap",
                         "Expenditure Grouping" = "exp_numcat"
)



usethis::use_data(finance_expenditure_types, overwrite = TRUE)
