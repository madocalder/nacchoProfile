## code to prepare `workforce_employee_types` dataset goes here

workforce_employee_types <- list("Total Employees" = "c5q36",
                                 "Per Capita Employees" = "employee_bypop",
                                 "Employee Grouping" = "employee_numcat")

usethis::use_data(workforce_employee_types, overwrite = TRUE)
