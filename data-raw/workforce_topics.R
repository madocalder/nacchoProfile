## code to prepare `workforce_topics` dataset goes here
workforce_topics <- list(
  "Full-Time Equivalents" = list("Total FTEs" = "c5q37",
                                 "FTEs by Population" = "fte_bypop",
                                 "FTE Groups" = "FTE_numcat"),

  "Employees" = list("Total Employees" = "c5q36",
                     "Employees by Population" = "employee_bypop",
                     "Employee Groups" = "employee_numcat"),

  "Jobs Lost and Added" = list("Total Jobs Lost/Added" = "jobs_total",
                               "Jobs Lost/Added Per 100" = "jobs_standard"),

  "Occupation Classifications" = list("LHDs with Occupation" = "a",
                                   "Total FTEs in Occupation" = "b")
)


usethis::use_data(workforce_topics, overwrite = TRUE)
