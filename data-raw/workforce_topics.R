## code to prepare `workforce_topics` dataset goes here
workforce_topics <- list(
  "Full-Time Equivalents" = list("Mean/Median FTEs" = "c5q37",
                                 "FTEs by Population" = "fte_bypop",
                                 "Total FTEs" = "FTE_numcat"),

  "Employees" = list("Mean/Median Employees" = "c5q36",
                     "Employees by Population" = "employee_bypop",
                     "Total Employees" = "employee_numcat"),

  "Jobs Lost and Added" = list("Total Jobs Lost/Added" = "jobs_total"),

  "Occupation Classifications" = list("LHDs with Occupation" = "a",
                                   "Total FTEs in Occupation" = "b")
)


usethis::use_data(workforce_topics, overwrite = TRUE)
