## code to prepare `finance_topics` dataset goes here
finance_topics <- list("Expenditures" = "expenditures",
                         "Revenues" = "revenues",
                         "Budget Changes" = "budget_changes"
)


usethis::use_data(finance_topics, overwrite = TRUE)
