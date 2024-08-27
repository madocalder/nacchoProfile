## code to prepare `finance_topics` dataset goes here
finance_topics <- list(
  "Expenditures" = list("Total Exp." = "c3q15",
                        "Per Capita Exp." = "totexp_percap",
                        "Grouped Total Exp." = "exp_total_numcat",
                        "Grouped Per Capita Exp." = "exp_percap_numcat"),

  "Revenues" = list("Total Revenues" = "non_weighted_revenue_total",
                    "Per Capita Revenues" = "non_weighted_revenue_percap",
                    "Grouped Total Revenue" = "rev_total_numcat",
                    "Grouped Per Capita Revenue" = "rev_percap_numcat"),

  "Budget Changes" = list("Budget Changes" = "budget_changes")
)


usethis::use_data(finance_topics, overwrite = TRUE)
