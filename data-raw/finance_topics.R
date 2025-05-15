## code to prepare `finance_topics` dataset goes here
finance_topics <- list(
  "Expenditures" = list( "Total Annual Expenditures"  = "exp_total_numcat",
                         "Mean/Median Expenditures" = "c3q15",
                        "Per Capita Expenditures" = "totexp_percap"

                        # ,
                        # "Grouped Per Capita Exp." = "exp_percap_numcat"
                        ),

  "Revenues" = list("Total Annual Revenues"  = "rev_total_tree",
                    "Mean/Median Revenues" = "rev_total",
                    "Per Capita Revenues" = "rev_percap"
                    # ,
                    # "Grouped Total Revenue" = "rev_total_numcat",
                    # "Grouped Per Capita Revenue" = "rev_percap_numcat"
                    ),

  "Budget Changes" = list("Budget Changes" = "budget_changes")
)


usethis::use_data(finance_topics, overwrite = TRUE)
