## code to prepare `finance_revenue_types` dataset goes here
finance_revenue_types <- list("Total Revenues" = "non_weighted_revenue_total",
                              "Per Capita Revenues" = "non_weighted_revenue_percap",
                              "Grouped Total Revenue" = "rev_total_numcat",
                              "Grouped Per Capita Revenue" = "rev_percap_numcat")

usethis::use_data(finance_revenue_types, overwrite = TRUE)
devtools::document()
