## code to prepare `finance_revenue_types` dataset goes here
finance_revenue_types <- list("Total Revenues" = "revenue_total",
                              "Per Capita Revenues" = "_percap",
                              "Revenue Grouping" = "revenue_numcat")

usethis::use_data(finance_revenue_types, overwrite = TRUE)
