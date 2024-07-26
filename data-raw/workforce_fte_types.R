## code to prepare `workforce_fte_types` dataset goes here

workforce_fte_types <- list("Total FTEs" = "c5q37",
                        "Per Capita FTEs" = "fte_bypop",
                        "FTE Grouping" = "FTE_numcat")


usethis::use_data(workforce_fte_types, overwrite = TRUE)
