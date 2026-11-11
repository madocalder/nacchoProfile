## code to prepare `workforce_classifications` dataset goes here

workforce_classifications <- list("Agency leadership" = "c5q63",
                                  "Animal control worker" = "c5q62",
                                  "Behavioral health staff" = "c5q52",
                                  "Business and financial operations staff" = "c5q64",
                                  "Community health worker" = "c5q61",
                                  "Disease intervention specialist" = "c5q68",
                                  "Environmental health worker" = "c5q55",
                                  "Epidemiologist or statistician" = "c5q47",
                                  "Health educator" = "c5q48",
                                  "Information systems specialist" = "c5q50",
                                  "Laboratory worker" = "c5q60",
                                  "Licensed practical or vocational nurse" = "c5q57",
                                  "Nursing aide and home health aide" = "c5q58",
                                  "Nutritionist" = "c5q49",
                                  "Office and administrative support staff" = "c5q65",
                                  "Oral health care professional" = "c5q59",
                                  "Preparedness staff" = "c5q56",
                                  "Public health physician" = "c5q44",
                                  "Public information professional" = "c5q51",
                                  "Registered nurse" = "c5q43")


usethis::use_data(workforce_classifications, overwrite = TRUE)
