## code to prepare `service_classifications` dataset goes here
service_classifications <- list(
  "Immunization",
  "Disease/Risk Factor Screening",
  "Communicable Disease Treatment",
  "Maternal and Child Health",
  "Other Clinical Services",
  "Epidemiology and Surveillance",
  "Population-Based Prevention",
  "Regulation, Inspection, Licensing",
  "Other Environmental Health",
  "Other Population-Based Services"
)


usethis::use_data(service_classifications, overwrite = TRUE)
