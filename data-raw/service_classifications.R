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


service_cats <- list(
  "Blood Lead Screening",
  "Chronic Disease",
  "Communicable Disease Screening or Treatment",
  "Diabetes Screening",
  "Emergency Preparedness",
  "Environmental Health Including Food Safety",
  "Epidemiology and Surveillance",
  "High Blood Pressure Screening",
  "Immunization",
  "Maternal and Child Health Services",
  "Mental Health",
  "Obesity Prevention",
  "Substance Use Prevention"
)

usethis::use_data(service_cats, overwrite = TRUE)
