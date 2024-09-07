## code to prepare `change_classifications` dataset goes here

change_classifications <- list(
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

usethis::use_data(change_classifications, overwrite = TRUE)
