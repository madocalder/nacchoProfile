## code to prepare `service_topics` dataset goes here
service_topics <- list(
  "Services Provided By..." = list("LHD (Directly)",
                             "LHD (By Contract)",
                             "Other Entity",
                             "Not Available in Community"),

  "Service Changes" = list("Reduced/Expanded Services")
)

usethis::use_data(service_topics, overwrite = TRUE)
