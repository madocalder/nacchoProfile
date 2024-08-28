## code to prepare `service_topics` dataset goes here
service_topics <- c(
  "Service Provision",
  "Service Changes"
)

usethis::use_data(service_topics, overwrite = TRUE)
