## code to prepare `other_topics` dataset goes here
other_topics <- list(
  "Informatics" = list("Technology Use" = "tech_use",
                       "Communication Channels" = "comm_channels"),

  "Access to Health Care" = list("Medical Services" = "medical",
                                 "Dental Services" = "dental",
                                 "Behavioral Services" = "behavioral"),

  "Policy & Advocacy" = list("Advocacy by Topic Area" = "c12q260",
                             "New or Revised Ordinances" = "Newc12q502"),

  "Planning & QI" = list("CHA and CHIP" = "chachip",
                         "CHIP Activities" = "m9q240")
)


usethis::use_data(other_topics, overwrite = TRUE)
