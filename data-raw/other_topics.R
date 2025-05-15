#' Prepare Other Topics Dataset
#'
#' This script creates a dataset called `other_topics`, which organizes various public health-related topics
#' and their corresponding survey sections. The dataset groups topics into broader categories such as
#' "Informatics", "Access to Health Care", "Policy & Advocacy", and "Planning & QI", each containing related subtopics.
#'
#' @details
#' The `other_topics` dataset categorizes survey topics into several sections:
#' - **Informatics**: Contains data on "Technology Use" and "Communication Channels".
#' - **Access to Health Care**: Includes "Medical Services", "Dental Services", and "Behavioral Services".
#' - **Policy & Advocacy**: Covers "Advocacy by Topic Area" and "New or Revised Ordinances".
#' - **Planning & QI**: Addresses "CHA and CHIP" and "CHIP Activities".
#'
#' Each subtopic is mapped to a specific survey section for easy reference.
#'
#' @return Saves the `other_topics` dataset as an `.rda` file in the package.
#'
#' @examples
#' # Prepare and save the `other_topics` dataset
#' other_topics <- list(
#'   "Informatics" = list("Technology Use" = "tech_use",
#'                        "Communication Channels" = "comm_channels"),
#'
#'   "Access to Health Care" = list("Medical Services" = "medical",
#'                                  "Dental Services" = "dental",
#'                                  "Behavioral Services" = "behavioral"),
#'
#'   "Policy & Advocacy" = list("Advocacy by Topic Area" = "c12q260",
#'                              "New or Revised Ordinances" = "Newc12q502"),
#'
#'   "Planning & QI" = list("CHA and CHIP" = "chachip",
#'                          "CHIP Activities" = "m9q240")
#' )
#'
#' @export
NULL

# Code to prepare `other_topics` dataset ----
other_topics <- list(
  "Informatics" = list("Technology Use" = "tech_use"),

  "Communication Channel Use" = list(
                       "Emergency Response" = "comm_channels_1",
                       "General Announcement" = "comm_channels_2",
                       "Any Use" = "comm_channels_3"),

  "Access to Health Care" = list("Medical Services" = "medical",
                                 "Dental Services" = "dental",
                                 "Behavioral Services" = "behavioral"),

  "Policy & Advocacy" = list("Advocacy by Topic Area" = "c12q260",
                             "New or Revised Ordinances" = "Newc12q502"),

  "Planning & QI" = list("CHA" = "c7q147",
                         "CHIP" = "c7q149",
                         "CHA and CHIP" = "chachip",
                         "CHIP Activities" = "m9q240"),

  "Partnerships" = list(
                        "Formal" = "formal_part",
                        "Any" = "any_part"
                        )
)

# Save the other_topics dataset
usethis::use_data(other_topics, overwrite = TRUE)

