## code to prepare `master_data` dataset goes here

master_data <- haven::read_dta("data-raw/Profile2022_dashboarddataset.dta")


# rename factor labels ----------------------------------------------------
master_data$c0popcat3 <- haven::labelled(master_data$c0popcat3, labels = c("Small (<50,000)" = 1, "Medium (50,000-100,000)" = 2, "Large (500.000+)" = 3), label = "3 level population category, renamed by LJ")
master_data$c2q501 <- haven::labelled(master_data$c2q501, labels = c("Combined" = 1, "Independent" = 0, "Don't Know" = 2), label = "Agency structure, renamed by LJ")
master_data$lboh <- haven::labelled(master_data$lboh, labels = c("None" = 0, "Advising" = 1, "Governing" = 2), label = "Board of Health, renamed by LJ")



# creating race variable --------------------------------------------------
race <- master_data %>%
  haven::as_factor() %>%
  dplyr::select(tidyselect::starts_with("c4q27"), c4q28, nacchoid)

race$c4q28 <- dplyr::na_if(race$c4q28, "Prefer not to answer")

race <- race %>%
  dplyr::filter(c4q27g != "checked" & c4q28 != "Hispanic or Latino") %>%
  dplyr::mutate(white = dplyr::case_when(c4q27a == "checked" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "White",
                                         TRUE ~ "Other")) %>%
  dplyr::mutate(race = dplyr::case_when(white == "White" ~ "White Alone",
                                        white == "Other" & c4q27c == "checked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Mixed Identity/Other",
                                        white == "Other" & c4q27c == "unchecked" & c4q27d == "checked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Mixed Identity/Other",
                                        white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "checked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Black or African American Alone",
                                        white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "checked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Mixed Identity/Other",
                                        white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "checked" & c4q28 == "not Hispanic/Latino" ~ "Mixed Identity/Other",
                                        white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "Hispanic/Latino" ~ "Hispanic or Latino Alone",
                                        TRUE ~ "Mixed Identity/Other")) %>%
  dplyr::select(nacchoid, race, white)



# add fips code -----------------------------------------------------------
fips <- usmap::fips_info() %>%
  dplyr::mutate(state_name = full,
                c0state = abbr) %>%
  dplyr::select(c0state, fips, state_name) %>%
  dplyr::mutate(fips = as.numeric(fips))


# join race and fips to master file ------------------------------------------------
master_data <- dplyr::left_join(master_data, race, by = "nacchoid")
master_data <- dplyr::left_join(master_data, fips, by = "c0state")


# save data ---------------------------------------------------------------
usethis::use_data(master_data, overwrite = TRUE)
devtools::document()


# race <- race %>%
#   dplyr::filter(c4q27g != "checked" & c4q28 != "Hispanic or Latino") %>%
#   dplyr::mutate(white = dplyr::case_when(c4q27a == "checked" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "white",
#                                          TRUE ~ "Other")) %>%
#   dplyr::mutate(race = dplyr::case_when(white == "white" ~ "white",
#                                         white == "Other" & c4q27c == "checked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "AIAN",
#                                         white == "Other" & c4q27c == "unchecked" & c4q27d == "checked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Asian",
#                                         white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "checked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Black or African American",
#                                         white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "checked" & c4q27f == "unchecked" & c4q28 == "not Hispanic/Latino" ~ "Native Hawaiin or Pacific Islander",
#                                         white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "checked" & c4q28 == "not Hispanic/Latino" ~ "Other",
#                                         white == "Other" & c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27b == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" & c4q28 == "Hispanic/Latino" ~ "Hispanic or Latino",
#                                         TRUE ~ "Mixed Race/Ethnicity")) %>%
#   dplyr::select(nacchoid, race, white)
