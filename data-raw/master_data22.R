## code to prepare `master_data22` dataset goes here
## This script needs to be run to create the datafile called
## "master_data22"

devtools::load_all()

start_time <- Sys.time() # get start time

master_data22 <- haven::read_dta("data-raw/Profile2022_dashboarddataset2.dta")


# rename factor labels ----------------------------------------------------

master_data22$c0popcat3 <- haven::labelled(
  master_data22$c0popcat3,
  labels = c("Small (<50,000)" = 1,
             "Medium (50,000-499,999)" = 2,
             "Large (500,000+)" = 3),
  label = "3 level population category, renamed by LJ"
)

# Convert to factor explicitly and preserve order
master_data22$c0popcat3 <- factor(
  haven::as_factor(master_data22$c0popcat3), # Convert to factor-compatible haven labels
  levels = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)"),
  ordered = TRUE
)





master_data22$c2q501 <- haven::labelled(master_data22$c2q501, labels = c("Combined" = 1, "Independent" = 0, "Don't Know" = 2), label = "Agency structure, renamed by LJ")
master_data22$lboh <- haven::labelled(master_data22$lboh, labels = c("None" = 0, "Advisory" = 1, "Governing" = 2), label = "Board of Health, renamed by LJ")



# creating race and ethnicity variables --------------------------------------------------
race <- master_data22 %>%
  haven::as_factor() %>%
  dplyr::select(tidyselect::starts_with("c4q27"), c4q28, nacchoid)

# race$c4q28 <- dplyr::na_if(race$c4q28, "Prefer not to answer")

race <- race %>%
  dplyr::mutate(
    # Assign a value to the race based on the checked categories
    race = dplyr::case_when(
      c4q27a == "checked" & c4q27b == "unchecked" & c4q27c == "unchecked" &
        c4q27d == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" ~ "White",

      c4q27b == "checked" & c4q27a == "unchecked" & c4q27c == "unchecked" &
        c4q27d == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" ~ "Black or African American",

      c4q27c == "checked" & c4q27a == "unchecked" & c4q27b == "unchecked" &
        c4q27d == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" ~ "American Indian or Alaska Native",

      c4q27d == "checked" & c4q27a == "unchecked" & c4q27b == "unchecked" &
        c4q27c == "unchecked" & c4q27e == "unchecked" & c4q27f == "unchecked" ~ "Asian",

      c4q27e == "checked" & c4q27a == "unchecked" & c4q27b == "unchecked" &
        c4q27c == "unchecked" & c4q27d == "unchecked" & c4q27f == "unchecked" ~ "Native Hawaiian or Other Pacific Islander",

      c4q27f == "checked"  ~ "Mixed Identity or Other Race",

      # Multiple races checked are classified as Mixed Identity/Other
      (c4q27a == "checked" | c4q27b == "checked" | c4q27c == "checked" |
         c4q27d == "checked" | c4q27e == "checked" | c4q27f == "checked") &
        (c4q27a == "checked" & (c4q27b == "checked" | c4q27c == "checked" | c4q27d == "checked" |
                                  c4q27e == "checked" | c4q27f == "checked")) ~ "Mixed Identity or Other Race",
      c4q27g == "checked" & c4q28 == "Prefer not to answer" ~ NA,
      TRUE ~ NA
    ),
    ethnicity = c4q28
  )%>%
  dplyr::select(nacchoid, race, ethnicity)


# add fips code -----------------------------------------------------------
fips <- usmap::fips_info() %>%
  dplyr::mutate(state_name = full,
                c0state = abbr) %>%
  dplyr::select(c0state, fips, state_name) %>%
  dplyr::mutate(fips = as.numeric(fips))


# join race and fips to master file ------------------------------------------------
master_data22 <- dplyr::left_join(master_data22, race, by = "nacchoid")
master_data22 <- dplyr::left_join(master_data22, fips, by = "c0state")



# add employee by pop -----------------------------------------------------
master_data22 <- master_data22 %>%
  dplyr::mutate(employee_bypop = c5q36/(totpop/10000))



# add employee categories -------------------------------------------------
master_data22 <- master_data22 %>%
  dplyr::mutate(employee_numcat = dplyr::case_when(c5q36 <= 4 ~ 1,
                                                   c5q36 >= 5 & c5q36 < 10 ~ 2,
                                                   c5q36 >= 10 & c5q36 < 25 ~ 3,
                                                   c5q36 >= 25 & c5q36 < 50 ~ 4,
                                                   c5q36 >= 50 & c5q36 < 100 ~ 5,
                                                   c5q36 >= 100 & c5q36 < 200 ~ 6,
                                                   c5q36 >= 200 & c5q36 ~ 7))


master_data22$employee_numcat <- haven::labelled(master_data22$employee_numcat,
                                               labels = c("<5" = 1, "5-9.9" = 2,
                                                          "10-24.9" = 3, "25-49.9" = 4,
                                                          "50-99.9" = 5, "100-199.9" = 6,
                                                          "200+" = 7),
                                               label = "Employee per 10,000 Population,
                                               created by LJ")


#
# # Add revenue weights -----------------------------------------------------
# revenue_args <- c("c3q17p", "c3q17c", "c3q17d", "c3q17e",
#                   "c3q17r", "c3q17i", "c3q17j", "c3q17v",
#                   "c3q17h", "c3q17m")
#
# add_revenue_weights <- master_data22
#
# for (i in revenue_args) {
#
#   add_revenue_weights <- add_revenue_weights %>%
#     haven::as_factor() %>%
#     dplyr::left_join(add_weights(i, popcat = "c0popcat7"), by = c("c0popcat7", "core_denom")) %>%
#     dplyr::rename_with(~paste0(i, "_weight"), .cols = starts_with("weights_static")) %>%
#     dplyr::select(nacchoid, c0popcat7, core_denom, "c3q17p", "c3q17c", "c3q17d", "c3q17e",
#                   "c3q17r", "c3q17i", "c3q17j", "c3q17v",
#                   "c3q17h", "c3q17m", ends_with("_weight"))
# }
#
#
#
# # calculate weighted revenue data -----------------------------------------
# for (i in revenue_args) {
#   new_weighted_val <- paste0(i, "_weighted")  # New column name
#   add_revenue_weights[[new_weighted_val]] <- add_revenue_weights[[i]] * add_revenue_weights[[paste0(i, "_weight")]]
# }
#
#
# add_revenue_weights <- add_revenue_weights %>%
#   dplyr::select(nacchoid, ends_with("_weighted"))
#
#
# # Add weights to the master file ------------------------------------------
# master_data22 <- dplyr::left_join(master_data22, add_revenue_weights, by = "nacchoid")

master_data22 <- master_data22 %>%
  dplyr::mutate(rev_total = c3q16,
                # non_weighted_revenue_total = rowSums(.[,c("c3q17p", "c3q17c", "c3q17d", "c3q17e",
                #                                        "c3q17r", "c3q17i", "c3q17j", "c3q17v",
                #                                        "c3q17h", "c3q17m")], na.rm = TRUE),
               rev_percap = c3q16/totpop)

sum(master_data22$rev_total, na.rm = TRUE) # check against NACCHO total

# master_data22$rev_total <- dplyr::mutate(dplyr::master_data22$rev_total, 0)
# # master_data22$non_rev_total <- dplyr::na_if(master_data22$non_rev_total, 0)
# master_data22$rev_percap <- dplyr::na_if(master_data22$rev_percap, 0)

# Add exp and rev categories for bar --------------------------------------

master_data22 <- master_data22 %>%
  dplyr::mutate(exp_total_numcat =
                  dplyr::case_when(c3q15 > 0 & c3q15 < 250000 ~ "Less than $250,000",
                                   c3q15 >= 250000 & c3q15 < 500000 ~ "$250,000 to $499,999",
                                   c3q15 >= 500000 & c3q15 < 750000 ~ "$500,000 to $749,999",
                                   c3q15 >= 750000 & c3q15 < 1000000 ~ "$750,000 to $999,999",
                                   c3q15 >= 1000000 & c3q15 < 2500000 ~ "$1,000,000 to $2,499,999",
                                   c3q15 >= 2500000 & c3q15 < 5000000 ~ "$2,500,000 to $5,000,000",
                                   c3q15 >= 5000000 & c3q15 < 10000000 ~ "$5,000,000 to $9,999,999",
                                   c3q15 >= 10000000 & c3q15 < 25000000 ~ "$10,000,000 to $24,999,999",
                                   c3q15 >= 25000000 ~ "$25,000,000 or more",
                                   is.na(c3q15) | c3q15 == 0 ~ "Not reported"),
                exp_total_numcat = factor(
                  exp_total_numcat,
                  levels = c(
                    "Less than $250,000",
                    "$250,000 to $499,999",
                    "$500,000 to $749,999",
                    "$750,000 to $999,999",
                    "$1,000,000 to $2,499,999",
                    "$2,500,000 to $5,000,000",
                    "$5,000,000 to $9,999,999",
                    "$10,000,000 to $24,999,999",
                    "$25,000,000 or more",
                    "Not reported"
                  ),
                  ordered = TRUE  # Ensure the factor is ordinal
                )) %>%
  dplyr::mutate(exp_percap_numcat =
                  dplyr::case_when(totexp_percap > 0 & totexp_percap < 10 ~ "Less than $10",
                                   totexp_percap >= 10 & totexp_percap < 25 ~ "$10 to $24",
                                   totexp_percap >= 25 & totexp_percap < 50 ~ "$24 to $50",
                                   totexp_percap >= 50 & totexp_percap < 75 ~ "$50 to $74",
                                   totexp_percap >= 75 & totexp_percap < 100 ~ "$75 to $100",
                                   totexp_percap >= 100 & totexp_percap < 150 ~ "$100 to $150",
                                   totexp_percap >= 150 ~ "$150 or more")) %>%
  dplyr::mutate(rev_total_numcat =
                  dplyr::case_when(rev_total > 0 & rev_total < 250000 ~ "Less than $250,000",
                                   rev_total >= 250000 & rev_total < 500000 ~ "$250,000 to $499,999",
                                   rev_total >= 500000 & rev_total < 750000 ~ "$500,000 to $749,999",
                                   rev_total >= 750000 & rev_total < 1000000 ~ "$750,000 to $999,999",
                                   rev_total >= 1000000 & rev_total < 2500000 ~ "$1,000,000 to $2,499,999",
                                   rev_total >= 2500000 & rev_total < 5000000 ~ "$2,500,000 to $5,000,000",
                                   rev_total >= 5000000 & rev_total < 10000000 ~ "$5,000,000 to $9,999,999",
                                   rev_total >= 10000000 & rev_total < 25000000 ~ "$10,000,000 to $24,999,999",
                                   rev_total >= 25000000 ~ "$25,000,000 or more",
                                   is.na(rev_total) ~ "Not reported")) %>%
  dplyr::mutate(rev_percap_numcat =
                  dplyr::case_when(rev_percap > 0 & rev_percap < 10 ~ "Less than $10",
                                   rev_percap >= 10 & rev_percap < 25 ~ "$10 to $24",
                                    rev_percap >= 25 & rev_percap < 50 ~ "$24 to $50",
                                    rev_percap >= 50 & rev_percap < 75 ~ "$50 to $74",
                                    rev_percap >= 75 & rev_percap < 100 ~ "$75 to $100",
                                    rev_percap >= 100 & rev_percap < 150 ~ "$100 to $150",
                                    rev_percap >= 150 ~ "$150 or more")) %>%

  dplyr::mutate(tenure_numcat = dplyr::case_when(tenure < 2 ~ 1,
                                                 tenure >= 2 & tenure < 6 ~ 2,
                                                 tenure >= 6 & tenure < 11 ~ 3,
                                                 tenure >= 11 ~ 4 )) %>%

  dplyr::mutate(exp_total_numcat = haven::as_factor(exp_total_numcat),
                exp_percap_numcat = haven::as_factor(exp_percap_numcat),
                rev_total_numcat = haven::as_factor(rev_total_numcat),
                rev_percap_numcat = haven::as_factor(rev_percap_numcat)
                # ,
                # tenure_numcat = haven::as_factor(tenure_numcat)
                )



master_data22$exp_total_numcat <- haven::labelled(
  master_data22$exp_total_numcat,
  labels = c(
          "Less than $250,000" = 1,
          "$250,000 to $499,999" = 2,
          "$500,000 to $749,999" = 3,
          "$750,000 to $999,999" = 4,
          "$1,000,000 to $2,499,999" = 5,
          "$2,500,000 to $5,000,000" = 6,
          "$5,000,000 to $9,999,999" = 7,
          "$10,000,000 to $24,999,999" = 8,
          "$25,000,000 or more" = 9,
          "Not reported" = 10
             ),
  label = "Total expenditures, by group, created by MA"
)


master_data22$rev_total_numcat <- haven::labelled(
  master_data22$rev_total_numcat,
  labels = c(
    "Less than $250,000" = 1,
    "$250,000 to $499,999" = 2,
    "$500,000 to $749,999" = 3,
    "$750,000 to $999,999" = 4,
    "$1,000,000 to $2,499,999" = 5,
    "$2,500,000 to $5,000,000" = 6,
    "$5,000,000 to $9,999,999" = 7,
    "$10,000,000 to $24,999,999" = 8,
    "$25,000,000 or more" = 9,
    "Not reported" = 10

  ),
  label = "Total revenue, by group, created by MA"
)


master_data22$exp_percap_numcat <- haven::labelled(
  master_data22$exp_percap_numcat,
  labels = c("Less than $10" = 1,
             "$10 to $24" = 2,
             "$24 to $50" = 3,
             "$50 to $74" = 4,
             "$75 to $100" = 5,
             "$100 to $150" = 6,
             "$150 or more" = 7
  ),
  label = "Per capita expenditures, by group, created by LJ"
)


master_data22$rev_percap_numcat <- haven::labelled(
  master_data22$rev_percap_numcat,
  labels = c("Less than $10" = 1,
             "$10 to $24" = 2,
             "$24 to $50" = 3,
             "$50 to $74" = 4,
             "$75 to $100" = 5,
             "$100 to $150" = 6,
             "$150 or more" = 7
  ),
  label = "Per capita revenue, by group, created by LJ"
)

master_data22$tenure_numcat <- haven::labelled(
  master_data22$tenure_numcat,
  labels = c(
    "Less than 2 years" = 1,
    "2 - 5 years" = 2,
    "6 - 10 years" = 3,
    "11 or more years" = 4
  )
)



master_data22 <- master_data22 %>%
  dplyr::mutate(spec_combined = case_when(
    c4q502a == 1 | c4q502b == 1 | c4q502c == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  # Apply labels to the new variable
  dplyr::mutate(spec_combined = haven::labelled(spec_combined, c("unchecked" = 0, "checked" = 1)))




# newc12q502_per_id <- master_data %>%
#   group_by(nacchoid) %>%
#   summarise(
#     newc12q502 = case_when(
#       # If any column is 1, set to 1
#       any(c_across(c(newc12q502x, newc12q502aa, newc12q502b, newc12q502ab, newc12q502o, newc12q502i,
#                      newc12q502y, newc12q502ac, newc12q502r, newc12q502m, newc12q502s, newc12q502q,
#                      newc12q502j, newc12q502l, newc12q502p, newc12q502t, newc12q502k, newc12q502v,
#                      newc12q502z, newc12q502f)) == 1) ~ 1,

# # If all columns are NA, set to NA
# all(is.na(newc12q502x), is.na(newc12q502aa), is.na(newc12q502b),
#     is.na(newc12q502ab), is.na(newc12q502o), is.na(newc12q502i),
#     is.na(newc12q502y), is.na(newc12q502ac), is.na(newc12q502r),
#     is.na(newc12q502m), is.na(newc12q502s), is.na(newc12q502q),
#     is.na(newc12q502j), is.na(newc12q502l), is.na(newc12q502p),
#     is.na(newc12q502t), is.na(newc12q502k), is.na(newc12q502v),
#     is.na(newc12q502z), is.na(newc12q502f)) ~ NA_real_,

#     # If all columns are 0 or NA, set to 0
#     all(c_across(c(newc12q502x, newc12q502aa, newc12q502b, newc12q502ab, newc12q502o, newc12q502i,
#                    newc12q502y, newc12q502ac, newc12q502r, newc12q502m, newc12q502s, newc12q502q,
#                    newc12q502j, newc12q502l, newc12q502p, newc12q502t, newc12q502k, newc12q502v,
#                    newc12q502z, newc12q502f)) == 0) ~ 0,
#
#     TRUE ~ 0
#   )
# ) %>%
# ungroup() %>%
# mutate(newc12q502 = haven::labelled(newc12q502, c("unchecked" = 0, "checked" = 1)))

# Step 2: Join the result back to the original master_data22
# This section is disabled because newc12q502_per_id
# is not currently being created.

# master_data22 <- master_data22 %>%
#   dplyr::left_join(newc12q502_per_id, by = "nacchoid")


# print elapsed time
end_time <- Sys.time() - start_time # calculate difference
print(end_time) # 6 seconds



# save data ---------------------------------------------------------------
usethis::use_data(master_data22, overwrite = TRUE)
devtools::document()



# ONLY USE BELOW IF...
# you want to display race categories differently
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
