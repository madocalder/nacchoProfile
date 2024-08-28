## code to prepare `services_data` dataset goes here
devtools::load_all()
start_time <- Sys.time() # get start time


# Get Vars ----------------------------------------------------------------
data <- master_data %>%
  dplyr::select(dplyr::starts_with("c6q"))

vars <- colnames(data)



# With Filter -------------------------------------------------------------
filters <- c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")

services_filter_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(filters)) {
  args <- list(vars, filters[[i]])

  filtered_data <- pmap_dfr(args, return_survey_weighted_counts) %>%
    dplyr::mutate(filter = paste(filters[i])) %>%
    dplyr::select(-c(13, 14))

  filtered_data <- data %>%
    dplyr::select(1:12, filter)

  services_filter_list[[i]] <- filtered_data
}



# bind the data generated in loop
services_with_filter <- do.call(rbind, services_filter_list) %>%
  dplyr::select(1:13, 15)


# No Filter ----------------------------------------------------------------
services_no_filter <- pmap_dfr(vars, return_survey_weighted_counts) %>%
  dplyr::mutate(filter = "All LHDs")


# Join Data ---------------------------------------------------------------
# join the two datasets -- with filters and with no filter
services_data <- rbind(services_with_filter, services_no_filter)




# Clean -------------------------------------------------------------------
services_data <- services_data %>%
  dplyr::mutate(service_category = dplyr::case_when(Question %in% c("c6q55a", "c6q55b", "c6q55i", "c6q55f", "c6q55g", "c6q56a", "c6q56b", "c6q56i",
                                                                    "c6q56f", "c6q56g", "c6q147a", "c6q147b", "c6q147i", "c6q147f", "c6q147g",
                                                                    "c6q148a", "c6q148b", "c6q148i", "c6q148f", "c6q148g") ~ "Immunization",
                                                    Question %in% c("c6q142a", "c6q142b", "c6q142i", "c6q142f", "c6q142g",
                                                                    "c6q60a", "c6q60b", "c6q60i", "c6q60f", "c6q60g",
                                                                    "c6q61a", "c6q61b", "c6q61i", "c6q61f", "c6q61g",
                                                                    "c6q62a", "c6q62b", "c6q62i", "c6q62f", "c6q62g",
                                                                    "c6q63a", "c6q63b", "c6q63i", "c6q63f", "c6q63g",
                                                                    "c6q57a", "c6q57b", "c6q57i", "c6q57f", "c6q57g",
                                                                    "c6q58a", "c6q58b", "c6q58i", "c6q58f", "c6q58g",
                                                                    "c6q59a", "c6q59b", "c6q59i", "c6q59f", "c6q59g") ~ "Disease/Risk Factor Screening",
                                                    Question %in% c("c6q65a", "c6q65b", "c6q65i", "c6q65f", "c6q65g", "c6q66a", "c6q66b",
                                                                    "c6q66i", "c6q66f", "c6q66g", "c6q67a", "c6q67b", "c6q67i", "c6q67f",
                                                                    "c6q67g") ~ "Communicable Disease Treatment",
                                                    Question %in% c("c6q73a","c6q73b","c6q73i","c6q73f","c6q73g","c6q69a","c6q69b",
                                                                    "c6q69i","c6q69f","c6q69g","c6q74a","c6q74b","c6q74i","c6q74f",
                                                                    "c6q74g", "c6q71a", "c6q71b", "c6q71i", "c6q71f", "c6q71g") ~ "Maternal and Child Health",
                                                    Question %in% c("c6q78a", "c6q78b", "c6q78i", "c6q78f", "c6q78g", "c6q75a", "c6q75b", "c6q75i", "c6q75f", "c6q75g", "c6q76a", "c6q76b", "c6q76i", "c6q76f", "c6q76g",
                                                                    "c6q77a", "c6q77b", "c6q77i", "c6q77f", "c6q77g", "c6q79a", "c6q79b", "c6q79i", "c6q79f", "c6q79g") ~ "Other Clinical Services",
                                                    Question %in% c("c6q83a", "c6q83b", "c6q83i", "c6q83f", "c6q83g", "c6q81a", "c6q81b", "c6q81i", "c6q81f", "c6q81g", "c6q80a", "c6q80b", "c6q80i", "c6q80f", "c6q80g",
                                                                    "c6q82a", "c6q82b", "c6q82i", "c6q82f", "c6q82g", "c6q84a", "c6q84b", "c6q84i", "c6q84f", "c6q84g", "c6q86a") ~ "Epidemiology and Surveillance",
                                                    Question %in% c("c6q89a",  "c6q89b",  "c6q89i",  "c6q89f",  "c6q89g",  "c6q87a",  "c6q87b",  "c6q87i",  "c6q87f",  "c6q87g",  "c6q95a",  "c6q95b",  "c6q95i",
                                                                    "c6q95f",  "c6q95g",  "c6q90a",  "c6q90b",  "c6q90i",  "c6q90f",  "c6q90g",  "c6q145a", "c6q145b", "c6q145i", "c6q145f", "c6q145g", "c6q91a",
                                                                    "c6q91b",  "c6q91i",  "c6q91f",  "c6q91g",  "c6q146a", "c6q146b", "c6q146i", "c6q146f", "c6q146g", "c6q93a",  "c6q93b",  "c6q93i",  "c6q93f",
                                                                    "c6q93g") ~ "Population-Based Prevention",
                                                    Question %in% c("c6q105a", "c6q105b", "c6q105i", "c6q105f", "c6q105g", "c6q97a",  "c6q97b",  "c6q97i",  "c6q97f",  "c6q97g",  "c6q103a", "c6q103b", "c6q103i",
                                                                    "c6q103f", "c6q103g", "c6q110a", "c6q110b", "c6q110i", "c6q110f", "c6q110g", "c6q114a", "c6q114b", "c6q114i", "c6q114f", "c6q114g", "c6q115a",
                                                                    "c6q115b", "c6q115i", "c6q115f", "c6q115g", "c6q101a", "c6q101b", "c6q101i", "c6q101f", "c6q101g", "c6q116a", "c6q116b", "c6q116i", "c6q116f",
                                                                    "c6q116g", "c6q10a",  "c6q109b", "c6q109i", "c6q109f", "c6q109g", "c6q111a", "c6q111b", "c6q111i", "c6q111f", "c6q111g", "c6q113a", "c6q113b",
                                                                    "c6q113i", "c6q113f", "c6q113g", "c6q112a", "c6q112b", "c6q112i", "c6q112f", "c6q112g", "c6q143a", "c6q143b", "c6q143i", "c6q143f", "c6q143g",
                                                                    "c6q102a", "c6q102b", "c6q102i", "c6q102f", "c6q102g", "c6q100a", "c6q100b", "c6q100i", "c6q100f", "c6q100g", "c6q107a", "c6q107b", "c6q107i",
                                                                    "c6q107f", "c6q107g")
                                                    ~ "Regulation, Inspection, Licensing",
                                                    Question %in% c("c6q127a", "c6q127b", "c6q127i", "c6q127f", "c6q127g", "c6q118a", "c6q118b", "c6q118i", "c6q118f", "c6q118g", "c6q124a", "c6q124b", "c6q124i",
                                                                    "c6q124f", "c6q124g", "c6q117a", "c6q117b", "c6q117i", "c6q117f", "c6q117g", "c6q121a", "c6q121b", "c6q121i", "c6q121f", "c6q121g", "c6q128a",
                                                                    "c6q128b", "c6q128i", "c6q128f", "c6q128g", "c6q144a", "c6q144b", "c6q144i", "c6q144f", "c6q144g", "c6q119a", "c6q119b", "c6q119i", "c6q119f",
                                                                    "c6q119g", "c6q120a", "c6q120b", "c6q120i", "c6q120f", "c6q120g") ~ "Other Environmental Health",
                                                    Question %in% c("c6q131a", "c6q131b", "c6q131i", "c6q131f", "c6q131g", "c6q130a", "c6q130b", "c6q130i", "c6q130f", "c6q130g", "c6q134a", "c6q134b", "c6q134i",
                                                                    "c6q134f", "c6q134g", "c6q137a", "c6q137b", "c6q137i", "c6q137f", "c6q137g", "c6q136a", "c6q136b", "c6q136i", "c6q136f", "c6q136g") ~ "Other Population-Based Services"))

services_data <- services_data %>%
  dplyr::mutate(service_type = dplyr::case_when(stringr::str_detect(Question, "c6q55") ~ "Routine Adult Immunizations",
                                                stringr::str_detect(Question, "c6q56") ~ "Routine Childhood Immunizations",
                                                stringr::str_detect(Question, "c6q147") ~ "COVID-19 Adult Immunizations",
                                                stringr::str_detect(Question, "c6q148") ~ "COVID-19 Child Immunizations",
                                                stringr::str_detect(Question, "c6q142") ~ "BMI",
                                                stringr::str_detect(Question, "c6q60") ~ "Cancer",
                                                stringr::str_detect(Question, "c6q61") ~ "Cardiovascular Disease",
                                                stringr::str_detect(Question, "c6q62") ~ "Diabetes",
                                                stringr::str_detect(Question, "c6q63") ~ "High Blood Pressure",
                                                stringr::str_detect(Question, "c6q57") ~ "HIV/AIDs",
                                                stringr::str_detect(Question, "c6q58") ~ "Other STDs",
                                                stringr::str_detect(Question, "c6q59") ~ "Tuberculosis",
                                                stringr::str_detect(Question, "c6q65") ~ "HIV/AIDs",
                                                stringr::str_detect(Question, "c6q66") ~ "Other STDs",
                                                stringr::str_detect(Question, "c6q67") ~ "Tuberculosis",
                                                stringr::str_detect(Question, "c6q73") ~ "EPSDT",
                                                stringr::str_detect(Question, "c6q69") ~ "Prenatal Care",
                                                stringr::str_detect(Question, "c6q74") ~ "Well-Child Clinic",
                                                stringr::str_detect(Question, "c6q71") ~ "WIC",
                                                stringr::str_detect(Question, "c6q78") ~ "Behavioral/Mental Health Services",
                                                stringr::str_detect(Question, "c6q75") ~ "Comprehensive Primary Care",
                                                stringr::str_detect(Question, "c6q76") ~ "Home Health Care",
                                                stringr::str_detect(Question, "c6q77") ~ "Oral Health",
                                                stringr::str_detect(Question, "c6q79") ~ "Substance Abuse Services",
                                                stringr::str_detect(Question, "c6q83") ~ "Behavioral Risk Factors",
                                                stringr::str_detect(Question, "c6q81") ~ "Chronic Disease",
                                                stringr::str_detect(Question, "c6q80") ~ "Communicable Infectious Disease",
                                                stringr::str_detect(Question, "c6q82") ~ "Injury",
                                                stringr::str_detect(Question, "c6q84") ~ "Environmental Health",
                                                stringr::str_detect(Question, "c6q86") ~ "Maternal and Child Health",
                                                stringr::str_detect(Question, "c6q85") ~ "Syndromic Surveillance",
                                                stringr::str_detect(Question, "c6q89") ~ "Chronic Disease Programs",
                                                stringr::str_detect(Question, "c6q87") ~ "Injury",
                                                stringr::str_detect(Question, "c6q95") ~ "Mental Illness",
                                                stringr::str_detect(Question, "c6q90a") ~ "Nutrition",
                                                stringr::str_detect(Question, "c6q145") ~ "Opioids",
                                                stringr::str_detect(Question, "c6q91") ~ "Physical Activity",
                                                stringr::str_detect(Question, "c6q146") ~ "Substance Abuse Other Than Opioids",
                                                stringr::str_detect(Question, "c6q93") ~ "Tobacco",
                                                stringr::str_detect(Question, "c6q105") ~ "Body Art (Tattoos, Piercings)",
                                                stringr::str_detect(Question, "c6q97") ~ "Campgrounds and RVs",
                                                stringr::str_detect(Question, "c6q103") ~ "Children's Camps",
                                                stringr::str_detect(Question, "c6q110") ~ "Food Processing",
                                                stringr::str_detect(Question, "c6q114") ~ "Food Service Establishments",
                                                stringr::str_detect(Question, "c6q115") ~ "Health-Related Facilities",
                                                stringr::str_detect(Question, "c6q101") ~ "Hotels and Motels",
                                                stringr::str_detect(Question, "c6q116") ~ "Housing",
                                                stringr::str_detect(Question, "c6q109") ~ "Lead Inspections",
                                                stringr::str_detect(Question, "c6q111") ~ "Milk Processing",
                                                stringr::str_detect(Question, "c6q113") ~ "Private Drinking Water",
                                                stringr::str_detect(Question, "c6q112") ~ "Public Drinking Water",
                                                stringr::str_detect(Question, "c6q143") ~ "Recreational Water (Pools, Lakes)",
                                                stringr::str_detect(Question, "c6q102") ~ "Schools or Daycares",
                                                stringr::str_detect(Question, "c6q100") ~ "Septic Systems",
                                                stringr::str_detect(Question, "c6q107") ~ "Tobacco Retailers",
                                                stringr::str_detect(Question, "c6q127") ~ "Air Pollution",
                                                stringr::str_detect(Question, "c6q118") ~ "Food Safety Education",
                                                stringr::str_detect(Question, "c6q124") ~ "Hazmat Response",
                                                stringr::str_detect(Question, "c6q117") ~ "Indoor Air Quality",
                                                stringr::str_detect(Question, "c6q121") ~ "Land Use Planning",
                                                stringr::str_detect(Question, "c6q128") ~ "Noise Pollution",
                                                stringr::str_detect(Question, "c6q144") ~ "Public Health Nuisance Abatement",
                                                stringr::str_detect(Question, "c6q119") ~ "Radiation Control",
                                                stringr::str_detect(Question, "c6q120") ~ "Vector Control",
                                                stringr::str_detect(Question, "c6q131") ~ "Animal Control",
                                                stringr::str_detect(Question, "c6q130") ~ "Emergency Medical Services",
                                                stringr::str_detect(Question, "c6q134") ~ "Laboratory Services",
                                                stringr::str_detect(Question, "c6q137") ~ "School Health",
                                                stringr::str_detect(Question, "c6q136") ~ "School-Based Clinics"))


services_data <- services_data %>%
  dplyr::mutate(service_provision = dplyr::case_when(stringr::str_detect(Question, "a") ~ "LHD (Directly)",
                                                     stringr::str_detect(Question, "b") ~ "LHD (By Contract)",
                                                     stringr::str_detect(Question, "i") ~ "Other Entity",
                                                     stringr::str_detect(Question, "f") ~ "Not Available in Community",
                                                     stringr::str_detect(Question, "g") ~ "Status Unknown"))


# create a dataframe just for the total n
services_data_n <- services_data %>%
  group_by(Question, group) %>%
  summarise(demoninator = sum(Responses))

services_data <- services_data %>%
  dplyr::filter(Answer == "checked")


services_data <- left_join(services_data, services_data_n, by = c("Question", "group"))


# Print Time --------------------------------------------------------------
# print elapsed time
end_time <- Sys.time() - start_time # calculate difference
print(end_time) # 1 or more hours



usethis::use_data(services_data, overwrite = TRUE)
