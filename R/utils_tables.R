#' Helper to handle common pivoting logic when names from group
#'
#' This function is a helper for pivoting a dataset where the `group` column
#' needs to be used as the pivot point. It selects a subset of columns and
#' uses `tidyr::pivot_wider` to widen the data based on the `group`.
#'
#' @param data A data frame containing the columns: `category`, `group`, `y`,
#' `Responses`, and optionally `subtitle` and `total_employees`.
#' @return A data frame with the `group` column pivoted wider.
#' @import dplyr
#' @import tidyr
pivot_data_group <- function(data) {
  data %>%
    dplyr::select(category, group, y, Responses, dplyr::any_of(c("subtitle", "filter", "comms_use", "comms_type", "partner_type", "Type"))) %>%
    tidyr::pivot_wider(names_from = group, values_from = c(y, Responses)) %>%
    dplyr::rename_with(standardize_column_names)  # Standardize column names
}

#' Helper to handle common pivoting logic when names from category
#'
#' This function pivots a dataset where the `category` column is used as the pivot point.
#' It selects a subset of columns and pivots them wider using `category`. The function
#' then renames `group` to `category` for consistency.
#'
#' @param data A data frame containing the columns: `category`, `group`, `y`,
#' `Responses`, and optionally `subtitle` and `total_employees`.
#' @return A data frame with the `category` column pivoted wider.
#' @import dplyr
#' @import tidyr
pivot_data_cat <- function(data) {
  data %>%
    dplyr::select(category, group, y, Responses, dplyr::any_of(c("subtitle", "total_employees"))) %>%
    tidyr::pivot_wider(names_from = category, values_from = c(y, Responses)) %>%
    dplyr::rename(category = group) %>%
    dplyr::rename_with(standardize_column_names)  # Standardize column names
}

#' Helper to calculate overall LHD data
#'
#' This function adds overall data for "All LHDs" by performing a left join
#' between the input `data` and `data_all` based on the `category` column.
#'
#' @param data A data frame to be augmented with "All LHDs" data.
#' @param data_all A data frame containing "All LHDs" data with `y_All LHDs`
#' and optionally `Responses_All LHDs`.
#' @return A data frame with additional columns for "All LHDs" values.
#' @import dplyr
add_all_ldh_count <- function(data, data_all) {
  # Ensure input is a data frame
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.data.frame(data_all)) stop("`data_all` must be a data frame.")

  # Standardize column names
  data <- data %>%
    dplyr::rename_with(standardize_column_names)

  data_all <- data_all %>%
    dplyr::rename_with(standardize_column_names)

  # Select necessary columns to avoid conflicts
  data_all <- data_all %>%
    dplyr::select(category, `y_All LHDs`, `Responses_All LHDs`)

  # Perform the join
  data <- data %>%
    dplyr::left_join(data_all, by = "category")

  return(data)
}


#' Helper to process weighted statistics
#'
#' This function processes weighted statistics for a given topic and optional filter.
#' It also adds a subtitle indicating the filter used and selects relevant columns.
#'
#' @param topic The topic to process statistics for.
#' @param filtering An optional filter to apply during processing.
#' @return A data frame with weighted statistics and additional metadata.
#' @import dplyr
process_weighted_stats <- function(topic, filtering = NULL) {
  return_survey_weighted_statistics(topic, filtering) %>%
    dplyr::mutate(subtitle = paste0("By ", names(landscape_filters[landscape_filters == filtering]))) %>%
    dplyr::select(category, group, y, subtitle, Responses) %>%
    dplyr::rename_with(standardize_column_names)  # Standardize here
}

#' Helper to process jobs-related data
#'
#' This function processes jobs data, either standardized or non-standardized.
#' If standardized, it calculates job metrics as percentages of total employees.
#'
#' @param jobs_data A data frame containing jobs data.
#' @param is_standardized A logical value indicating whether the data should be
#' processed in a standardized format (as percentages).
#' @return A processed data frame with job-related statistics.
#' @import dplyr
process_jobs_data <- function(jobs_data, filtering = "All LHDs" ) {
  if (filtering == "All LHDs") {

    data  <- jobs_data %>%
      dplyr::mutate(group = dplyr::case_when(group == "13" ~ "All LHDs", TRUE ~ group)) %>%
      dplyr::filter(filter == "All LHDs") %>%
      dplyr::select(category, group, y, Responses, total_employees) %>%
      pivot_data_group() %>%
      dplyr::rename_with(standardize_column_names)  # Standardize here

  } else {

    data_all  <- jobs_data %>%
      dplyr::mutate(group = dplyr::case_when(group == "13" ~ "All LHDs", TRUE ~ group)) %>%
      dplyr::filter(filter == "All LHDs") %>%
      dplyr::select(category, group, y, Responses, total_employees) %>%
      pivot_data_group() %>%
      dplyr::rename_with(standardize_column_names)  # Standardize here

    data  <- jobs_data %>%
      dplyr::mutate(group = dplyr::case_when(group == "13" ~ "All LHDs", TRUE ~ group)) %>%
      dplyr::filter(filter == filtering) %>%
      dplyr::select(category, group, y, Responses, total_employees) %>%
      pivot_data_group() %>%
      dplyr::rename_with(standardize_column_names) %>%  # Standardize here
      dplyr::left_join(data_all, by = "category")
  }
}


#' Helper to clean and process classification data
#'
#' This function processes classification data for a given topic, applying the necessary
#' filtering and pivoting logic. If `filtering` is set to "All LHDs", it returns only
#' the processed "All LHDs" data.
#'
#' @param topic The topic for classification processing.
#' @param classifications A vector of classifications to process.
#' @param filtering The filter to apply during processing (e.g., "All LHDs").
#' @return A data frame with processed classification data or NULL if classifications are invalid.
#' @import dplyr
#' @import tidyr
clean_class_data <- function(topic, classifications, filtering) {
  if (!is.null(classifications) && length(classifications) >= 1) {

    selected_vars <- paste0(classifications, topic)
    selected_data <- get(paste0("classification_data_", topic))

    if (topic == "a") {

    data_all_class_n <- selected_data %>%
      dplyr::filter(Question %in% selected_vars) %>%
      dplyr::group_by(Question, group)%>%
      dplyr::reframe(Question = unique(Question),
                     Responses = sum(Responses))%>%
      dplyr::select(Responses, Question, group)

    data_all_class <- selected_data %>%
      dplyr::filter(Question %in% selected_vars) %>%
      dplyr::filter(category == "Checked", filter == "All LHDs") %>%
      dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(Question)]),
                    group = dplyr::case_when(group %in% c("13", "All Lhds") ~ "All LHDs", TRUE ~ group))%>%
      dplyr::select(-Responses) %>%
      dplyr::left_join(data_all_class_n, by = c("Question", "group"))


      data_all_class <- data_all_class %>%
        dplyr::mutate(filter = filtering,
                      y = round_up(Percentage, 0)) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(x = seq_along(category) - 1) %>%
        dplyr::ungroup()%>%
        pivot_data_group()

    } else {

      data_all_class <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::filter(category == "Yes", filter == "All LHDs") %>%
        dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(Question)]),
                      group = dplyr::case_when(group %in% c("13", "All Lhds") ~ "All LHDs", TRUE ~ group))
      # %>%
      #   dplyr::left_join(data_all_class_n, by = c("Question", "group"))

      data_all_class <- data_all_class %>%
        dplyr::mutate(filter = filtering,
                      y = round_up(Total, -2))

      data_all_class <- data_all_class %>%
        # dplyr::group_by(group) %>%
        # dplyr::mutate(x = seq_along(category) - 1) %>%
        # dplyr::ungroup()%>%
        pivot_data_group()

    }



    # %>%
    #   dplyr::rename_with(standardize_column_names)  # Standardize here

    if (topic == "a") {

      data_class_n <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::group_by(Question, group)%>%
        dplyr::reframe(Question = unique(Question),
                       Responses = sum(Responses),
                       group = unique(group))%>%
        dplyr::select(Responses, Question, group)

      data_filtered <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::filter(category == "Checked", filter == filtering) %>%
        dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(.data$Question)]),
                      y = round_up(Percentage, 0)) %>%
      dplyr::select(-Responses) %>%
        dplyr::left_join(data_class_n, by = c("Question", "group"))

    } else {
      data_filtered <- selected_data %>%
        dplyr::filter(Question %in% selected_vars) %>%
        dplyr::filter(category == "Yes", filter == filtering) %>%
        dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(.data$Question)]),
                      y = round_up(Total, -2))
    }

    data <- data_filtered %>%
      pivot_data_group()  %>%
      dplyr::left_join(data_all_class, by = c("category", "filter"))
    # %>%
    #   dplyr::rename_with(standardize_column_names)  # Standardize here


    # data <- bind_rows(data_all_class, data_filtered)
    #
    #
    # %>%
    #   dplyr::mutate(filter = filtering) %>%
    #   dplyr::group_by(group) %>%
    #   dplyr::mutate(x = seq_along(category) - 1) %>%
    #   dplyr::ungroup()%>%
    #   pivot_data_group() %>%
    #   dplyr::rename_with(standardize_column_names)  # Standardize here
    #
    # data_all_class <- data_all_class

    if (filtering == "All LHDs") {
      return(data_all_class)
    } else {
      return(data)
    }


  }

  return(NULL)
}


#' Helper to process general data for tables
#'
#' This function processes data based on different topics and filtering options,
#' including handling special cases like "All LHDs", jobs-related data, and more.
#'
#' @param topic The topic to process.
#' @param filtering The filter to apply to the data (e.g., "All LHDs").
#' @param classifications Optional classifications to apply to the data.
#' @return A processed data frame for the specified topic and filtering.
#' @import dplyr
#' @import tidyr
#' @export
#Function to filter data for tables--------

process_table_data <- function(topic, filtering, classifications = NULL) {
  # Handle All LHDs cases
  if (filtering == "All LHDs") {

    if (topic %in% c("FTE_numcat", "employee_numcat")) {
      data <- return_survey_weighted_counts(topic) %>%
        pivot_data_group()

    } else if (topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop",
                            "c3q15", "totexp_percap", "rev_percap", "rev_total")) {

      data <- return_survey_weighted_statistics(topic) %>%
        dplyr::select(Responses, y, group, Quantile_q50)%>%
        dplyr::rename(Median = Quantile_q50,
                      Mean = y)%>%
        tidyr::pivot_longer(cols = c("Mean", "Median"),
                            names_to = "category",
                            values_to = "y")%>%
        pivot_data_group()




    } else if (topic == "jobs_total") {
      data <- process_jobs_data(jobs_data)

    } else if (topic == "field") {

      data <- spec_data_all %>%
        dplyr::select(category, y, subtitle, Responses) %>%
        dplyr::mutate(category = category,
                         'y_All LHDs' = y,
                         'Responses_All LHDs' = Responses,
                         subtitle = subtitle)

    } else if (topic == "tenure") {

      data <- return_survey_weighted_counts("tenure_numcat") %>%
        pivot_data_group()

      data$category <- factor(data$category,
                              levels = c("Less Than 2 Years", "2 - 5 Years",
                                         "6 - 10 Years", "11 Or More Years"),
                              ordered = TRUE)




     } else if (topic == "a") {

         data <- clean_class_data(topic, classifications, filtering)

     } else if (topic == "b") {

         data <- clean_class_data(topic, classifications, filtering)

     } else if (topic == "budget_changes") {

       data <- budget_data %>%
         dplyr::filter(filter == "All LHDs") %>%
         pivot_data_group()

     } else if (topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & filtering == "All LHDs") {

       data <- return_survey_weighted_counts(topic) %>%
         dplyr::mutate(x_label = NA)%>%
         pivot_data_group()

     # } else if (topic == "rev_total") {
     #
     #   data <- return_survey_weighted_counts(topic) %>%
     #     dplyr::mutate(x_label = NA)%>%
     #     pivot_data_group()

     # } else if (topic %in% c("c3q15", "totexp_percap", "rev_percap")) {
     #
     #   data <- return_survey_weighted_statistics(topic) %>%
     #     dplyr::select(Responses, y, group, Quantile_q50) %>%
     #     dplyr::rename(
     #       Mean = y,
     #       Median = Quantile_q50
     #     ) %>%
     #     tidyr::pivot_longer(
     #       cols = c("Mean", "Median"),
     #       names_to = "category",
     #       values_to = "y"
     #     ) %>%
     #     pivot_data_cat()

     } else if (topic %in% c("LHD (Directly)", "LHD (By Contract)",
                            "Other Entity", "Not Available in Community")) {


       data <- services_data %>%
         dplyr::filter(service_classification %in% classifications,
                       filter == "All LHDs",
                       service_provision == topic) %>%
         dplyr::group_by(service_type) %>%
         dplyr::reframe(
           y = unique(y),
           Responses = sum(Responses, na.rm = TRUE),
           group = unique(group),
           category = unique(category),
           filter = unique(filter)
         ) %>%
         dplyr::filter(category %in% c("Checked", "1")) %>%
         dplyr::mutate(group = filter,
                       category = service_type,
                       subtitle = "All LHDs") %>%

         pivot_data_group()


     } else if (topic == "Reduced/Expanded Services") {




       data <- services_change_data%>%
         dplyr::filter(filter == "All LHDs",
                       service_type %in% classifications) %>%
         dplyr::group_by(category) %>%
         dplyr::reframe(
           y = round_up(mean(y, na.rm = TRUE), 1),
           Responses = sum(Responses, na.rm = TRUE),
           group = unique(group),
           category = unique(category)
         ) %>%
         dplyr::mutate(Responses = sum(Responses)) %>%
         pivot_data_group() %>%
         dplyr::mutate(group = "All LHDs")



     } else if (topic == "tech_use") {



       data <- tech_use_data %>%
         dplyr::filter(filter == "All LHDs")%>%
         dplyr::group_by(tech_type) %>%
         dplyr::mutate(Responses = sum(Responses)) %>%
         dplyr::ungroup() %>%
         dplyr::filter(category %in% c("Implemented")) %>%
         dplyr::select(y, group, subtitle, Responses, filter, tech_type ) %>%
         dplyr::rename(category = tech_type)%>%
         pivot_data_group()



     } else if (stringr::str_detect(topic, "comm_channels")) {

       data <- comms_use_data %>%
         dplyr::filter(filter == "All LHDs")%>%
         dplyr::group_by(comms_type, comms_use) %>%
         dplyr::mutate(Responses = sum(Responses)) %>%
         dplyr::ungroup() %>%
         dplyr::filter(category %in% c("Checked")) %>%
         dplyr::select(y, group, subtitle, Responses, filter, comms_type, comms_use)%>%
         dplyr::mutate(category = comms_type)


       if (topic == "comm_channels_1") {
         data <- data %>%
           dplyr::filter(comms_use == "Emergency Response")
       } else if (topic == "comm_channels_2") {
         data <- data %>%
           dplyr::filter(comms_use == "General Announcements")
       } else {
         data <- data %>%
           dplyr::filter(comms_use == "Any Use")
       }

       data <- data %>%
         pivot_data_group()



     } else if (topic == "medical") {

       data <- med_data %>%
         dplyr::filter(filter == "All LHDs")%>%
         dplyr::group_by(category) %>%
         pivot_data_group()

     } else if (topic == "dental") {

       data <- dental_data %>%
         dplyr::filter(filter == "All LHDs")%>%
         dplyr::group_by(category) %>%
         pivot_data_group()

     } else if (topic == "behavioral") {

       data <- behav_data %>%
         dplyr::filter(filter == "All LHDs")%>%
         dplyr::group_by(category) %>%
         pivot_data_group()

     } else if (topic %in% c("chachip", "c7q149", "c7q147")) {

       data_all <- return_survey_weighted_counts(topic)


       if (topic == "chachip") {

         data_all <- data_all %>% dplyr::mutate(
           category = ordered(category, levels = c("Yes", "No")))%>%
           dplyr::mutate(category =
                           dplyr::recode(category,
                                         "Yes" = "Completed both in past 5 years",
                                         "No" = " Completed one or none in past 5 years"))


       } else if (topic == "c7q147") {


         data_all <- data_all %>%
           dplyr::mutate(
             category = dplyr::case_when(
               category == "Yes, Within Last 3 Years" ~
                 "Completed CHA within last 3 years",

               category == "Yes, More Than 3 Years, Within 5 Years" ~
                 "Completed CHA within the last 3 to 5 years",

               category == "Yes, More Than 5 Years" ~
                 "Completed CHA more than 5 years ago",

               category %in% c("No", "No, But Plan To Within Next Year") ~
                 "Has not yet completed CHA",

               TRUE ~ NA_character_
             )
           ) %>%
           dplyr::group_by(category, group) %>%
           dplyr::reframe(
             y = sum(y),
             Responses = sum(Responses),
             group = unique(group),
             category = unique(category)
           )

       } else {

         data_all <- data_all %>%
           dplyr::mutate(
             category = dplyr::case_when(
               category == "Yes, Within Last 3 Years" ~
                 "Completed CHIP within last 3 years",
               category == "Yes, More Than 3 Years, Within 5 Years" ~
                 "Completed CHIP within the last 3 to 5 years",
               category == "Yes, More Than 5 Years" ~
                 "Completed CHIP more than 5 years ago",
               category %in% c("No", "No, But Plan To Within Next Year") ~
                 "Has not yet completed CHIP",
               TRUE ~ NA_character_
             )
           ) %>%
           dplyr::group_by(category, group) %>%
           dplyr::reframe(
             y = sum(y),
             Responses = sum(Responses),
             group = unique(group),
             category = unique(category)
           )
       }

       data <- data_all %>%
         pivot_data_group() %>%
         dplyr::mutate(`Responses_All LHDs` = 1056)





     } else if (topic == "c12q260") {

       data_n <- advocacy_data %>%
         dplyr::filter(filter == "All LHDs") %>%
         dplyr::group_by(advocacy_type)%>%
         dplyr::reframe(Responses = sum(Responses))

       data <- advocacy_data %>%
         dplyr::filter(filter == "All LHDs",
                       category %in% "Checked") %>%
         dplyr::select(y, group, subtitle, Responses, filter, advocacy_type)%>%
         dplyr::rename(category = advocacy_type) %>%
         dplyr::mutate(Responses = data_n$Responses) %>%
         pivot_data_group()



     } else if (topic == "Newc12q502") {

       data <- new_advocacy_data %>%
         dplyr::filter(
           filter == "All LHDs",
           category %in% c("Checked", "1")
         ) %>%
         dplyr::group_by(new_advocacy_type, group) %>%
         dplyr::summarise(
           y = mean(y, na.rm = TRUE),
           Responses = dplyr::first(Responses),
           .groups = "drop"
         ) %>%
         dplyr::rename(category = new_advocacy_type) %>%
         pivot_data_group()





     } else if (topic == "m9q240") {

       data_n <- chip_data %>%
         dplyr::filter(filter == "All LHDs") %>%
         dplyr::group_by(action) %>%
         dplyr::summarise(
           Responses = sum(Responses, na.rm = TRUE),
           .groups = "drop"
         )

       data <- chip_data %>%
         dplyr::filter(
           filter == "All LHDs",
           category == "1"
         ) %>%
         dplyr::select(y, group, subtitle, filter, action) %>%
         dplyr::rename(category = action) %>%
         dplyr::left_join(
           data_n,
           by = c("category" = "action")
         ) %>%
         pivot_data_group()


     } else if (topic %in%  c("formal_part", "any_part")) {

       if (topic == "formal_part") {

       data <- partnerships %>%
         dplyr::filter(
                       filter == "All LHDs",
                       suffix == "work2"

                       ) %>%

         dplyr::group_by(partner) %>%
         # dplyr::group_by(Question) %>%
         dplyr::mutate(Responses = sum(Responses)) %>%
         dplyr::ungroup() %>%
         dplyr::mutate(category = dplyr::case_when(
           Answer == "LHD works with- agreements, personnel, meetings" ~ "Checked",
           Answer == "no relationship, exchange info" ~ "Unchecked",
           TRUE ~ NA_character_
         )) %>%
         dplyr::filter(category == "Checked")   %>%
         dplyr::group_by(partner_type)%>%
         dplyr::select(y, group, subtitle, Responses, filter, partner, partner_type)%>%
         dplyr::rename(category = partner,
                       "Type" = partner_type) %>%
         pivot_data_group()

       } else {

         data <- partnerships %>%
           dplyr::filter(
             filter == "All LHDs",
             suffix == "work"

           ) %>%

           dplyr::group_by(partner) %>%
           # dplyr::group_by(Question) %>%
           dplyr::mutate(Responses = sum(Responses)) %>%
           dplyr::ungroup() %>%
           dplyr::mutate(category = dplyr::case_when(
             Answer == "no relationship" ~ "Unchecked",
             TRUE ~ "Checked")) %>%
           dplyr::filter(category == "Checked")   %>%
           dplyr::group_by(partner_type, filter, group) %>%
           dplyr::select(y, group, subtitle, Responses, filter, partner, partner_type)%>%
           dplyr::rename(category = partner,
                         "Type" = partner_type) %>%
           pivot_data_group()


       }






    } else {
      data <- return_survey_weighted_counts(topic)

      # Calculate total responses and 'y_All LHDs'
      total_responses <- sum(data$Responses, na.rm = TRUE)
      data <- data %>%
        dplyr::mutate(
          y = 100 * (Responses / total_responses),
          Responses = total_responses
        ) %>%
        dplyr::select(category, group, y, Responses) %>%
        dplyr::left_join(., add_all_counts_long(topic), by = "category")%>%
        dplyr::filter(category != "Na")

    }

  } else {

    # Handle filters other than All LHDs
    if (topic %in% c("FTE_numcat", "employee_numcat")) {

      grp_data_all <- return_survey_weighted_counts(topic) %>%
        pivot_data_group()

      data <- return_survey_weighted_counts(topic, filtering) %>%
        pivot_data_group()  %>%
        add_all_ldh_count(data_all = grp_data_all)


    } else if (topic %in% c("c5q37", "fte_bypop", "c5q36", "employee_bypop",
                            "c3q15", "totexp_percap", "rev_percap", "rev_total")) {


      data_1 <- nacchoProfile::return_survey_weighted_statistics(topic, filtering) %>%
        dplyr::select(Question, category, Group, y, Responses) %>%
        dplyr::mutate(year = 2022,
                      filter = filtering,
                      category = "Mean",
                      y = as.numeric(y),
                      group = Group) %>%
        dplyr::select(Question, category, group, y, filter, year, Responses)

      data_2 <- nacchoProfile::return_survey_weighted_statistics(topic, filtering) %>%
        dplyr::select(Question, category, Group, Quantile_q50, Responses) %>%
        dplyr::mutate(year = 2022,
                      filter = filtering,
                      category = "Median",
                      y = as.numeric(Quantile_q50),
                      group = Group) %>%
        dplyr::select(Question, category, group, y, filter, year, Responses)


      data <- dplyr::full_join(data_1, data_2) %>%
        pivot_data_group()

      all_data <- return_survey_weighted_statistics(topic) %>%
        dplyr::select(Responses, y, group, Quantile_q50)%>%
        dplyr::rename(Median = Quantile_q50,
                      Mean = y)%>%
        tidyr::pivot_longer(cols = c("Mean", "Median"),
                            names_to = "category",
                            values_to = "y")%>%
        pivot_data_group()

      data <- dplyr::left_join(data, all_data)

      colnames(data) <- standardize_column_names(colnames(data))



      # data <- process_weighted_stats(topic, filtering) %>%
      #   pivot_data_cat()
      #
      # stat_data_all <- process_weighted_stats(topic) %>%
      #   pivot_data_cat()
      #
      # data <- process_weighted_stats(topic, filtering) %>%
      #   pivot_data_cat() %>%
      #   add_all_ldh_count(data_all = stat_data_all)


    } else if (topic == "a") {

      data <- clean_class_data(topic, classifications, filtering)

    } else if (topic == "b") {

      data <- clean_class_data(topic, classifications, filtering)

    } else if (topic == "field") {

      data <- spec_data %>% dplyr::filter(filter == filtering) %>%
        dplyr::select(category, group, y, subtitle, Responses) %>%
        dplyr::group_by(category) %>%
        tidyr::pivot_wider(names_from = group, values_from = c(y, Responses))

      # Calculate overall y and Responses for "All LHDs"
      overall_data <- spec_data_all %>%
        dplyr::select(category, y, Responses) %>%
        dplyr::mutate(category = category,
                      'y_All LHDs' = y,
                      'Responses_All LHDs' = Responses)

      # Join the overall data to the pivoted data
      data <- data %>%
        dplyr::left_join(overall_data, by = "category")

    } else if (topic == "tenure") {

      data_all <- return_survey_weighted_counts("tenure_numcat") %>%
        pivot_data_group()

      data <- return_survey_weighted_counts("tenure_numcat", filtering) %>%
        pivot_data_group() %>%
        dplyr::left_join(data_all, by = "category")

      data$category <- factor(data$category,
                              levels = c("Less Than 2 Years", "2 - 5 Years",
                                         "6 - 10 Years", "11 Or More Years"),
                              ordered = TRUE)




    } else if (topic == "jobs_total") {

      data <- process_jobs_data(jobs_data, filtering)



    } else if (topic == "totpop") {
      data <- master_data %>%
        haven::as_factor() %>%
        dplyr::mutate(category = stringr::str_to_title(get(filtering)),
                      fig_value = get("totpop")) %>%
        dplyr::group_by(category)%>%
        dplyr::summarise(Responses = n(),
                         fig_value = sum(fig_value))%>%
        dplyr::mutate(y = round_up(fig_value/sum(fig_value)*100, 1),
                      subtitle = paste0("By ", names(landscape_filters[landscape_filters == filtering]))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(group = "All LHDs") %>%
        dplyr::select(category, group, y, subtitle, Responses) %>%
        tidyr::pivot_wider(names_from = category, values_from = c(y, Responses))

      overall_totals <- master_data %>%
        dplyr::summarise('Responses_All LHDs' = n(),
                         fig_value_All_LHDs = sum(get("totpop"))) %>%
        dplyr::mutate('y_All LHDs' = round_up(fig_value_All_LHDs / sum(fig_value_All_LHDs) * 100, 1))

      # Add the calculated columns to your data
      data <- data %>%
        dplyr::mutate('y_All LHDs' = overall_totals$'y_All LHDs',
                      'Responses_All LHDs' = overall_totals$'Responses_All LHDs')%>%
        dplyr::rename(category = group)

    } else if (topic == "budget_changes") {

      budget_data_all <- budget_data %>%
        dplyr::filter(filter == "All LHDs") %>%
        pivot_data_group()

      data <- budget_data %>%
        dplyr::filter(filter == filtering) %>%
        pivot_data_group() %>%
        add_all_ldh_count(data_all = budget_data_all)

    } else if (topic %in% c("exp_total_numcat", "exp_percap_numcat", "rev_total_numcat", "rev_percap_numcat") & filtering != "All LHDs") {

        exp_data_all <- return_survey_weighted_counts(topic) %>%
          dplyr::mutate(x_label = NA)%>%
          pivot_data_group()

        data <- return_survey_weighted_counts(topic, filtering) %>%
          dplyr::mutate(x_label = NA)%>%
          pivot_data_group() %>%
          add_all_ldh_count(data_all = exp_data_all)

    # } else if (topic == "rev_total") {
    #
    #   rev_data_all <- revenue_percentages%>%
    #     dplyr::mutate(
    #       category = revenue_type,
    #       y = (percentage*100)
    #     ) %>%
    #     pivot_data_group()
    #
    #
    #   data <- revenue_percentages_filter %>%
    #     dplyr::mutate(
    #       group = category,
    #       category = revenue_type,
    #       y = (percentage*100)
    #     ) %>%
    #     dplyr::filter(filter == filtering)%>%
    #     pivot_data_group()%>%
    #     add_all_ldh_count(data_all = rev_data_all )

    # } else if (topic %in% c("c3q15", "totexp_percap", "rev_percap")) {
    #
    #   all_lhds_data <- return_survey_weighted_statistics(topic) %>%
    #     dplyr::select(Responses, y, group, Quantile_q50) %>%
    #     dplyr::rename(
    #       Mean = y,
    #       Median = Quantile_q50
    #     ) %>%
    #     tidyr::pivot_longer(
    #       cols = c("Mean", "Median"),
    #       names_to = "category",
    #       values_to = "y"
    #     ) %>%
    #     pivot_data_group()
    #
    #   filtered_data <- return_survey_weighted_statistics(topic, filtering) %>%
    #     dplyr::select(Responses, y, Group, Quantile_q50) %>%
    #     dplyr::rename(
    #       Mean = y,
    #       Median = Quantile_q50
    #     ) %>%
    #     dplyr::group_by(Group) %>%
    #     tidyr::pivot_longer(
    #       cols = c("Mean", "Median"),
    #       names_to = "category",
    #       values_to = "y"
    #     )
    #     pivot_data_group()
    #
    #   # Add All LHDs data to filtered data
    #   data <- filtered_data %>%
    #     add_all_ldh_count(data_all = all_lhds_data)


    } else if (topic %in% c("LHD (Directly)", "LHD (By Contract)",
                           "Other Entity", "Not Available in Community")) {



      services_data_all <- services_data %>%
        dplyr::filter(service_classification %in% classifications,
                      filter == "All LHDs",
                      service_provision == topic) %>%
        dplyr::group_by(service_type) %>%
        dplyr::reframe(
          y = y,
          Responses = sum(Responses, na.rm = TRUE),
          group = unique(group),
          category = unique(category),
          filter = unique(filter)
        ) %>%
        dplyr::filter(category %in% c("Checked", "1")) %>%
        dplyr::mutate(group = filter,
                      category = service_type,
                      subtitle = "All LHDs") %>%

        pivot_data_group()


      data <- services_data %>%
        dplyr::filter(service_classification %in% classifications,
                      filter == filtering,
                      service_provision == topic) %>%
        dplyr::group_by(service_type, group) %>%
        dplyr::reframe(
          y = y,
          Responses = sum(Responses, na.rm = TRUE),
          group = unique(group),
          category = unique(category),
          filter = unique(filter)
        ) %>%
        dplyr::filter(category %in% c("Checked", "1")) %>%
        dplyr::mutate(category = service_type) %>%
        pivot_data_group()%>%
        add_all_ldh_count(data_all = services_data_all)

# View(data)

    } else if (topic == "Reduced/Expanded Services") {

      services_data_all <- services_change_data%>%
        dplyr::filter(filter == "All LHDs",
                      service_type %in% classifications) %>%
        # dplyr::group_by(category) %>%
        dplyr::reframe(
          y = round_up(y, 0),
          x = as.numeric(factor(group, levels = unique(group))) - 1,
          Responses = sum(Responses, na.rm = TRUE),
          group = unique(group),
          category = unique(category)
        )
      # %>%
      #   pivot_data_group() %>%
      #   dplyr::mutate(group = "All LHDs")


      data <- services_change_data %>%
        dplyr::filter(filter == filtering,
                      service_type %in% classifications)%>%
        dplyr::group_by(group) %>%
        dplyr::reframe(
          y = round_up(y, 0),
          x = as.numeric(factor(group, levels = unique(group))) - 1,
          Responses = sum(Responses, na.rm = TRUE),
          group = unique(group),
          category = unique(category)
        )

      data <- dplyr::full_join(data, services_data_all)%>%
        dplyr::mutate(x = as.numeric(factor(group, levels = unique(group))) - 1)%>%
        pivot_data_group()
      # %>%
      #   add_all_ldh_count(data_all = services_data_all)

    } else if (topic == "tech_use") {

      tech_data_all <- tech_use_data %>%
        dplyr::filter(filter == "All LHDs")%>%
        dplyr::group_by(tech_type) %>%
        dplyr::mutate(Responses = sum(Responses)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(category %in% c("Implemented")) %>%
        dplyr::select(y, group, subtitle, Responses, filter, tech_type ) %>%
        dplyr::rename(category = tech_type)%>%
        pivot_data_group()


      data <- tech_use_data %>%
        dplyr::filter(filter == filtering)%>%
        dplyr::group_by(tech_type) %>%
        dplyr::mutate(Responses = sum(Responses)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(category %in% c("Implemented")) %>%
        dplyr::select(y, group, subtitle, Responses, filter, tech_type) %>%
        dplyr::rename(category = tech_type)%>%
        pivot_data_group()%>%
        add_all_ldh_count(data_all = tech_data_all)


    } else if (stringr::str_detect(topic, "comm_channels")) {

      comms_data_all <- comms_use_data %>%
        dplyr::filter(filter == "All LHDs")%>%
        dplyr::group_by(comms_type, comms_use) %>%
        dplyr::mutate(Responses = sum(Responses)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(category %in% c("Checked")) %>%
        dplyr::select(y, group, subtitle, Responses, filter, comms_type, comms_use)%>%
        dplyr::mutate(category = comms_type,
                      filter = filtering)

      data_filter <- comms_use_data %>%
        dplyr::filter(filter == filtering)%>%
        dplyr::group_by(comms_type, comms_use) %>%
        dplyr::mutate(Responses = sum(Responses)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(category %in% c("Checked")) %>%
        dplyr::select(y, group, subtitle, Responses, filter, comms_type, comms_use)%>%
        dplyr::mutate(category = comms_type)

      data <- dplyr::full_join(comms_data_all, data_filter)

      if (topic == "comm_channels_1") {
      data <- data %>%
        dplyr::filter(comms_use == "Emergency Response")
      } else if (topic == "comm_channels_2") {
        data <- data %>%
          dplyr::filter(comms_use == "General Announcements")
      } else {
        data <- data %>%
          dplyr::filter(comms_use == "Any Use")
      }

      data <- data %>%
        dplyr::select(!(subtitle)) %>%
        pivot_data_group()


    } else if (topic == "medical") {

      med_data_all <- med_data %>%
        dplyr::filter(filter == "All LHDs")%>%
        dplyr::group_by(category) %>%
        pivot_data_group()



      data <- med_data %>%
        dplyr::filter(filter == filtering)%>%
        dplyr::group_by(category) %>%
        pivot_data_group() %>%
        add_all_ldh_count(data_all = med_data_all)

    } else if (topic == "dental") {

      dental_data_all <- dental_data %>%
        dplyr::filter(filter == "All LHDs")%>%
        dplyr::group_by(category) %>%
        pivot_data_group()

      data <- dental_data %>%
        dplyr::filter(filter == filtering)%>%
        dplyr::group_by(category) %>%
        pivot_data_group() %>%
        add_all_ldh_count(data_all = dental_data_all)

    } else if (topic == "behavioral") {

      behav_data_all <- behav_data %>%
        dplyr::filter(filter == "All LHDs")%>%
        dplyr::group_by(category) %>%
        pivot_data_group()

      data <- behav_data %>%
        dplyr::filter(filter == filtering)%>%
        dplyr::group_by(category) %>%
        pivot_data_group() %>%
        add_all_ldh_count(data_all = behav_data_all)

    } else if (topic %in% c("chachip", "c7q149", "c7q147")) {

        data_all <- return_survey_weighted_counts(topic)


        data <- return_survey_weighted_counts(topic, filtering)



        if (topic == "chachip") {

          data_all <- data_all %>%
            dplyr::mutate(category =
                            dplyr::recode(category,
                                          "Yes" = "Completed both in past 5 years",
                                          "No" = "Completed one or none in past 5 years"))

          data <- data %>%
            dplyr::mutate(category =
                            dplyr::recode(category,
                                          "Yes" = "Completed both in past 5 years",
                                          "No" = "Completed one or none in past 5 years")
      )
        } else if (topic == "c7q147") {

          data <- data %>%
            dplyr::mutate(
              category = dplyr::case_when(
                category == "Yes, Within Last 3 Years" ~
                  "Completed CHA within last 3 years",
                category == "Yes, More Than 3 Years, Within 5 Years" ~
                  "Completed CHA within the last 3 to 5 years",
                category == "Yes, More Than 5 Years" ~
                  "Completed CHA more than 5 years ago",
                category %in% c("No", "No, But Plan To Within Next Year") ~
                  "Has not yet completed CHA",
                TRUE ~ NA_character_
              )
            ) %>%
            dplyr::group_by(category, group) %>%
            dplyr::reframe(
              y = sum(y),
              Responses = sum(Responses),
              group = unique(group),
              category = unique(category)
            )

          data_all <- data_all %>%
            dplyr::mutate(
              category = dplyr::case_when(
                category == "Yes, Within Last 3 Years" ~
                  "Completed CHA within last 3 years",
                category == "Yes, More Than 3 Years, Within 5 Years" ~
                  "Completed CHA within the last 3 to 5 years",
                category == "Yes, More Than 5 Years" ~
                  "Completed CHA more than 5 years ago",
                category %in% c("No", "No, But Plan To Within Next Year") ~
                  "Has not yet completed CHA",
                TRUE ~ NA_character_
              )
            ) %>%
            dplyr::group_by(category, group) %>%
            dplyr::reframe(
              y = sum(y),
              Responses = sum(Responses),
              group = unique(group),
              category = unique(category)
            )

        } else {

          data <- data %>%
            dplyr::mutate(
              category = dplyr::case_when(
                category == "Yes, Within Last 3 Years" ~
                  "Completed CHIP within last 3 years",
                category == "Yes, More Than 3 Years, Within 5 Years" ~
                  "Completed CHIP within the last 3 to 5 years",
                category == "Yes, More Than 5 Years" ~
                  "Completed CHIP more than 5 years ago",
                category %in% c("No", "No, But Plan To Within Next Year") ~
                  "Has not yet completed CHIP",
                TRUE ~ NA_character_
              )
            ) %>%
            dplyr::group_by(category, group) %>%
            dplyr::reframe(
              y = sum(y),
              Responses = sum(Responses),
              group = unique(group),
              category = unique(category)
            )

          data_all <- data_all %>%
            dplyr::mutate(
              category = dplyr::case_when(
                category == "Yes, Within Last 3 Years" ~
                  "Completed CHIP within last 3 years",
                category == "Yes, More Than 3 Years, Within 5 Years" ~
                  "Completed CHIP within the last 3 to 5 years",
                category == "Yes, More Than 5 Years" ~
                  "Completed CHIP more than 5 years ago",
                category %in% c("No", "No, But Plan To Within Next Year") ~
                  "Has not yet completed CHIP",
                TRUE ~ NA_character_
              )
            ) %>%
            dplyr::group_by(category, group) %>%
            dplyr::reframe(
              y = sum(y),
              Responses = sum(Responses),
              group = unique(group),
              category = unique(category)
            )
        }

        data_all <- data_all %>%
          pivot_data_group()

        data <- data %>%
          dplyr::group_by(group)%>%
          dplyr::mutate(Responses = sum(Responses)) %>%
          dplyr::ungroup()%>%
          pivot_data_group()

        data <- data %>%
          dplyr::left_join(data_all, by = "category") %>%

          # add_all_ldh_count(data_all = data_all) %>%
          dplyr::mutate(`Responses_All LHDs` = 1056)



      } else if (topic %in% c("formal_part", "any_part")) {


        if (topic == "formal_part") {

          data_all <- partnerships %>%
            dplyr::filter(
              filter == "All LHDs",
              suffix == "work2"

            ) %>%

            dplyr::group_by(partner) %>%
            # dplyr::group_by(Question) %>%
            dplyr::mutate(Responses = sum(Responses)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(category = dplyr::case_when(
              Answer == "LHD works with- agreements, personnel, meetings" ~ "Checked",
              Answer == "no relationship, exchange info" ~ "Unchecked",
              TRUE ~ NA_character_
            )) %>%
            dplyr::filter(category == "Checked")   %>%
            # dplyr::group_by(partner_type)%>%
            dplyr::select(y, group, Responses, partner)%>%
            dplyr::rename(category = partner) %>%
            pivot_data_group()


          data <- partnerships %>%
            dplyr::filter(
                          filter == filtering,
                          suffix == "work2"

            ) %>%

            dplyr::group_by(partner, group) %>%
            # dplyr::group_by(group) %>%
            # dplyr::group_by(Question) %>%
            dplyr::mutate(Responses = sum(Responses)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(category = dplyr::case_when(
              Answer == "LHD works with- agreements, personnel, meetings" ~ "Checked",
              Answer == "no relationship, exchange info" ~ "Unchecked",
              TRUE ~ NA_character_
            )) %>%
            dplyr::filter(category == "Checked")   %>%
            dplyr::group_by(partner_type, group, filter)%>%
            dplyr::select(y, group, subtitle, Responses, filter, partner, partner_type)%>%
            dplyr::rename(category = partner,
                          "Type" = partner_type) %>%
            pivot_data_group() %>%
            dplyr::left_join(data_all, by = "category")



        } else {

          data_all <- partnerships %>%
            dplyr::filter(
              filter == "All LHDs",
              suffix == "work"

            ) %>%

            dplyr::group_by(partner) %>%
            # dplyr::group_by(Question) %>%
            dplyr::mutate(Responses = sum(Responses)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(category = dplyr::case_when(
              Answer == "no relationship" ~ "Unchecked",
              TRUE ~ "Checked")) %>%
            dplyr::filter(category == "Checked")   %>%
            # dplyr::group_by(partner_type)%>%
            dplyr::select(y, group, Responses, partner)%>%
            dplyr::rename(category = partner) %>%
            pivot_data_group()


          data <- partnerships %>%
            dplyr::filter(
              filter == filtering,
              suffix == "work"

            ) %>%

            dplyr::group_by(partner, group) %>%
            # dplyr::group_by(group) %>%
            # dplyr::group_by(Question) %>%
            dplyr::mutate(Responses = sum(Responses)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(category = dplyr::case_when(
              Answer == "no relationship" ~ "Unchecked",
              TRUE ~ "Checked")) %>%
            dplyr::filter(category == "Checked")   %>%
            dplyr::group_by(partner_type, group, filter)%>%
            dplyr::select(y, group, subtitle, Responses, filter, partner, partner_type)%>%
            dplyr::rename(category = partner,
                          "Type" = partner_type) %>%
            pivot_data_group() %>%
            dplyr::left_join(data_all, by = "category")




        }




    } else if (topic == "c12q260") {

      data_all_n <- advocacy_data %>%
        dplyr::filter(filter == "All LHDs") %>%
        dplyr::group_by(advocacy_type)%>%
        dplyr::reframe(Responses = sum(Responses))

      data_n <- advocacy_data %>%
        dplyr::filter(filter == filtering) %>%
        dplyr::group_by(advocacy_type, group)%>%
        dplyr::reframe(Responses = sum(Responses))


      advocacy_data_all <- advocacy_data %>%
        dplyr::filter(filter == "All LHDs",
                      category %in% "Checked") %>%
        dplyr::select(y, group, subtitle, Responses, filter, advocacy_type)%>%
        dplyr::rename(category = advocacy_type) %>%
        dplyr::mutate(Responses = data_all_n$Responses) %>%
        pivot_data_group()

      data <- advocacy_data %>%
        dplyr::filter(filter == filtering,
                      category %in% "Checked") %>%
        dplyr::select(y, group, subtitle, Responses, filter, advocacy_type)%>%
        dplyr::rename(category = advocacy_type) %>%
        dplyr::mutate(Responses = data_n$Responses) %>%
        pivot_data_group()%>%
        add_all_ldh_count(data_all = advocacy_data_all)

    } else if (topic == "Newc12q502") {

      # All LHDs
      new_advocacy_data_all <- new_advocacy_data %>%
        dplyr::filter(
          filter == "All LHDs",
          category %in% c("Checked", "1")
        ) %>%
        dplyr::group_by(new_advocacy_type, group) %>%
        dplyr::summarise(
          y = mean(y, na.rm = TRUE),
          Responses = dplyr::first(Responses),
          .groups = "drop"
        ) %>%
        dplyr::rename(category = new_advocacy_type) %>%
        pivot_data_group()

      # Filtered groups
      data <- new_advocacy_data %>%
        dplyr::filter(
          filter == filtering,
          category %in% c("Checked", "1")
        ) %>%
        dplyr::group_by(new_advocacy_type, group) %>%
        dplyr::summarise(
          y = mean(y, na.rm = TRUE),
          Responses = dplyr::first(Responses),
          .groups = "drop"
        ) %>%
        dplyr::rename(category = new_advocacy_type) %>%
        pivot_data_group() %>%
        add_all_ldh_count(data_all = new_advocacy_data_all)

    } else if (topic == "m9q240") {

      # Calculate All LHDs response counts by CHIP activity
      data_n <- chip_data %>%
        dplyr::filter(filter == "All LHDs") %>%
        dplyr::group_by(action) %>%
        dplyr::summarise(
          Responses = sum(Responses, na.rm = TRUE),
          .groups = "drop"
        )

      # Prepare All LHDs data
      chip_data_all <- chip_data %>%
        dplyr::filter(filter == "All LHDs",
                      as.character(category) == "1"
        ) %>%
        dplyr::select(y, group, subtitle, filter, action) %>%
        dplyr::rename(category = action) %>%
        dplyr::left_join(
          data_n,
          by = c("category" = "action")
        ) %>%
        pivot_data_group()

      # Prepare filtered CHIP Activities data
      data <- chip_data %>%
        dplyr::filter(filter == filtering) %>%
        dplyr::group_by(action, group) %>%
        dplyr::mutate(
          Responses = sum(Responses, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(as.character(category) == "1") %>%
        dplyr::select(y, group, subtitle, Responses, filter, action) %>%
        dplyr::rename(category = action) %>%
        pivot_data_group() %>%
        add_all_ldh_count(data_all = chip_data_all)

    } else {
      # Process data as usual for other filters
      data <- return_survey_weighted_counts(topic, filtering) %>%
        dplyr::select(category, group, y, subtitle, Responses) %>%
        tidyr::pivot_wider(names_from = group, values_from = c(y, Responses)) %>%
        dplyr::left_join(., add_all_counts_long(topic), by = "category") %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("Responses_"), ~ ifelse(is.na(.), 0, .))
        )%>%
        dplyr::filter(!category %in% c("Na"))
    }

  } #closes not All LHDs filter else

  # Display zero percentages instead of NA for CHA
  if (topic == "c7q147") {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("y_"),
          ~ tidyr::replace_na(.x, 0)
        )
      )
  }

  data <- data %>%
    dplyr::rename_with(standardize_column_names)

  return(data)

}


# Tables function--------
#' Function to create a non-time series table
#'
#' This function creates a reactable table for non-time series data, applying custom
#' column definitions, styles, and formatting based on the specified `topic`.
#'
#' @param data The data frame to display in the table.
#' @param category The category for grouping columns.
#' @param colors A vector of colors to use for the data bars.
#' @param topic The topic to customize the formatting and styles.
#' @return A reactable table object.
#' @import reactable
#' @import reactablefmtr
#' @importFrom scales percent_format
create_non_time_table <- function(data, category, colors, topic) {
  # Ensure 'category' is the first column
  data <- data %>%
    dplyr::select(category, everything())

  # Adjust data for specific topics
  if (topic %in% c("formal_part", "any_part")) {
    if (!"Type" %in% colnames(data)) {
      stop("The column 'Type' is missing in the data.")
    }
    # Explicit grouping for Type
    data <- data %>%
      dplyr::group_by(Type, .add = TRUE) %>%
      dplyr::ungroup() # Ensure data is not grouped when passed to reactable
  }

  # Debugging: Print data to verify it includes Type
  message("Data after grouping by 'Type':")
  print(head(data))


  # Helper for formatting numbers
  num_format <- function(x) {

    rounded_value <- if ((topic %in% c("fte_bypop", "employee_bypop"))) {
      round_up(x, 1)

    } else if (topic %in% c("c3q15", "rev_total")) {

      if (x >= 10000000) {

        round_up(x, -5)

      } else if ( 9999999 > x & x >= 100000 ) {

        round_up(x, -4)

      } else {

      round_up(x, -3)

      }

    } else {
      round_up(x, 0)  # no decimals

    }


    # rounded_value <- scales::label_comma()(rounded_value)

    if (topic == "jobs_total") {
      scales::label_comma()(rounded_value)
    } else if (topic == "b") {
      paste0(scales::label_comma()(rounded_value), " FTEs")
    } else if (topic == "fte_bypop") {
      paste0(rounded_value, " FTEs")
    } else if (topic == "c5q37") {
      paste0(rounded_value, " FTE")
    } else if (topic == "employee_bypop") {
      paste0(rounded_value, " Employees")
    } else if (topic == "c5q36") {
      paste0(rounded_value, " Total Employees")
    } else if (topic %in% c("c3q15", "totexp_percap", "rev_percap", "rev_total")) {
      scales::label_currency()(rounded_value)
    } else {
      ifelse(
        !is.na(x) & x > 0 & x < 0.5,
        paste0(round(x, 1), "%"),
        scales::percent_format(scale = 1)(rounded_value)  # Format as percentage with rounded value
      )
    }
  }

  # Handle y_All LHDs column
  all_lhds_col_name <- intersect(c("y_All LHDs", "y_All Lhds"), colnames(data))
  if (length(all_lhds_col_name) > 0) {
    all_lhds_col <- reactable::colDef(
      name = "All LHDs",
      align = "center",
      header = with_tooltip("All LHDs",
                            paste0("n = ", table_n_count(data, topic, 'Responses_All LHDs'))),
      cell = reactablefmtr::data_bars(
        data,
        text_position = "outside-end",
        max_value = ifelse(topic == "c3q15", 125000000,
                           ifelse(topic == "c5q37", 600,
                            ifelse(topic == "c5q36", 700,
                            ifelse(topic %in% c("totexp_percap", "rev_percap"), 200,
                                  ifelse(topic == "rev_total", 150000000,
                                         ifelse(topic == "jobs_total", 15000,
                                                ifelse(topic == "b", 21000, 120))))))),
        fill_color = naccho.viz::green,  # Always use naccho.viz::green
        background = 'none',
        number_fmt = num_format
      ),
      style = list(
        fontFamily = "Heebo",
        fontSize = "calc(10px + .2em + .1vmin)",
        lineHeight = "calc(10px + .2em + .1vmin)",
        padding = "calc(.2vh + 4px + .1em) calc(.2vw + 5px + .1em)",
        vAlign = "center"
      ),
      headerStyle = list(
        fontWeight = "bolder",
        textAlign = "center",
        textTransform = "uppercase",
        fontSize = "calc(10px + .2em + .5vmin)",
        lineHeight = "calc(10px + .2em + .5vmin)"
      ),
      minWidth = 100
    )
  } else {
    all_lhds_col <- NULL
  }

  # Handle selected_columns based on category
  if (category == "All LHDs") {
    if (!is.null(all_lhds_col_name)) {
      selected_columns <- all_lhds_col_name
    } else {
      stop("No column matching 'y_All LHDs' or 'y_All Lhds' found in the data.")
    }
  } else {
    group_definitions <- list(
      Rurality = list(columns = standardize_column_names(c("y_urban", "y_rural"))),
      Governance = list(columns = standardize_column_names(c("y_local", "y_shared", "y_state"))),
      `LHD size` = list(columns = standardize_column_names(c("y_small (<50,000)", "y_medium (50,000-499,999)", "y_large (500,000+)"))),
      Region = list(columns = standardize_column_names(c("y_midwest", "y_northeast", "y_south", "y_west")))
    )

    if (category %in% names(group_definitions)) {
      selected_columns <- group_definitions[[category]]$columns
      selected_columns <- selected_columns[selected_columns %in% colnames(data)]  # Only include existing columns
    } else {
      selected_columns <- grep("^y_", colnames(data), value = TRUE)
      selected_columns <- setdiff(selected_columns, "y_All LHDs")
    }

    # Ensure "All LHDs" column is included and renamed
    if (!is.null(all_lhds_col_name)) {
      selected_columns <- c(selected_columns, all_lhds_col_name)
    }
  }

  # Debugging messages for troubleshooting
  message("Selected columns: ", paste(selected_columns, collapse = ", "))

  # Ensure selected_columns is not empty
  if (length(selected_columns) == 0) {
    stop("No columns selected. Please check the data and category definitions.")
  }

  # Hide extra columns not needed
  hidden_columns <- setdiff(colnames(data), c("category", all_lhds_col_name, selected_columns, "Total"))
  hidden_col_definitions <- setNames(
    lapply(hidden_columns, function(col) {
      reactable::colDef(show = FALSE)
    }),
    hidden_columns
  )

  # Define columns for reactable


  if ("Type" %in% colnames(data)) {
    column_definitions <- list(
      category = reactable::colDef(
        name = "Category",
        align = "center",
        headerStyle = list(
          fontWeight = "bold",
          textAlign = "center",
          textTransform = "uppercase",
          fontSize = "calc(10px + .2em + .5vmin)",  # Font size in em
          lineHeight = "calc(10px + .2em + .5vmin)"
        ),
        style = list(
          textAlign = "left",
          fontFamily = "Heebo",
          fontSize = "calc(10px + .2em + .1vmin)",
          lineHeight = "calc(10px + .2em + .1vmin)",
          padding = "calc(.2vh + 4px + .1em) calc(.2vw + 5px + .1em)",
          vAlign = "center",
          borderRight = ".2em solid rgba(0, 0, 0, 0.1)"
        ),
        minWidth = 100
        # ,
        # width = "calc(20px + 2vw + .2em)"
      ),
      Type = reactable::colDef(
        name = "Type",
        align = "center",
        headerStyle = list(
          fontWeight = "bold",
          textAlign = "center",
          textTransform = "uppercase"
        )
      )
    )
  } else {
    column_definitions <- list(
      category = reactable::colDef(
        name = "Category",
        align = "center",
        headerStyle = list(
          fontWeight = "bold",
          textAlign = "center",
          textTransform = "uppercase",
          fontSize = "calc(10px + .2em + .5vmin)",  # Font size in em
          lineHeight = "calc(10px + .2em + .5vmin)"
        ),
        style = list(
          textAlign = "left",
          fontFamily = "Heebo",
          fontSize = "calc(10px + .2em + .1vmin)",
          lineHeight = "calc(10px + .2em + .1vmin)",
          padding = "calc(.2vh + 4px + .1em) calc(.2vw + 5px + .1em)",
          vAlign = "center",
          borderRight = ".2em solid rgba(0, 0, 0, 0.1)"
        ),
        minWidth = 100
        # ,
        # width = "calc(20px + 2vw + .2em)"
      )
    )
  }

  # Add selected_columns to column definitions
  column_definitions <- c(
    column_definitions,
    setNames(
      lapply(seq_along(selected_columns), function(i) {
        col <- selected_columns[i]
        col_name <- sub("^y_", "", col)
        reactable::colDef(
          name = if (col == all_lhds_col_name) "All LHDs" else stringr::str_replace_all(col_name, "_", " "),
          align = "center",
          header = with_tooltip(
            if (col == all_lhds_col_name) "All LHDs" else col_name,
            paste0("n = ", table_n_count(data, topic, paste0("Responses_", col_name)))
          ),
          headerStyle = list(
            fontWeight = "bold",
            textAlign = "center",
            textTransform = "uppercase",
            fontSize = "calc(10px + .2em + .5vmin)",  # Font size in em
            lineHeight = "calc(10px + .2em + .5vmin)"
          ),
          cell = reactablefmtr::data_bars(
            data,
            text_position = "outside-end",
            max_value = ifelse(topic == "c3q15", 125000000,
                               ifelse(topic == "c5q37", 600,
                                      ifelse(topic == "c5q36", 700,
                                             ifelse(topic %in% c("totexp_percap", "rev_percap"), 200,
                                                    ifelse(topic == "rev_total", 150000000,
                                                           ifelse(topic == "jobs_total", 15000,
                                                                  ifelse(topic == "b", 21000, 120))))))),
            fill_color = ifelse(col == all_lhds_col_name, naccho.viz::green, colors[i]),
            background = 'none',
            number_fmt = num_format
          ),
          style = list(
            fontFamily = "Heebo",
            fontSize = "calc(10px + .2em + .1vmin)",
            lineHeight = "calc(10px + .2em + .1vmin)",
            padding = "calc(.2vh + 4px + .1em) calc(.2vw + 5px + .1em)",
            vAlign = "center"
          ),
          minWidth = 100
          # ,
          # width = "calc(20px + 2vw + .2em)"
        )
      }),
      selected_columns
    )
  )

  # Add hidden columns to column definitions
  column_definitions <- c(column_definitions, hidden_col_definitions)



  # Generate reactable
  reactable::reactable(
    data = data,
    columns = column_definitions,
    groupBy = if (topic %in% c("formal_part", "any_part")) "Type" else NULL,
    defaultExpanded = TRUE,
    pagination = TRUE,
    paginateSubRows = TRUE,
    compact = TRUE,
    defaultPageSize = 8,
    filterable = FALSE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(6, 8, 10, 12),
    striped = FALSE,
    highlight = TRUE,
    searchable = FALSE,
    bordered = FALSE,
    resizable = TRUE

  )
}


#' Helper function to add tooltips
#'
#' This function adds tooltips to column headers for additional context.
#'
#' @param value The text to display.
#' @param tooltip The tooltip message to display on hover.
#' @param ... Additional arguments passed to the tooltip function.
#' @return A div with tooltip-enabled text.
#' @importFrom tippy tippy

with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy::tippy(value, tooltip, ...))
}


#' Calculate Summary Counts for Table Display
#'
#' This helper function computes the summary `n` count for a dataset based on a specified topic.
#' It dynamically handles different use cases, including returning a range of values (via `show_n`)
#' for certain topics or the total sum for others. The function is designed to be integrated into
#' table generation logic.
#'
#' @param data A data frame containing the data for which the `n` count is to be calculated.
#' @param topic A string specifying the topic, used to determine the calculation logic for the `n` count.
#' @param col_name An optional string specifying the column name for `Responses`. If not provided, defaults to `"Responses"`.
#' @return A numeric value or a string:
#' - If the topic requires a range (`"LHD (Directly)"`, `"Reduced/Expanded Services"`, etc.), uses `show_n` to return a string in the format `"min to max"`.
#' - Otherwise, returns the total sum of the `Responses` column as a numeric value.
#' @details
#' For topics requiring a range, the function relies on the `show_n` function, which calculates
#' and returns either a range (`"min to max"`) or a single value (if all values are the same).
#'
#' Topics that require a range:
#' - `"LHD (Directly)"`
#' - `"LHD (By Contract)"`
#' - `"Other Entity"`
#' - `"Not Available in Community"`
#' - `"c5q37"`
#' - `"fte_bypop"`
#' - `"c5q36"`
#' - `"employee_bypop"`
#' - `"Reduced/Expanded Services"`
#'
#' @examples
#' # Example data
#' data <- data.frame(Responses = c(10, 20, 30, 40))
#'
#' # Default sum for a generic topic
#' table_n_count(data, topic = "generic_topic")
#' #> 100
#'
#' # Range for specific topics
#' table_n_count(data, topic = "LHD (Directly)")
#' #> "10 to 40"
#'
#' # Using a custom column name
#' data <- data.frame(Responses_Custom = c(15, 25, 35, 45))
#' table_n_count(data, topic = "LHD (By Contract)", col_name = "Responses_Custom")
#' #> "15 to 45"
#'
#' @seealso [show_n()] for calculating ranges or single values dynamically.
#' @export
table_n_count <- function(data, topic, col_name = NULL) {
  # Check if col_name is provided, if not default to a generic Responses column
  if (is.null(col_name)) {
    col_name <- "Responses_All LHDs"
  }

  # Policy table: use the valid N from the None category
  if (topic == "Newc12q502") {

    none_n <- data %>%
      dplyr::filter(as.character(category) == "None") %>%
      dplyr::pull(dplyr::all_of(col_name))

    if (length(none_n) > 0 && any(!is.na(none_n))) {
      return(as.integer(none_n[which(!is.na(none_n))[1]]))
    }

    return(NA_integer_)
  }

  # Handle specific topics with variability
  if (topic %in% c(
    "LHD (Directly)", "LHD (By Contract)",
    "Other Entity", "Not Available in Community",
    "c5q37", "fte_bypop", "c5q36", "employee_bypop",
    "Reduced/Expanded Services", "c3q15", "totexp_percap",
    "rev_percap", "rev_total", "comm_channels_1", "comm_channels_2",
    "comm_channels_3","tech_use",
    "jobs_total", "field", "c12q260", "m9q240", "a", "b", "chachip",
    "medical", "dental", "behavioral", "chachip", "c7q149", "c7q147"
  )) {
    # Use `show_n` to calculate range or single value
    show_n(data[[paste0(col_name)]])
  } else if (topic %in% c("formal_part", "any_part")) {

    show_n(data[[col_name]])

  } else {
    # Default behavior: sum the Responses column
    sum(data[[paste0(col_name)]], na.rm = TRUE)
  }
}


#' Standardize Column Names
#'
#' This helper function standardizes column names by ensuring that columns
#' starting with "y_" have their second part capitalized (e.g., "y_urban" becomes "y_Urban").
#' Columns that do not match the "y_" pattern are left unchanged.
#'
#' @param columns A character vector of column names to be standardized.
#' @return A character vector of standardized column names where applicable.
#' @examples
#' # Example usage
#' columns <- c("y_urban", "y_rural", "Responses")
#' standardize_column_names(columns)
#' # [1] "y_Urban"    "y_Rural"    "Responses"
#'
#' columns <- c("y_local", "y_state", "group")
#' standardize_column_names(columns)
#' # [1] "y_Local"    "y_State"    "group"
#'
#' @export
standardize_column_names <- function(columns) {
  sapply(columns, function(col) {
    if (grepl("^(y_|Responses_).*All\\sLhds?$", col, ignore.case = TRUE)) {
      normalized <- gsub("\\s+", " ", stringr::str_replace(col, "All\\sLhds?$", "All LHDs"))
      return(normalized)
    }
    if (startsWith(col, "y_") || startsWith(col, "Responses_")) {
      parts <- strsplit(col, "_")[[1]]
      parts <- c(parts[1], stringr::str_to_title(parts[-1]))
      transformed <- paste(parts, collapse = "_")
      return(transformed)
    }
    return(col)
  }, USE.NAMES = FALSE)
}


