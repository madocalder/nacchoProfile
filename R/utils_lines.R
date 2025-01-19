#' Process Line Data for Local Health Departments (LHDs) Topics and Filters
#'
#' This function processes historical and current survey data on Local Health Departments (LHDs) for a variety of demographic and organizational topics.
#' It merges historical data with 2022 survey data, performs transformations on specified topic areas, and generates descriptive labels for visualization.
#'
#' @param option A character string specifying the topic to process. Supported options include:
#'   - "lboh" for local boards of health,
#'   - "age_cat" for executive age categories,
#'   - "c4q29" for executive gender identification,
#'   - "race" for executive race,
#'   - "ethnicity" for executive ethnicity,
#'   - "tenure" for mean executive tenure,
#'   - "field" for executive field of study,
#'   - "c4q34" for the highest degree earned by top executives.
#' @param filtering A character string representing the filter to apply, either "All LHDs" or specific regional or demographic filters such as "censusregion" or "urbanrural2022".
#' @param classifications Optional; a character string or list of additional classifications for data processing. Defaults to NULL.
#'
#' @return A processed data frame that combines historical and current data for the specified `option` topic. The returned data frame includes:
#'   - Descriptive labels in columns such as `legend_label` and `legend_title` for easy integration in visualizations.
#'   - Values for the current year as well as historical data, when applicable.
#'   - Aggregated and summarised percentages or counts based on the specified filter and topic.
#'
#' @details
#' This function performs topic-specific data processing steps, including:
#' - **Merging Historical and Current Data**: Combines historical `lines_data` with 2022 survey data.
#' - **Transformations**: Recalculates and recodes variables like age, race, gender, and field of study for consistency.
#' - **Labeling**: Generates descriptive labels to facilitate visualization, such as for percentage summaries.
#'
#' ## Supported Topics
#' - **Local Board of Health (lboh)**: Examines whether LHDs have a local board of health.
#' - **Executive Age (age_cat)**: Analyzes age categories of LHD top executives.
#' - **Gender (c4q29)**: Analyzes gender identification of top LHD executives.
#' - **Race and Ethnicity (race, ethnicity)**: Tracks race and ethnicity information for top LHD executives.
#' - **Tenure (tenure)**: Analyzes average tenure of top LHD executives.
#' - **Field of Study (field)**: Tracks the field of study for top LHD executives.
#' - **Highest Degree Earned (c4q34)**: Shows the highest academic degree earned by top LHD executives.
#'
#' @examples
#' # Example usage for processing data on local boards of health across all LHDs
#' data <- process_line_data("lboh", "All LHDs")
#'
#' # Example for analyzing executive age data filtered by urban-rural classifications
#' data <- process_line_data("age_cat", "urbanrural2022")
#'
#' @import dplyr
#' @export


process_line_data <- function(option, filtering, classifications = NULL) {
 req(option != "c2q501")
  # Initial debugging
  print(paste("Option:", option, "Filtering:", filtering))


#All LHD CASES------------
  if (filtering == "All LHDs") {
##Local board of health------------------
    if (option == "lboh") {
      ###historical data-----------
      hist_data <- lines_data %>%
        dplyr::filter(topic == "lboh",
                      filter == "All LHDs",
                      category %in% c("Yes", "No"))%>%
        dplyr::mutate(y = round_up((100*as.numeric(y)), 1))

      # View(hist_data)

      ### 2022 data-------------
      current_data <- return_survey_weighted_counts("lboh") %>%
        dplyr::select(Question, y, category, group, Group, Responses)%>%
        dplyr::mutate(category =dplyr::case_when(
          category %in% c("Advisory", "Governing") ~ "Yes",  # Recode both variations to "Yes"
          category == "None" ~ "No",
          FALSE ~ category
        ))%>%
        dplyr::group_by(category, group, Group, Question) %>%
        dplyr::reframe(y = sum(y),
                         Responses = sum(Responses))%>%
        dplyr::mutate(Responses = sum(Responses))

      current_data$year <- 2022
      # View(current_data)

      ###combine the two datasets
      combined <- full_join(current_data, hist_data,
                       by = c("Question" = "topic",
                              "Group" = "filter",
                              "group" = "group",
                              "y" = "y",
                              "year" = "year",
                              "category" = "category",
                              "Responses" = "Responses"
                       ))%>%
        arrange(year)%>%
          dplyr::mutate(
            legend_label =dplyr::case_when(
              category == "Yes" ~ "Local Board of Health",
              category == "No" ~ "No Local Board of Health",
              FALSE ~ category
            ),
            base =dplyr::case_when(
              category == "Yes" ~ "had a Local Board of Health",
              category == "No" ~ "did not have a Local Board of Health",
              FALSE ~ category
            ),
            one = paste(y),
            two = paste("% of LHDs", tolower(base)
              )
          )


      } else if (option == "age_cat"){
  ##Top executive age---------
        ### historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "age_cat",
                        filter == "All LHDs")%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1)
          ) %>%
          dplyr::group_by(category, group, year, filter, topic) %>%
          dplyr::summarise(y = sum(y),
                           Responses = unique(Responses))

       ### 2022 data-------------
        current_data <- return_survey_weighted_counts(option) %>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(category =dplyr::case_when(
            category == "40-49 Years" ~ "40-49",
            category == "50-59 Years" ~ "50-59",
            category == "60-69 Years" ~ "60-69",
            category == "70 Or Older" ~ "70+",
            TRUE ~ category  # Handle any other cases, though we don't expect others
          ),
          year = 2022,
          filter = "All LHDs",
          Responses = sum(Responses))

        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year, group) %>%
          dplyr::mutate(
            legend_label = paste(category, "years"),
            one = paste(y),
            two = paste("% of LHD executives were", tolower(legend_label), "old")
          )

        combined$legend_label <- factor(combined$legend_label,
                                        levels = c("Under 40 years", "40-49 years",
                                                   "50-59 years", "60-69 years",
                                                   "70+ years"))

        combined <- combined %>%
          arrange(legend_label)

      } else if(option == "c4q29"){
##Top executive gender -------

          ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "c4q29",
                        filter == "All LHDs") %>%
          dplyr::group_by(year, category, group, filter, topic)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1),
                 Responses = unique(Responses),
            category =dplyr::case_when(
              category == "Prefer not to answer" ~ "Not specified",
              category == "Other" ~ "Not specified",
              TRUE ~ category  # Handle any other cases, though we don't expect others
            )
          ) %>%
          dplyr::summarise(y = sum(y),
                           Responses = unique(Responses))

        # View(hist_data)


        ### 2022 data-------------
        current_data <- return_survey_weighted_counts(option)%>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(
            category =dplyr::case_when(
              category == "Prefer Not To Answer" ~ "Not specified",
              category == "Other" ~ "Not specified",
              TRUE ~ category  # Handle any other cases, though we don't expect others
            ),
            year = 2022,
            filter = "All LHDs",
            Responses = sum(Responses))

        # View(current_data)

        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year, category) %>%
          dplyr::mutate(
            legend_label = paste(category),
            one = paste(y),
            two = paste("% of top LHD executives identified as", tolower(category)),
          )%>%
          dplyr::filter(category %in% c("Male", "Female"))


      } else if (option == "race") {
  ##Top exec race------------
        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "race",
                        filter == "All LHDs") %>%
          dplyr::group_by(year, category, group, filter, topic)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1),
            category =dplyr::case_when(
              category %in% c("Some other race", "American Indian/Alaska Native",
                              "Black/African American", "Native Hawaiian/Other Pacific Islander",
             "Asian" ) ~ "Race other than White",
             category == "White" ~ "White",
            FALSE ~ category  # Handle any other cases, though we don't expect others
            ),
            Responses = unique(Responses))%>%
          dplyr::group_by(category, group, filter, topic, year)%>%
          dplyr::reframe(y = sum(y),
                           Responses = unique(Responses))


        ### 2022 data-------------
        current_data <- return_survey_weighted_counts("race") %>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(category =dplyr::case_when(
            category %in% c("American Indian Or Alaska Native",
                            "Black Or African American",
                            "Native Hawaiian Or Other Pacific Islander",
                            "Asian", "Mixed Identity Or Other Race") ~ "Race other than White",
            category == "White" ~ "White",
            FALSE ~ category),  # Handle any other cases, though we don't expect others
            year = 2022,
            filter = "All LHDs")%>%
          dplyr::group_by(category, group, filter, year, Question)%>%
          dplyr::reframe(y = sum(y),
                         Responses = sum(Responses))%>%
          dplyr::mutate(Responses = sum(Responses))


        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year, category) %>%
          dplyr::filter(category != "Na") %>%
          dplyr::mutate(
            legend_label = paste(category),
            one = paste(y),
            two = paste("% of top LHD executives identified as", category),
          )

        # View(combined)
      } else if (option == "ethnicity"){

        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "ethnicity",
                        filter == "All LHDs") %>%
          dplyr::group_by(year, category, group, filter, topic)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1),
            Responses = unique(Responses))

        ### 2022 data-------------
        current_data <- return_survey_weighted_counts("ethnicity") %>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(
            y = round_up(as.numeric(y)),
            year = 2022,
            filter = "All LHDs",
            Responses = sum(Responses)
          )

        combined <- dplyr::full_join(current_data, hist_data,
                                     by = c("Question" = "topic",
                                            "group" = "group",
                                            "filter" = "filter",
                                            "y" = "y",
                                            "year" = "year",
                                            "category" = "category",
                                            "Responses" = "Responses"
                                     )) %>%
          dplyr::arrange(year, category) %>%
          dplyr::filter(!category %in% c("Na", "NA", "Prefer Not To Answer")) %>%
          dplyr::mutate(
            legend_label = paste(category),
            one = paste(y),
            two = paste("% of top LHD executives were", tolower(category)),
          )

      } else if (option == "tenure"){
##Top exec tenure--------
       ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "tenure",
                        filter == "All LHDs")
        # %>%
        #   dplyr::group_by(year, category, group, filter, topic)

       ### 2022 data-------------
        current_data <- return_survey_weighted_statistics("tenure") %>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(
            group = category,
            category = "Mean",
            year = 2022,
            filter = "All LHDs",
            y = as.character(y))

        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "Responses" = "Responses"
                              )) %>%
          dplyr::mutate(y = round_up(as.numeric(y)))%>%
          arrange(year) %>%
          dplyr::mutate(
            legend_label = paste(category),
            one = paste(y),
            two = paste(" years was the average tenure of top LHD executives"))

        ##Top exec field of study --------
      } else if (option == "field"){
        ###create other 2016 rows
        # other_2016 <- lines_data %>%
        #   dplyr::filter(topic == "field",
        #                 year == 2016,
        #                 filter == "All LHDs") %>%  # Only for 2016
        #   dplyr::group_by(year, group, filter, topic) %>%
        #   dplyr::summarise(
        #     y_sum = sum(as.numeric(y), na.rm = TRUE)) %>%
        #   dplyr::mutate(
        #     y = round_up((100*as.numeric(1 - y_sum)))
        #   ) %>%
        #   dplyr::select(year, category, group, filter, topic, y)  # Select relevant columns

        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "field",
                        filter == "All LHDs",
                        category != "Specialized Degree",
                        category != "None of the above") %>%
          dplyr::group_by(year, category, group, filter, topic)%>%
          dplyr::mutate(y = round_up((100*as.numeric(y)), 1),
                        Responses = unique(Responses),
                        year = as.integer(year))

        # hist_data <- rbind(hist_data, other_2016)

        ###2022 data-------------
        current_data <- spec_data_all %>%
          dplyr::filter(category != "Specialized Degree") %>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(
            filter = filtering,
            year = 2022,
            Question = "field",
            y = round_up(as.numeric(y)))


        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year) %>%
          dplyr::mutate(
            legend_label = paste(category),
            tt_base =dplyr::case_when(
              category == "Other Field" ~ "in a field other than Public Health, Nursing, or Medical",
              category %in% c("Public Health", "Nursing") ~ paste("in the field of", category),
              TRUE ~ paste("in the", category, "field")
            ),
            one = paste(y),
            two = paste("% of LHD top executives studied", tt_base)
          )

        # %>%
        #   dplyr::filter(category != "Other Field")

      } else if (option == "c4q34"){
##Top executive highest degree earned----------
        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "c4q34",
                        filter == "All LHDs") %>%
          dplyr::group_by(year, category, group, filter, topic)%>%
          dplyr::mutate(y = round_up((100*as.numeric(y)), 1),
                        Responses = unique(Responses))


        ### 2022 data-------------
        current_data <- return_survey_weighted_counts("c4q34") %>%
          dplyr::select(Question, y, category, group, Responses) %>%
          dplyr::mutate(
            year = 2022,
            filter = "All LHDs",
            Responses = sum(Responses),
            y = round_up(as.numeric(y)))


        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "Responses" = "Responses"
                              )) %>%
          dplyr::mutate(y = round_up(as.numeric(y))) %>%
          arrange(year) %>%
          dplyr::mutate(
            legend_label = paste(category),
            one = paste(y),
            two =dplyr::case_when(
              category == "Associates" ~ paste("% of LHD top executives' highest
                                               degree earned was an",
                                               category, "degree"),
              category %in% c("Masters", "Bachelors") ~ paste("% of LHD top
                                                        executives' highest degree
                                                              earned was a",
                                                              category, "degree"),
              TRUE ~  paste("% of LHD top executives' highest degree earned was a",
                            category))
          )
      } else if (option %in% c("c5q36", "c5q37")){

        ###2022 data-----
        current_data_1 <- nacchoProfile::return_survey_weighted_statistics(option) %>%
          dplyr::select(Question, category, group, y, Responses)%>%
          dplyr::mutate(year = 2022,
                        filter = "All LHDs",
                        category = "Mean",
                        Responses = sum(Responses),
                        y = round_up(as.numeric(y)))

        current_data_2 <- nacchoProfile::return_survey_weighted_statistics(option) %>%
          dplyr::select(Question, category, group, Quantile_q50, Responses)%>%
          dplyr::mutate(year = 2022,
                        filter = "All LHDs",
                        category = "Median",
                        Responses = sum(Responses),
                        y = round_up(as.numeric(Quantile_q50))) %>%
          dplyr::select(Question, category, group, y, filter, year, Responses )


        current_data <- dplyr::full_join(current_data_1, current_data_2)


        ###historical-----
        hist_data <- lines_data %>%
          dplyr::filter(topic == option,
                        filter == "All LHDs",
                        category %in% c("Mean", "Median"))%>%
          dplyr::mutate(y = round_up(as.numeric(y)))

        if (option == "c5q37"){

        ###combine
        combined <- dplyr::full_join(current_data, hist_data,
                                     by = c("Question" = "topic",
                                            "group" = "group",
                                            "filter" = "filter",
                                            "y" = "y",
                                            "year" = "year",
                                            "category" = "category",
                                            "Responses" = "Responses"
                                     )) %>%
          dplyr::mutate(y = round_up(as.numeric(y)),
                        legend_label = paste(category),
                        one = paste("the", tolower(category),  "number of FTEs across all LHDs was "),
                        two = paste(y)) %>%
          dplyr::arrange(year)
        } else {
          combined <- dplyr::full_join(current_data, hist_data,
                                       by = c("Question" = "topic",
                                              "group" = "group",
                                              "filter" = "filter",
                                              "y" = "y",
                                              "year" = "year",
                                              "category" = "category",
                                              "Responses" = "Responses"
                                       )) %>%
            dplyr::mutate(y = round_up(as.numeric(y)),
                          legend_label = paste(category),
                          one = paste("the", tolower(category), "number of total employees across LHDs was"),
                          two = paste(" ", y)) %>%
            dplyr::arrange(year)
        }

      } else if (option %in%  c("fte_bypop", "employee_bypop")){

        ###2022 data-----
        current_data <- nacchoProfile::return_survey_weighted_statistics(option) %>%
          dplyr::select(Question, category, group, y, Responses ) %>%
          dplyr::mutate(y = as.numeric(y),
                        year = 2022,
                        Responses = sum(Responses),
                        filter = "All LHDs",
                        category = dplyr::case_when(Question == "fte_bypop" ~ "FTEs per 10,000 people",
                                                    TRUE ~ "Employees per 10,000 people")
                        )
#
#         current_data_2 <- nacchoProfile::return_survey_weighted_statistics(option) %>%
#           dplyr::select(Question, category, group, y, Responses )%>%
#           dplyr::mutate(year = 2022,
#                         Responses = sum(Responses),
#                         y = round_up(as.numeric(y)))
#
#         current_data <- dplyr::full_join(current_data_1, current_data_2)%>%
#           dplyr::mutate(category = "FTEs per 10,000 people",
#                         filter = "All LHDs")


        ###historical-----
        hist_data <- lines_data %>%
          dplyr::filter(topic == option,
                        filter == "All LHDs") %>%
          dplyr::mutate(y = as.numeric(y))%>%
          dplyr::select(topic, group, filter, y, year, category, Responses)

        ###combine
        combined <- dplyr::full_join(current_data, hist_data,
                                     by = c("Question" = "topic",
                                            "group" = "group",
                                            "filter" = "filter",
                                            "y" = "y",
                                            "year" = "year",
                                            "category" = "category",
                                            "Responses" = "Responses"
                                     )) %>%
          dplyr::mutate(y = as.numeric(y),
                        legend_label = paste(category),
                        tt = dplyr::case_when(Question == "fte_bypop" ~ "FTEs",
                                            TRUE ~ "employees"),
                        one = paste(" across all LHDS, the average number of",
                                    tt,  "per 10,000 people was "),
                        two = paste(y)) %>%
          dplyr::arrange(year)


      } else if (option == "b"){

        current_vars <-  paste0(classifications, "b")
        hist_vars <- paste0("new", classifications, "b")
        selected_data <- get(paste0("classification_data_b"))


        current_data <- classification_data_b %>%
          dplyr::filter(Question %in% current_vars,
                        filter == "All LHDs")%>%
          dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(Question)]),
                        group = dplyr::case_when(group %in% c("13", "All Lhds") ~ "All LHDs",
                                                 TRUE ~ group),
                        year = 2022,
                        Responses = unique(Responses),
                        y = round_up(as.numeric(y)))%>%
          dplyr::select(Question, category, y, filter, group, year, Responses)



        hist_data <- lines_data %>%
          dplyr::filter(topic %in% hist_vars) %>%
          dplyr::mutate(category = stringr::str_to_title(q_remap[as.character(topic)]),
                        y = round_up(as.numeric(y)))


        combined <- dplyr::full_join(current_data, hist_data,
                                     by = c("Question" = "topic",
                                            "group" = "group",
                                            "filter" = "filter",
                                            "y" = "y",
                                            "year" = "year",
                                            "category" = "category",
                                            "Responses" = "Responses"
                                     )) %>%
          dplyr::mutate(y = round_up(as.numeric(y)) ,
                        legend_label = paste("Total", category, "FTEs"),
                        one = paste("LHDs had a total of "),
                        two = paste(scales::label_comma()(y), tolower(category), "FTEs")) %>%
          dplyr::arrange(year)

      } else if (option %in% c("health_part", "comm_part", "gov_part")) {

        topic <- if (option == "health_part"){
          "Healthcare Partnerships"
        } else if (option == "comm_part"){

          "Community-Based Partnerships"

        } else {
         "Government Partnerships"
        }

        combined <- partnerships %>%
          dplyr::filter(group == topic)


      } else if (option == "totexp_percap"){


      combined <- lines_data %>%
        dplyr::filter(topic == "totexp_percap")


      } else if (option == "budget_changes") {
        combined <- lines_data %>%
          dplyr::filter(topic == "budget_changes")

       } else {
          stop("ERROR: No matching option identified for All LHDs") }



    } else {
#NON ALL LHD CASES-------------
      ## Local Board of Health ----------
      if (option %in% c("lboh")){
        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "lboh",
                        filter == filtering,
                        category == "Yes")%>%
          dplyr::mutate(y = round_up((100*as.numeric(y)), 1))

        # View(hist_data)

        ### 2022 data-------------
        current_data <- return_survey_weighted_counts(option, filtering) %>%
          dplyr::select(Question, y, category, group, Group, subtitle, Responses)%>%
          dplyr::group_by(category, group, Group, Question, subtitle) %>%
          dplyr::mutate(category =dplyr::case_when(
            category %in% c("Advisory", "Governing") ~ "Yes",  # Recode both variations to "Yes"
            category == "None" ~ "No",
            FALSE ~ category
          ))%>%
          dplyr::summarise(y = sum(y),
                           Responses = sum(Responses)) %>%
          dplyr::ungroup()%>%
          dplyr::mutate(Responses = sum(Responses))%>%
          dplyr::filter(category == "Yes")

        current_data$year <- 2022
        current_data$filter <- filtering

        # View(current_data)

        ### Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year, group) %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            one = paste(y),
            two =dplyr::case_when(
              category == "Yes" ~ paste("% of", tolower(group_mod), "LHDs had a Local Board of Health"),
              category == "No" ~  paste("% of", tolower(group_mod), "LHDs did not have a Local Board of Health"),
              FALSE ~ category  # Handle any other cases
            ),
            legend_label = paste(group),
            legend_title = paste("LHDs with a Local Board of Health Over Time")
            )%>%
          arrange(year, group, one)


        ##Executive age-------------
      } else if (option == "age_cat"){

        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "age_cat",
                        filter == filtering)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1)
          ) %>%
          dplyr::group_by(category, group, year, filter, topic, subtitle) %>%
          dplyr::summarise(y = sum(y),
                           Responses = unique(Responses))


        ### 2022 data-------------
        current_data <- return_survey_weighted_counts(option, filtering) %>%
          dplyr::select(Question, y, category, group, subtitle, Responses) %>%
          dplyr::mutate(category =dplyr::case_when(
            category == "40-49 Years" ~ "40-49",
            category == "50-59 Years" ~ "50-59",
            category == "60-69 Years" ~ "60-69",
            category == "70 Or Older" ~ "70+",
            TRUE ~ category  # Handle any other cases, though we don't expect others
          ),
          year = 2022,
          filter = filtering,
          Responses = sum(Responses))

        ### Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year, group) %>%
          dplyr::mutate(category = dplyr::case_when(
            category == "Under 40" ~ "Under 40",
            TRUE ~ "40 and over"
          ))%>%
          dplyr::group_by(category, year, group, filter, subtitle)%>%
          dplyr::summarise(y=sum(y),
                           Responses = unique(Responses))%>%
          dplyr::filter(category == "Under 40") %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            cat_base = dplyr::case_when(
              category == "40 and over" ~ "40+",
              TRUE ~ category
            ),
            legend_title = paste("Top LHD Executives Under 40 Over Time"),
            legend_label = paste(group),
            one = paste(y),
            two = paste("% of", tolower(group_mod), "LHD executives were", tolower(cat_base), " years old"),
          )


##Top executive gender---------
      } else if (option == "c4q29"){
        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "c4q29",
                        filter == filtering) %>%
          dplyr::group_by(year, category, group, filter, topic, subtitle)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1),
            category =dplyr::case_when(
              category == "Prefer not to answer" ~ "Not specified",
              category == "Other" ~ "Not specified",
              TRUE ~ category  # Handle any other cases, though we don't expect others
            )
          ) %>%
          dplyr::reframe(y = sum(y),
                           Responses = unique(Responses))

        # View(hist_data)


        ### 2022 data-------------
        current_data <- return_survey_weighted_counts(option, filtering)%>%
          dplyr::select(Question, y, category, group, subtitle, Responses) %>%
          dplyr::mutate(
            category =dplyr::case_when(
              category == "Prefer Not To Answer" ~ "Not specified",
              category == "Other" ~ "Not specified",
              TRUE ~ category  # Handle any other cases, though we don't expect others
            ),
            year = 2022,
            filter = filtering,
            Responses = sum(Responses))

        # View(current_data)

        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          arrange(year, category) %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            legend_title = paste(category, "Top LHD Executives Over Time"),
            legend_label = paste(group),
            one = paste(y),
            two = paste("% of top executives from", tolower(group_mod), "LHDs identified as", tolower(category)),
          )%>%
          dplyr::filter(category %in% c("Female"))

      } else if (option == "race"){
##Top executive race ----------
       ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "race",
                        filter == filtering) %>%
          dplyr::group_by(year, category, group, filter, topic, subtitle)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1),
          category = dplyr::case_when(
              category %in% c("American Indian Or Alaska Native",
                              "Black Or African American",
                              "Native Hawaiian Or Other Pacific Islander",
                              "Asian", "Mixed Identity Or Other Race") ~ "Race other than White",
              category == "White" ~ "White",
              FALSE ~ category))


        ### 2022 data-------------
        current_data <- return_survey_weighted_counts(option, filtering) %>%
          dplyr::select(Question, y, category, group, subtitle, Responses) %>%
          dplyr::mutate(category =dplyr::case_when(
            category %in% c("American Indian Or Alaska Native",
                            "Black Or African American",
                            "Native Hawaiian Or Other Pacific Islander",
                            "Asian", "Mixed Identity Or Other Race") ~ "Race other than White",
            category == "White" ~ "White",
            FALSE ~ category),  # Handle any other cases, though we don't expect others
            year = 2022,
            filter = filtering)%>%
          dplyr::group_by(category, Question, group, filter, year, subtitle)%>%
          dplyr::reframe(y = sum(y),
                           Responses = sum(Responses))%>%
          dplyr::mutate(Responses = sum(Responses)) %>%
          dplyr::filter(category != is.na(category))

# View(current_data)
        # Combine data and create more descriptive legend labels
        combined <- dplyr::full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          dplyr::arrange(year, category) %>%
          dplyr::filter(category == "Race other than White") %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            legend_title = paste("Non-white Top LHD Executives Over Time"),
            legend_label = paste(group),
            one = paste(y),
            two =
              # dplyr::case_when(
              # category == "White" ~ paste("% of top executives from",
              #                             tolower(group), "LHDs identified as", category),
              # TRUE ~
                paste("% of top executives from",
                           tolower(group_mod), "LHDs identified as a race other than White")
          )
        # )
# View(combined)

      } else if (option == "ethnicity"){

        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "ethnicity",
                        filter == filtering) %>%
          dplyr::group_by(year, category, group, filter, topic, subtitle)%>%
          dplyr::mutate(
            y = round_up((100*as.numeric(y)), 1),
            Responses = unique(Responses))

        ### 2022 data-------------
        current_data <- return_survey_weighted_counts("ethnicity", filtering) %>%
          dplyr::select(Question, y, category, group, subtitle, Responses) %>%
          dplyr::mutate(
            y = round_up(as.numeric(y)),
            year = 2022,
            filter = filtering,
            Responses = sum(Responses)
          )
        #   dplyr::group_by(category, group, Question, year, filter)%>%
        #   dplyr::summarise(y = sum(y))

        if (filtering == "c0govcat") {
          current_data <- current_data %>%
            dplyr::add_row(
              y = 0,
              category = "Hispanic/Latino",
              Question = "ethnicity",
              group = "Shared",
              filter = filtering,
              year = 2022,
              subtitle = "by Governance",
              Responses = unique(current_data$Responses)
            )
        }


        # View(current_data)

        combined <- dplyr::full_join(current_data, hist_data,
                                     by = c("Question" = "topic",
                                            "group" = "group",
                                            "filter" = "filter",
                                            "y" = "y",
                                            "year" = "year",
                                            "category" = "category",
                                            "subtitle" = "subtitle",
                                            "Responses" = "Responses"
                                     )) %>%
          dplyr::arrange(year, category) %>%
          dplyr::filter(category == "Hispanic/Latino",
          !category %in% c("Na", "Prefer Not To Answer")) %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            legend_title = paste(category, "Top LHD Executives Over Time"),
            legend_label = paste(group),
            one = paste(y),
            two = paste("% of top LHD executives from", tolower(group_mod), "LHDs were Hispanic and/or Latino")
          )

# View(combined)

      } else if (option == "tenure") {

        ##Top exec tenure--------
        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "tenure",
                        filter == filtering) %>%
          dplyr::group_by(year, category, group, filter, topic, subtitle)

        ### 2022 data-------------
        current_data <- return_survey_weighted_statistics("tenure", filtering) %>%
          dplyr::select(Question, y, category, group, subtitle, Responses) %>%
          dplyr::mutate(
            group = category,
            category = "Mean",
            year = 2022,
            filter = filtering,
            y = as.character(y),
            Responses = sum(Responses))


        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          dplyr::mutate(y = round_up(as.numeric(y)))%>%
          arrange(year) %>%
          dplyr::mutate(
          group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            legend_label = paste(group),
            one = paste(y),
            two = paste(" years was the average tenure of top executives at",
                       tolower(group_mod), "LHDs"))

        ##Top exec field of study --------
      } else if (option == "field"){
        #   ###create other 2016 rows
        # other_2016 <- lines_data %>%
        #   dplyr::filter(topic == "field",
        #                 year == 2016,
        #                 filter == filtering) %>%  # Only for 2016
        #   dplyr::group_by(year, group, filter, topic, subtitle) %>%
        #   dplyr::summarise(
        #     y_sum = round_up(sum(as.numeric(y), na.rm = TRUE))) %>%
        #   dplyr::mutate(
        #     y = round_up((100*as.numeric(1 - y_sum)))
        #   ) %>%
        #   dplyr::select(year, category, group, filter, topic, y, subtitle)  # Select relevant columns

       ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "field",
                        filter == filtering,
                        category == "None of the above") %>%
          dplyr::group_by(year, category, group, filter, topic, subtitle)%>%
          dplyr::mutate(
                        y = round_up((100*(1-as.numeric(y))), 1),
                        category = "Specialized Degree",
                        year = as.integer(year))

        # hist_data <- rbind(hist_data, other_2016)

        ###2022 data-------------
        current_data <- spec_data %>%
          dplyr::filter(filter == filtering,
                        Question == "spec_combined") %>%
          dplyr::select(Question, y, category, group, filter, subtitle, Responses) %>%
          dplyr::mutate(
            year = 2022,
            Question = "field",
            y = round_up(as.numeric(y)),
            Responses = unique(Responses))


        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          # dplyr::mutate(
          #   category =dplyr::case_when(
          #     category %in% c("Public Health", "Nursing", "Medical") ~ "Specialized Degree",
          #     TRUE ~ category
          #   )) %>%
          # dplyr::group_by(category, group, filter, year, subtitle) %>%
          # dplyr::summarise(y = )%>%
          # dplyr::filter(category == "Specialized Degree") %>%
          dplyr::arrange(year) %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            legend_label = paste(group),
            legend_title = "Top LHD Executives with Specialized Degrees Over Time",
            two = paste("% of top executives from", tolower(group_mod), "LHDs had a Medical, Public Health, and/or Nursing degree"),
            one = paste(y)
          )
        # %>%
        #   dplyr::filter(category != "Other Field")

      } else if (option == "c4q34"){
  ##Top exec highest degree earned-------
        ###historical data-----------
        hist_data <- lines_data %>%
          dplyr::filter(topic == "c4q34",
                        filter == filtering) %>%
          dplyr::group_by(year, category, group, filter, topic, subtitle)%>%
          dplyr::mutate(y = round_up((100*as.numeric(y)), 1),
                        Responses = unique(Responses))

        ### 2022 data-------------
        current_data <- return_survey_weighted_counts("c4q34", filtering) %>%
          dplyr::select(Question, y, category, group, subtitle, Responses) %>%
          dplyr::mutate(
            year = 2022,
            filter = filtering,
            y = round_up(as.numeric(y)),
            Responses = sum(Responses))

        # Combine data and create more descriptive legend labels
        combined <- full_join(current_data, hist_data,
                              by = c("Question" = "topic",
                                     "group" = "group",
                                     "filter" = "filter",
                                     "y" = "y",
                                     "year" = "year",
                                     "category" = "category",
                                     "subtitle" = "subtitle",
                                     "Responses" = "Responses"
                              )) %>%
          dplyr::mutate(y = round_up(as.numeric(y))) %>%
          arrange(year) %>%
          dplyr::mutate(
            group_mod = dplyr::case_when(
              filtering == "censusregion" ~ paste0(group, "ern"),
              TRUE ~ group
            ),
            category = dplyr::case_when(
              !category %in% c("Associates", "Bachelors") ~ "Graduate",
              TRUE ~ "No Graduate"
            )
          )%>%
          dplyr::filter(category == "Graduate")%>%
          dplyr::group_by(category, group, filter, year, subtitle, group_mod)%>%
          dplyr::summarise(y = sum(y),
                           Responses = unique(Responses))%>%
          dplyr::mutate(legend_label = paste(group),
                        legend_title = paste("LHD Top Executives with Graduate Degrees Over Time"),
                        one = paste(y),
                        two = paste("% of top executives from", tolower(group_mod),
                                    "LHDs had at least one graduate degree"))

        # View(combined)

      } else if(option %in% c("c5q36", "c5q37")){
# # option <- "c5q36"
# # filtering <- "c0popcat3"
#         ###2022 data-----
#         current_data_1 <- nacchoProfile::return_survey_weighted_statistics(option, filtering) %>%
#           dplyr::select(Question, category, Group, y )%>%
#           dplyr::mutate(year = 2022,
#                         filter = filtering,
#                         category = "Mean",
#                         y = as.numeric(y),
#                         group = Group)%>%
#           dplyr::select(Question, category, group, y, filter, year )
#
#         current_data_2 <- nacchoProfile::return_survey_weighted_statistics(option, filtering) %>%
#           dplyr::select(Question, category, Group, Quantile_q50 )%>%
#           dplyr::mutate(year = 2022,
#                         filter = filtering,
#                         category = "Median",
#                         y = as.numeric(Quantile_q50),
#                         group = Group)%>%
#           dplyr::select(Question, category, group, y, filter, year )
#
#
#         current_data <- dplyr::full_join(current_data_1, current_data_2)


        ###current-----
        current_data <- nacchoProfile::return_survey_weighted_statistics(option, filtering)%>%
          dplyr::select(Question, Group, category, y, Responses, subtitle) %>%
          dplyr::mutate(year = 2022,
                        category = "Mean",
                        group = Group,
                        y = round_up(as.numeric(y)),
                        filter = filtering,
                        Responses = sum(Responses))%>%
          dplyr::select(Question, group, filter, y, year, category, Responses, subtitle)

        ###historical-----
        hist_data <- lines_data%>%
          dplyr::filter(topic == option,
                        filter == filtering,
                        category == "Mean") %>%
          dplyr::mutate(y = round_up(as.numeric(y)))

        if (option == "c5q37"){
        ###combine
        combined <- dplyr::full_join(current_data, hist_data,
                                     by = c("Question" = "topic",
                                            "group" = "group",
                                            "filter" = "filter",
                                            "y" = "y",
                                            "year" = "year",
                                            "category" = "category",
                                            "Responses" = "Responses",
                                            "subtitle" = "subtitle"
                                     )) %>%
          dplyr::mutate(y = round_up(as.numeric(y)),
                        group_mod = dplyr::case_when(
                          filtering == "censusregion" ~ paste0(group, "ern"),
                          TRUE ~ group
                        ),
                        legend_label = paste(group),
                        one = paste(tolower(group_mod), "LHDs had a", tolower(category), "of "),
                        two = paste(y, " FTEs")) %>%
          dplyr::arrange(year)
        } else {
          ###combine
          combined <- dplyr::full_join(current_data, hist_data,
                                       by = c("Question" = "topic",
                                              "group" = "group",
                                              "filter" = "filter",
                                              "y" = "y",
                                              "year" = "year",
                                              "category" = "category",
                                              "Responses" = "Responses",
                                              "subtitle" = "subtitle"
                                       )) %>%
            dplyr::mutate(y = round_up(as.numeric(y)),
                          group_mod = dplyr::case_when(
                            filtering == "censusregion" ~ paste0(group, "ern"),
                            TRUE ~ group
                          ),
                          legend_label = paste(group),
                          one = paste(tolower(group_mod),"LHDs had an average of "),
                          two = paste(y, " employees")) %>%
            dplyr::arrange(year)
        }
       } else if(option == "fte_bypop"){

         ###2022 data-----
         current_data <- nacchoProfile::return_survey_weighted_statistics(option, filtering) %>%
           dplyr::select(Question, category, group, y, Responses)%>%
           dplyr::mutate(y = as.numeric(y),
                         year = 2022,
                         Responses = sum(Responses),
                         filter = filtering,
                         group = category)


         ###historical-----
         hist_data <- lines_data %>%
           dplyr::filter(topic == option,
                         filter == filtering) %>%
           dplyr::mutate(y = as.numeric(y),
                         category = group)%>%
           dplyr::select(topic, group, filter, y, year, category, Responses)

         ###combine
         combined <- dplyr::full_join(current_data, hist_data,
                                      by = c("Question" = "topic",
                                             "group" = "group",
                                             "filter" = "filter",
                                             "y" = "y",
                                             "year" = "year",
                                             "category" = "category",
                                             "Responses" = "Responses"
                                      )) %>%
           dplyr::mutate(y = as.numeric(y),
                         group_mod = dplyr::case_when(
                           group == "Small (<50,000)" ~ "small",
                           group == "Medium (50,000-499,999)" ~ "medium",
                           TRUE ~ "large"
                         ),
                         legend_label = paste(category),
                         one = paste(" across ", group_mod, " LHDs, the average number of FTEs per 10,000 people was "),
                         two = paste(y)) %>%
           dplyr::arrange(year)




      } else {
          stop("ERROR: No matching option identified for non-All LHDs")


    }

    }


    combined$year <- as.factor(combined$year)

  return(combined)
}
