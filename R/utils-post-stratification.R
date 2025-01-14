#' Calculate Weights
#'
#' @description Calculate LHD post-stratification weights for a given question.
#'
#' @param q_label The question column name in the data set
#' @param popcat Based on which categories to calculate the weights (popcat3 or popcat7).
#'
#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr group_by group_by_at summarise mutate left_join
#' @importFrom haven as_factor
#'
#' @examples
#' add_weights("c3q15")
#' add_weights("totexp_percap", popcat = "c0popcat3")
#'
#' @return tibble
add_weights <- function(q_label, popcat = "c0popcat7") {

    grps <- c(popcat, "core_denom")

    master_data %>%
        as_factor() %>%
        as_tibble() %>%
        group_by_at(grps) %>%
        summarise(
            resp = sum(!is.na(.data[[q_label]]), na.rm = TRUE),
            .groups = "drop") %>%
        mutate(
            core_denom_total = sum(.data$core_denom),
            tot_resp = sum(.data$resp)
        ) %>%
        mutate(
            prop_lhd = .data$core_denom / .data$core_denom_total,
            prop_lhd_resp = .data$resp / .data$tot_resp
        ) %>%
        mutate(
            weights = .data$prop_lhd / .data$prop_lhd_resp,
            weights_static = .data$core_denom / .data$resp
        )

}

#' Weighted Counts
#'
#' @description Calculate weighted results for categorical questions.
#'
#' @param q_label The question label in the data set
#' @param popcat Based on which categories to calculate the weights (popcat3 or popcat7)
#' @param wtype The type of weights to use: "weights" or "weights_static"
#' @param grps A value from the master_data file to use for grouping statistics.
#'
#' @export
#' @importFrom haven as_factor
#' @importFrom dplyr left_join filter mutate select all_of rename_all
#' @importFrom dplyr group_by_at ungroup rename
#' @importFrom srvyr as_survey_design survey_count
#'
#' @examples
#' return_survey_weighted_counts("c3q15")
#' return_survey_weighted_counts("c3q15", popcat = "c0popcat7")
#'
#' @return tibble
return_survey_weighted_counts <- function(q_label, grps = NULL, popcat = "c0popcat7", wtype = "weights") {

  if (is.null(grps)) {

    new_names <- c(
      "Answer",
      "NA.RM",
      "N",
      "SE",
      "Percentage",
      "Question",
      "Group")

    data <- master_data %>%
      as_factor() %>%
      left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
      as_survey_design(strata = popcat, weights = wtype) %>%
      filter(!is.na(.data[[q_label]])) %>%
      survey_count(.data[[q_label]], na.rm = TRUE) %>%
      mutate(Percentage = (n/sum(n, na.rm = TRUE)) * 100) %>%
      mutate(Question = q_label) %>%
      mutate(Group = "All LHDs") %>%
      rename_all(~new_names) %>%
      select(all_of(c("Question", "Group", "Answer", "NA.RM", "N", "SE", "Percentage"))) %>%
      mutate(
        Answer = .data$Answer,
        x = as.numeric(as.factor(Answer)) -1,
        y = round_up(Percentage),
        category = sapply(Answer, custom_str_to_title),
        group = Group,
        subtitle = paste0("All LHDs"),
        x_label = NA
      )

    add_responses <- master_data %>%
      as_factor() %>%
      as_tibble() %>%
      group_by_at(q_label) %>%
      summarise(
        Responses = sum(!is.na(.data[[q_label]]), na.rm = TRUE)) %>%
      mutate(Answer = get(q_label))


    left_join(data, add_responses, by = "Answer")

  } else {

    new_names <- c(
      "Group",
      "Answer",
      "NA.RM",
      "N",
      "SE",
      "Percentage",
      "Question")

    data <- master_data %>%
      as_factor() %>%
      left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
      as_survey_design(strata = popcat, weights = wtype) %>%
      filter(!is.na(.data[[q_label]])) %>%
      group_by_at(grps) %>%
      survey_count(.data[[q_label]], na.rm = TRUE) %>%
      mutate(Percentage = (n/sum(n, na.rm = TRUE)) * 100) %>%
      ungroup()%>%
      mutate(Question = q_label) %>%
      rename_all(~new_names) %>%
      select("Question", "Group", "Answer", "NA.RM", "N", "SE", "Percentage") %>%
      mutate(
        Answer = .data$Answer,
        x = as.numeric(as.factor(Answer)) -1,
        y = round_up(Percentage),
        category = sapply(Answer, custom_str_to_title),
        group = sapply(Group, custom_str_to_title),
        subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps])),
        x_label = NA
      )

    add_responses <- master_data %>%
      as_factor() %>%
      group_by_at(c(q_label, grps)) %>%
      summarise(
        Responses = sum(!is.na(.data[[q_label]]), na.rm = TRUE)) %>%
      mutate(Answer = get(q_label),
             Group = get(grps))

    left_join(data, add_responses, by = c("Answer", "Group"))



  }

}







#' Weighted All LHD Counts Wide
#'
#' @description Calculate weighted results for categorical questions for use in tables.
#'
#' @param q_label The question label in the data set
#' @param popcat Based on which categories to calculate the weights (popcat3 or popcat7)
#' @param wtype The type of weights to use: "weights" or "weights_static"
#'
#' @export
#' @importFrom haven as_factor
#' @importFrom dplyr left_join filter mutate select all_of rename_all
#' @importFrom dplyr group_by_at ungroup rename
#' @importFrom srvyr as_survey_design survey_count
#'
#' @examples
#' add_all_counts_long("c3q15")
#'
#' @return tibble
add_all_counts_long <- function(q_label, popcat = "c0popcat7", wtype = "weights") {

  new_names <- c(
    "Answer",
    "NA.RM",
    "N",
    "SE",
    "Percentage",
    "Group")

  data <- master_data %>%
    as_factor() %>%
    left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
    as_survey_design(strata = popcat, weights = wtype) %>%
    filter(!is.na(.data[[q_label]])) %>%
    survey_count(.data[[q_label]], na.rm = TRUE) %>%
    mutate(Percentage = (n/sum(n, na.rm = TRUE)) * 100) %>%
    mutate(Group = "All LHDs") %>%
    rename_all(~new_names) %>%
    select(all_of(c("Group", "Percentage", "Answer"))) %>%
    mutate(
      y = round_up(Percentage, 1),
      category = sapply(Answer, custom_str_to_title),
      group = Group,
      subtitle = paste0("All LHDs")
    )

  add_responses <- master_data %>%
    as_factor() %>%
    as_tibble() %>%
    group_by_at(q_label) %>%
    summarise(
      Responses = sum(!is.na(.data[[q_label]]), na.rm = TRUE)) %>%
    mutate(Answer = get(q_label))

  data <- left_join(data, add_responses, by = "Answer")  %>%
    dplyr::select(category, group, y, Responses)

  tidyr::pivot_wider(data, names_from = group, values_from = c(y, Responses))
}

#' Weighted Statistics
#'
#' @description Calculate weighted statistics results: total, mean, and quantiles.
#'
#' @param q_label The question label in the data set
#' @param wtype The type of weights to use: "weights" or "weights_static"
#' @param grps A value from the master_data file to use for grouping statistics.
#' @param popcat The strata by which to weight, either c0popcat3 or c0popcat7.
#'
#' @export
#' @importFrom haven as_factor
#' @importFrom dplyr left_join summarise mutate select all_of n
#' @importFrom dplyr group_by_at starts_with ungroup rename
#' @importFrom srvyr as_survey_design survey_total survey_mean survey_quantile
#'
#' @examples
#' return_survey_weighted_statistics("tenure")
#' return_survey_weighted_counts("c5q36", grps = "c0popcat7", wtype = "weights_static")
#'
#' @return tibble
return_survey_weighted_statistics <- function(q_label, grps = NULL, popcat = "c0popcat7", wtype = "weights") {

  if (is.null(grps)) {

    data <- master_data %>%
      as_factor() %>%
      left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
      as_survey_design(strata = popcat, weights = wtype, fpc = "core_denom") %>%
      summarise(
        N = n(),
        Responses = length(which(!is.na(.data[[q_label]]))),
        Total = survey_total(.data[[q_label]], na.rm = TRUE, vartype = c("ci", "se")),
        Mean = survey_mean(.data[[q_label]], na.rm = TRUE),
        Quantile = survey_quantile(.data[[q_label]], quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE, df = sum(Responses) - 1)
      ) %>%
      mutate(Question = q_label) %>%
      mutate(Group = "All LHDs") %>%
      select(.data$Question, .data$Group, .data$N, .data$Responses, .data$Total, Total_SE = "Total_se", ci_low = "Total_low", ci_upp = "Total_upp", .data$Mean, all_of(starts_with("Quantile_"))) %>%
      mutate(
        y = round_up(Mean, 1),
        x = as.numeric(as.factor(Group)) - 1,
        category = sapply(Group, custom_str_to_title),
        group = "All LHDs",
        subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps])),
        x_label = NA
      )

    } else {

      data <- master_data %>%
        as_factor() %>%
        left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
        as_survey_design(strata = popcat, weights = wtype, fpc = "core_denom") %>%
        group_by_at(grps) %>%
        summarise(
          N = n(),
          Responses = length(which(!is.na(.data[[q_label]]))),
          Total = survey_total(.data[[q_label]], na.rm = TRUE, vartype = c("ci", "se")),
          Mean = survey_mean(.data[[q_label]], na.rm = TRUE),
          Quantile = survey_quantile(.data[[q_label]], quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE, df = sum(Responses) - sum(dplyr::n_distinct(.data[[popcat]]))),
          .groups = "drop"
        ) %>%
        mutate(Question = q_label) %>%
        rename(Group = grps) %>%
        select(.data$Question, .data$Group, .data$N, .data$Responses, .data$Total, Total_SE = "Total_se", ci_low = "Total_low", ci_upp = "Total_upp", .data$Mean, all_of(starts_with("Quantile_"))) %>%
        mutate(
          y = round_up(Mean, 1),
          x = as.numeric(as.factor(Group)) - 1,
          category = sapply(Group, custom_str_to_title),
          group = "All LHDs",
          subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps])),
          x_label = NA
        )

      if (grps == "c0popcat3") {
        data <- data %>%
          dplyr::mutate(Group = factor(Group,
                                       levels = c("Small (<50,000)", "Medium (50,000-499,999)", "Large (500,000+)")
          ))
      }

    }
return(data)
}


#' Create Histogram Data
#'
#' @description Calculate weighted statistics results: total, mean, and quantiles.
#'
#' @param input The input to use for filtering the dataframe.
#' @param x_label character string; The text to appear on the x-axis.
#'
#' @export
#'
#' @return dataframe
create_hist <- function(input, x_label) {

  data <- master_data %>%
    haven::as_factor() %>%
    dplyr::left_join(add_weights(input, popcat = "c0popcat7"), by = c("c0popcat7", "core_denom")) %>%
    dplyr::mutate(x = get(input) * weights,
                  category = "All LHDs") %>%
    dplyr:: select(x)

  data <- hist(data$x, plot = FALSE, breaks = 200)

  bins <- data$breaks
  counts <- data$counts
  midpoints <- data$mids

  data.frame(
    low_bin = head(bins, -1),
    high_bin = tail(bins, -1),
    y = counts,
    midpoint = midpoints,
    category = midpoints,
    # category = paste(head(bins, -1), " - ", tail(bins, -1)),
    x = 1:length(midpoints) -1,
    subtitle = "Click and drag to zoom into a specific range on the chart.
    Hold the Shift key and drag to pan horizontally.",
    x_label = x_label,
    group = "All LHDs"
  )
  # %>%
  #   dplyr::mutate(y = (y/sum(y))*100,
  #                 y = round_up(y, 1)) # this would make the y a %
}




#' Dynamic Classification Dataframe
#'
#' @description Dynamically create a dataframe to use with a shiny widget..
#'
#' @param args A variable from master_data.
#'
#' @export
#'
#' @examples
#' class_data("c5q63a")
#'
#' @return dataframe
class_data <- function(args) {
  map_dfr(args, return_survey_weighted_counts)
}




#' Dynamic Classification Dataframe
#'
#' @description Dynamically create a dataframe to use with a shiny widget..
#'
#' @param args A variable from master_data.
#'
#' @export
#' @return dataframe
class_data_b <- function(args) {
  pmap_dfr(args, return_survey_weighted_statistics)
}




#' Retrieve n for jobs data
#'
#' @description Filtering jobs_data to retrieve the n for visualizations.
#'
#' @param question A variable from jobs_data, either "jobsgone" or "jobsgain" - either works.
#'
#' @export
#' @return dataframe
jobs_responses <- function(question = "jobsgone") {
  jobs_data %>%
  dplyr::filter(Question == question) %>%
  dplyr::group_by(filter) %>%
  dplyr::summarise(responses = sum(Responses))
}



#' Retrieve filtered jobs dataset for visualization
#'
#' @description Filtering jobs_data or budget_data.
#'
#' @param selected_data jobs_data or budget_jobs.
#'
#' @export
#' @return dataframe
filtered_butterfly <- function(selected_data = "jobs_data") {

  if (selected_data == "jobs_data") {
    data <- jobs_data %>%
      dplyr::mutate(x = as.numeric(as.factor(group))-1,
                    group = dplyr::case_when(group == "All Lhds" ~ "All LHDs",
                                             TRUE ~ group)) %>%
      dplyr::select(group, Question, y, x, total_employees, filter)


    data %>%
      tidyr::pivot_wider(names_from = Question, values_from = y) %>%
      dplyr::rename(left = jobsgone,
                    right = jobsgain) %>%
      dplyr::mutate(left_standard = round_up((left/total_employees)*100, 1),
                    right_standard = round_up((right/total_employees)*100, 1),
                    difference = right - left) %>%
      dplyr::mutate(group = sub(" .*", "", group)) %>% # shorten labels
      dplyr::mutate_if(is.numeric, round, 1)


  } else if (selected_data == "budget_data") {
    data <- budget_data %>%
      dplyr::filter(category != "Unknown") %>%
      dplyr::mutate(x = as.numeric(as.factor(group))-1,
                    group = dplyr::case_when(group == "All Lhds" ~ "All LHDs",
                                             TRUE ~ group)) %>%
      dplyr::select(group, x, y, category, Responses, filter) %>%
      tidyr::pivot_wider(names_from = category, values_from = c(y, Responses)) %>%
      dplyr::rename(left = 4,
                    right = 5,
                    left_n = 6,
                    right_n = 7) %>%
      dplyr::mutate(group = sub(" .*", "", group)) # shorten labels
  }
}



#' Create list for boxplot
#'
#' @description Create a list with a length of one or more for use in boxplot visualizations.
#'
#' @param args A variable from master_data.
#' @param grps A value from the master_data file to use for grouping statistics.
#'
#' @export
#' @return A list of one or more.
boxplot_data <- function(args, grps = NULL) {

  if (is.null(grps)) {
    data <- return_survey_weighted_statistics(args) %>%
      dplyr::mutate(across(where(is.numeric), ~ signif(., digits = 3)))

    find_limits <- master_data %>%
      haven::as_factor() %>%
      dplyr::left_join(add_weights(args, popcat = "c0popcat7"), by = c("c0popcat7", "core_denom")) %>%
      dplyr::mutate(y = get(args) * weights) %>%
      tidyr::drop_na(y) %>%
      dplyr::select(y)

    # set upper and lower bounds for the box plot
    iqr <- data$Quantile_q75 - data$Quantile_q25
    find_lower <- data$Quantile_q25 - (1.5 * iqr)
    upper_bound <- data$Quantile_q75 + (1.5 * iqr)
    lower_bound <- ifelse((find_lower <= 1), 0, find_lower)

    # mean
    mean <- data.frame(x = 0, y = data$Mean)

    # identify outliers, min, max, number of
    outliers <- find_limits %>% dplyr::filter(y >= upper_bound | y <= lower_bound)

    list(
      list(
        name = data$group,
        low = lower_bound,
        q1 = data$Quantile_q25,
        median = data$Quantile_q50,
        q3 = data$Quantile_q75,
        high = upper_bound,
        category = "All LHDs",
        mean = mean,
        responses = data$Responses,
        outliers = outliers,
        min_outlier = min(as.numeric(outliers$y)),
        max_outlier = max(as.numeric(outliers$y)),
        total_outliers = length(as.numeric(outliers$y)),
        subtitle = data$subtitle
      )
    )

  } else {

    data <- return_survey_weighted_statistics(args, grps) %>%
      dplyr::mutate(across(where(is.numeric), ~ signif(., digits = 3)),
                    Group = as.character(Group)) %>%
      dplyr::select(category, Group, Quantile_q25, Quantile_q50, Quantile_q75, Responses, Mean, subtitle) %>%
      dplyr::rename(q1 = Quantile_q25,
                    median = Quantile_q50,
                    q3 = Quantile_q75,
                    name = Group,
                    mean = Mean,
                    group_responses = Responses,
                    category = category,
                    subtitle = subtitle) %>%
      dplyr::mutate(responses = sum(group_responses))


    data <- split(data, data$category)

    find_limits <- master_data %>%
      haven::as_factor() %>%
      dplyr::left_join(add_weights(args, popcat = "c0popcat7"), by = c("c0popcat7", "core_denom")) %>%
      dplyr::mutate(y = get(args) * weights) %>%
      tidyr::drop_na(y) %>%
      dplyr::select(y, grps)

    for(i in seq_along(data)) {

      iqr <- data[[i]]$q3 - data[[i]]$q1
      find_lower <- data[[i]]$q1 - (1.5 * iqr)
      upper_bound <- data[[i]]$q3 + (1.5 * iqr)
      lower_bound <- ifelse((find_lower <= 1), 0, find_lower)

      outliers <- find_limits %>%
        dplyr::filter(get(grps) == data[[i]]$name) %>%
        dplyr::filter(y >= upper_bound | y <= lower_bound)

      # add elements to the list
      data[[i]][["mean"]] <- data.frame(x = i-1, y = data[[i]]$mean)
      data[[i]][["high"]] <- upper_bound
      data[[i]][["low"]] <- lower_bound
      data[[i]][["outliers"]] <- list(outliers)
      data[[i]][["min_outlier"]] <- min(as.numeric(outliers$y))
      data[[i]][["max_outlier"]] <- max(as.numeric(outliers$y))
      data[[i]][["total_outliers"]] <- length(as.numeric(outliers$y))
    }

    lapply(data, function(tbl) as.list(tbl[1, ]))

  }
}



#' Data cleaning for scatter plot visualization
#'
#' @description This function cleans and formats data for scatter plot visualization.
#' It summarizes, pivots, and restructures the data while retaining key contextual
#' information. It also assigns alignment properties for visualization purposes.
#'
#' @param set_filter A character string. The filter to apply to the data (e.g., "All LHDs").
#' @param set_service A character string or vector. The service type(s) to include
#' (e.g., "Blood Lead Screening").
#'
#' @export
#' @return A tibble containing cleaned and formatted data ready for visualization.
scatter_data <- function(set_filter = "All LHDs",
                         set_service = "Blood Lead Screening") {


  # Step 1: Filter and summarize the data
  data_xy <- services_change_data %>%
    dplyr::select(service_type, group, service_provision, y, Responses, filter, Question) %>%
    dplyr::filter(filter %in% set_filter,
                  service_type %in% set_service) %>%
    dplyr::group_by(service_type, filter, group) %>%
    dplyr::reframe(
      x = sum(y[service_provision == "Reduced Service"], na.rm = TRUE),
      y = sum(y[service_provision == "Expanded Service"], na.rm = TRUE),
      Responses = sum(Responses)
      # ,
      # group = group, # Keep Responses as is
      # .groups = "drop"  # Drop grouping after summarization
    )
  # View(data_xy)




  # Step 2: Add alignment and short name properties
  data_clean <- data_xy %>%
    # Add vertical alignment based on `service_type`
    dplyr::mutate(
      verticalAlign = dplyr::case_when(
        service_type == "Blood Lead Screening" ~ "top",
        service_type == "Chronic Disease" ~ "bottom",
        service_type == "Communicable Disease Screening or Treatment" ~ "bottom",
        service_type == "Diabetes Screening" ~ "top",
        service_type == "Emergency Preparedness" ~ "top",
        service_type == "Environmental Health Including Food Safety" ~ "top",
        service_type == "Epidemiology and Surveillance" ~ "bottom",
        service_type == "High Blood Pressure Screening" ~ "middle",
        service_type == "Immunization" ~ "bottom",
        service_type == "Maternal and Child Health Services" ~ "bottom",
        service_type == "Mental Health" ~ "middle",
        service_type == "Obesity Prevention" ~ "top",
        service_type == "Substance Use Prevention" ~ "bottom"
      )
    ) %>%

    # Add horizontal alignment based on `service_type`
    dplyr::mutate(
      align = dplyr::case_when(
        service_type == "Blood Lead Screening" ~ "left",
        service_type == "Chronic Disease" ~ "left",
        service_type == "Communicable Disease Screening or Treatment" ~ "center",
        service_type == "Diabetes Screening" ~ "center",
        service_type == "Emergency Preparedness" ~ "center",
        service_type == "Environmental Health Including Food Safety" ~ "center",
        service_type == "Epidemiology and Surveillance" ~ "center",
        service_type == "High Blood Pressure Screening" ~ "left",
        service_type == "Immunization" ~ "center",
        service_type == "Maternal and Child Health Services" ~ "center",
        service_type == "Mental Health" ~ "left",
        service_type == "Obesity Prevention" ~ "center",
        service_type == "Substance Use Prevention" ~ "center"
      )
    ) %>%

    # Create short names for `service_type`
    dplyr::mutate(
      service_type_short = dplyr::case_when(
        service_type == "Blood Lead Screening" ~ "Blood Lead",
        service_type == "Chronic Disease" ~ "Chronic Disease",
        service_type == "Communicable Disease Screening or Treatment" ~ "Comm. Disease",
        service_type == "Diabetes Screening" ~ "Diabetes",
        service_type == "Emergency Preparedness" ~ "Emergency Prep.",
        service_type == "Environmental Health Including Food Safety" ~ "Env. Health",
        service_type == "Epidemiology and Surveillance" ~ "Epi & Surveillance",
        service_type == "High Blood Pressure Screening" ~ "High Blood Pressure",
        service_type == "Immunization" ~ "Immunization",
        service_type == "Maternal and Child Health Services" ~ "MCH",
        service_type == "Mental Health" ~ "Mental Health",
        service_type == "Obesity Prevention" ~ "Obesity Prev.",
        service_type == "Substance Use Prevention" ~ "Substance Use Prev."
      )
    )

  # Step 3: Add data label configuration for visualization
  data <- data_clean
  # %>%
  #   dplyr::mutate(
  #     dataLabels = purrr::map2(
  #       verticalAlign, align,
  #       ~ list(enabled = TRUE, verticalAlign = .x, align = .y)
  #     )
  #   )
#
# View(data)

  # Return the cleaned and formatted dataset
  return(data)
}
