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
          as_survey_design(ids = popcat, weights = wtype) %>%
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
            y = round(Percentage,1),
            category = stringr::str_to_title(Answer),
            group = Group,
            subtitle = paste0("Estimated Across All LHDs")
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
          as_survey_design(ids = popcat, weights = wtype) %>%
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
            y = round(Percentage,1),
            category = stringr::str_to_title(Answer),
            group = stringr::str_to_title(Group),
            subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps]))
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

#' Weighted Statistics
#'
#' @description Calculate weighted statistics results: total, mean, and quantiles.
#'
#' @param q_label The question label in the data set
#' @param wtype The type of weights to use: "weights" or "weights_static"
#' @param grps A value from the master_data file to use for grouping statistics.
#' @param popcat The stata by which to weight, either c0popcat3 or c0popcat7.
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

    master_data %>%
      as_factor() %>%
      left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
      as_survey_design(ids = popcat, weights = wtype) %>%
      summarise(
        N = n(),
        Responses = length(which(!is.na(.data[[q_label]]))),
        Total = survey_total(.data[[q_label]], na.rm = TRUE),
        Mean = survey_mean(.data[[q_label]], na.rm = TRUE),
        Quantile = survey_quantile(.data[[q_label]], quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE, df = sum(Responses) - 1)
      ) %>%
      mutate(Question = q_label) %>%
      mutate(Group = "All LHDs") %>%
      select(.data$Question, .data$Group, .data$N, .data$Responses, .data$Total, Total_SE = "Total_se", .data$Mean, all_of(starts_with("Quantile_"))) %>%
      mutate(
        y = round(Mean, 1),
        x = as.numeric(as.factor(Group)) - 1,
        category = stringr::str_to_title(Group),
        group = "All LHDs",
        subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps]))
      )

    } else {

      master_data %>%
        as_factor() %>%
        left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
        as_survey_design(ids = popcat, weights = wtype) %>%
        group_by_at(grps) %>%
        summarise(
          N = n(),
          Responses = length(which(!is.na(.data[[q_label]]))),
          Total = survey_total(.data[[q_label]], na.rm = TRUE),
          Mean = survey_mean(.data[[q_label]], na.rm = TRUE),
          Quantile = survey_quantile(.data[[q_label]], quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE, df = sum(Responses) - sum(dplyr::n_distinct(.data[[popcat]]))),
          .groups = "drop"
        ) %>%
        mutate(Question = q_label) %>%
        rename(Group = grps) %>%
        select(.data$Question, .data$Group, .data$N, .data$Responses, .data$Total, Total_SE = "Total_se", .data$Mean, all_of(starts_with("Quantile_"))) %>%
        mutate(
          y = round(Mean, 1),
          x = as.numeric(as.factor(Group)) - 1,
          category = stringr::str_to_title(Group),
          group = "All LHDs",
          subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps]))
        )

    }

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
    subtitle = "Click and span a range on the figure to zoom.",
    x_label = x_label,
    group = "All LHDs"
  )
  # %>%
  #   dplyr::mutate(y = (y/sum(y))*100,
  #                 y = round(y, 1)) # this would make the y a %
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
responses <- function(question = "jobsgone") {
  jobs_data %>%
  dplyr::filter(Question == question) %>%
  dplyr::group_by(filter) %>%
  dplyr::summarise(responses = sum(Responses))
}



#' Retrieve filtered jobs dataset for visualization
#'
#' @description Filtering jobs_data.
#'
#' @param question A variable from jobs_data, either "jobsgone" or "jobsgain" - either works.
#'
#' @export
#' @return dataframe
filtered_jobs <- function(selected_data = jobs_data) {

  data <- selected_data %>%
    dplyr::select(1:7, 14:20) %>%
    dplyr::mutate(x = as.numeric(as.factor(group))-1,
                  group = dplyr::case_when(group == "All Lhds" ~ "All LHDs",
                                           TRUE ~ group)) %>%
    dplyr::select(group, Question, y, x, total_employees)


  data %>%
    tidyr::pivot_wider(names_from = Question, values_from = y) %>%
    dplyr::mutate(jobsgone_standard = round((jobsgone/total_employees)*100,1),
                  jobsgain_standard = round((jobsgain/total_employees)*100,1),
                  difference = jobsgain - jobsgone) %>%
    dplyr::mutate(group = sub(" .*", "", group)) %>%
    dplyr::mutate_if(is.numeric, round, 1)


}
