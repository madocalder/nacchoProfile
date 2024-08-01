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

        master_data %>%
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
            y = round(Percentage,1),
            category = stringr::str_to_title(Answer),
            group = Group,
            subtitle = paste0("Across All LHDs")
          )

    } else {

        new_names <- c(
            "Group",
            "Answer",
            "NA.RM",
            "N",
            "SE",
            "Percentage",
            "Question")

        # master_data %>%
        #   as_factor() %>%
        #   filter(!is.na(.data[[q_label]])) %>%
        #   group_by_at(c(grps, q_label)) %>%
        #   summarise(N = n()) %>%
        #   ungroup() %>%
        #   group_by_at(c(grps)) %>%
        #   mutate(Percentage = (N/sum(N, na.rm = TRUE)) * 100) %>%
        #   ungroup() %>%
        #   mutate("NA.RM" = TRUE) %>%
        #   mutate(SE = NA) %>%
        #   mutate(Question = q_label) %>%
        #   select(grps, q_label, "NA.RM", "N", "SE", "Percentage", "Question") %>%
        #   rename_all(~new_names) %>%
        #   mutate(
        #     Answer = .data$Answer,
        #     x = as.numeric(as.factor(Answer)) -1,
        #     y = round(Percentage,1),
        #     category = stringr::str_to_title(Answer),
        #     group = stringr::str_to_title(Group),
        #     subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps]))
        #   )
        
        master_data %>%
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
            y = round(Percentage,1),
            category = stringr::str_to_title(Answer),
            group = stringr::str_to_title(Group),
            subtitle = paste0("By ", names(landscape_filters[landscape_filters == grps]))
          )
    }

}

#' Weighted Statistics
#'
#' @description Calculate weighted statistics results: total, mean, and quantiles.
#'
#' @param q_label The question label in the data set
#' @param wtype The type of weights to use: "weights" or "weights_static"
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
      as_survey_design(strata = popcat, weights = wtype) %>%
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

      if (grps != "c0popcat7") {
        master_data %>%
          as_factor() %>%
          left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
          as_survey_design(strata = popcat, weights = wtype) %>%
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
      } else {

        master_data %>%
          as_factor() %>%
          group_by_at(grps) %>%
          summarise(
            N = n(),
            Responses = length(which(!is.na(.data[[q_label]]))),
            Total = n(),
            Total_se = NA,
            Mean = mean(.data[[q_label]], na.rm = TRUE),
            Quantile_q25 = quantile(.data[[q_label]], probs = 0.25, na.rm = TRUE),
            Quantile_q50 = quantile(.data[[q_label]], probs = 0.50, na.rm = TRUE),
            Quantile_q75 = quantile(.data[[q_label]], probs = 0.75, na.rm = TRUE),
            Quantile_q25_se = NA,
            Quantile_q50_se = NA,
            Quantile_q75_se = NA,
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
#' @examples
#' create_hist("c5q37")
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
    x_label = x_label
  )
}

