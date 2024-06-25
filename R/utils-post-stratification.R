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
        haven::as_factor() %>%
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
#' add_weights("c3q15")
#' add_weights("totexp_percap")
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
            as_survey_design(ids = popcat, weights = wtype) %>%
            filter(!is.na(.data[[q_label]])) %>%
            survey_count(.data[[q_label]], na.rm = TRUE) %>%
            mutate(Percentage = (n/sum(n, na.rm = TRUE)) * 100) %>%
            mutate(Question = q_label) %>%
            mutate(Group = "All LHDs") %>%
            rename_all(~new_names) %>%
            select(all_of(c("Question", "Group", "Answer", "NA.RM", "N", "SE", "Percentage"))) %>%
            mutate(Answer = as.character(.data$Answer))
    } else {
        
        new_names <- c(
            "Group",
            "Answer",
            "NA.RM", 
            "N", 
            "SE", 
            "Percentage",
            "Question")

        master_data %>%
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
            mutate(Answer = as.character(.data$Answer))
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
                Total = survey_total(.data[[q_label]], na.rm = TRUE),
                Mean = survey_mean(.data[[q_label]], na.rm = TRUE),
                Quantile = survey_quantile(.data[[q_label]], quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE)
                ) %>%
            mutate(Question = q_label) %>%
            mutate(Group = "All LHDs") %>%
            select(.data$Question, .data$Group, .data$N, .data$Total, Total_SE = "Total_se", .data$Mean, all_of(starts_with("Quantile_")))

    } else {

        master_data %>%
            as_factor() %>%
            left_join(add_weights(q_label, popcat = popcat), by = c(popcat, "core_denom")) %>%
            as_survey_design(ids = popcat, weights = wtype) %>%
            group_by_at(grps) %>%
            summarise(
                N = n(),
                Total = survey_total(.data[[q_label]], na.rm = TRUE),
                Mean = survey_mean(.data[[q_label]], na.rm = TRUE),
                Quantile = survey_quantile(.data[[q_label]], quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE),
                .groups = "drop"
                ) %>%
            mutate(Question = q_label) %>%
            rename(Group = grps) %>%
            select(.data$Question, .data$Group, .data$N, .data$Total, Total_SE = "Total_se", .data$Mean, all_of(starts_with("Quantile_")))

    }

}