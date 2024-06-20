#' Calculate Weights
#' 
#' @description Calculate LHD post-stratification weights for a given question.
#'
#' @param dt_sel A data frame with the selected data.
#' @param grps Additional grouping variables.
#'
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by group_by_at summarise mutate left_join
#' @importFrom haven as_factor
#' 
#' @example 
#' add_weights("c3q15")
#' add_weights("totexp_percap")
#' 
#' @return tibble
add_weights <- function(q_label, grps = NULL) {

    grps <- c("c0popcat7", "core_denom", grps)

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
            weights = .data$prop_lhd / .data$prop_lhd_resp
        )
    
}

test_characteristics_gender <- function() {
    master_data |>
        haven::as_factor() |>
        dplyr::left_join(add_weights("c4q29"), by = c("c0popcat7", "core_denom")) |>
        dplyr::count(c4q29) |>
        dplyr::filter(c4q29 %in% c("male", "female")) |>
        dplyr::mutate(perc = round(n/sum(n, na.rm = TRUE)*100,1))
}   

test_characteristics_age <- function() {
    master_data |>
        haven::as_factor() |>
        dplyr::left_join(add_weights("c4q29"), by = c("c0popcat7", "core_denom")) |>
        dplyr::mutate(
            age_categ = dplyr::case_when(
                c4q30 < 40 ~ "Younger than 40",
                c4q30 >= 40 & c4q30 < 49 ~ "40-49",
                c4q30 >= 50 & c4q30 < 59 ~ "50-59",
                c4q30 >= 60 & c4q30 < 69 ~ "60-69",
                c4q30 >= 70 ~ "Older than 70",
            )
        ) |>
        dplyr::count(age_categ) |>
        dplyr::filter(!is.na(age_categ)) |>
        dplyr::mutate(perc = round(n/sum(n, na.rm = TRUE)*100,1))
}

test_characteristics_age_fig4.2_2022 <- function() {
    master_data |>
        haven::as_factor() |>
        dplyr::left_join(add_weights("c4q29"), by = c("c0popcat7", "core_denom")) |>
        dplyr::mutate(
            age_categ = dplyr::case_when(
                c4q30 < 40 ~ "Younger than 40",
                c4q30 >= 40 & c4q30 < 49 ~ "40-49",
                c4q30 >= 50 & c4q30 < 59 ~ "50-59",
                c4q30 >= 60 & c4q30 < 69 ~ "60-69",
                c4q30 >= 70 ~ "Older than 70",
            )
        ) |>
        dplyr::count(age_categ) |>
        dplyr::filter(!is.na(age_categ)) |>
        dplyr::mutate(perc = round(n/sum(n, na.rm = TRUE)*100,1))
}


test_expenditures_fig6.2_2022 <- function() {

    master_data_w <- master_data %>%
        haven::as_factor() %>%
        dplyr::left_join(add_weights("c3q15"), by = c("c0popcat7", "core_denom")) %>%
        dplyr::mutate(value = c3q15) %>%
        dplyr::mutate(weighted_value = c3q15 * weights)

    overall <- master_data_w %>%
        dplyr::summarise(
            Mean = mean(weighted_value, na.rm = TRUE),
            `25th percentile` = quantile(weighted_value, probs = 0.25, na.rm = TRUE),
            `50th percentile (Median)` = median(weighted_value, na.rm = TRUE),
            `75th percentile` = quantile(weighted_value, 0.75, na.rm = TRUE),
            .groups = "drop"
            )%>%
        dplyr::mutate(c0popcat7 = "All LHDs")

    split_lhd <- master_data_w %>%
        dplyr::group_by(c0popcat7) %>%
        dplyr::summarise(
            Mean = mean(value, na.rm = TRUE),
            `25th percentile` = quantile(value, 0.25, na.rm = TRUE),
            `50th percentile (Median)` = median(value, na.rm = TRUE),
            `75th percentile` = quantile(value, 0.75, na.rm = TRUE),
            .groups = "drop"
            )

    dplyr::bind_rows(overall, split_lhd) %>%
        dplyr::mutate_if(is.numeric, signif, 3)
}

test_expenditures_fig6.6_2022 <- function() {

    master_data_w <- master_data %>%
        haven::as_factor() %>%
        dplyr::left_join(add_weights("totexp_percap"), by = c("c0popcat7", "core_denom")) %>%
        dplyr::mutate(value = totexp_percap) %>%
        dplyr::mutate(weighted_value = totexp_percap * weights)

    overall <- master_data_w %>%
        dplyr::summarise(
            Mean = mean(weighted_value, na.rm = TRUE),
            Median = median(weighted_value, na.rm = TRUE),
            .groups = "drop"
            )%>%
        dplyr::mutate(c0popcat7 = "All LHDs")

    split_lhd <- master_data_w %>%
        dplyr::group_by(c0popcat7) %>%
        dplyr::summarise(
            Mean = mean(value, na.rm = TRUE),
            Median = median(value, na.rm = TRUE),
            .groups = "drop"
            )

    dplyr::bind_rows(overall, split_lhd) %>%
        dplyr::mutate_if(is.numeric, signif, 3)

}

test_expenditures_fig6.9_2022 <- function() {

    var <- c(
        "local_percap",
        "state_percap",
        "fed_percap",
        "clinical_percap"
    )

    do.call(
        dplyr::bind_rows,
        lapply(
            var,
            function(x) {
                master_data_w <- master_data %>%
                    haven::as_factor() %>%
                    dplyr::left_join(add_weights(as.character(x)), by = c("c0popcat7", "core_denom")) %>%
                    dplyr::mutate(value = .data[[x]]) %>%
                    dplyr::mutate(weighted_value = .data[[x]] * weights)

                overall <- master_data_w %>%
                    dplyr::summarise(
                        Mean = mean(weighted_value, na.rm = TRUE),
                        Median = median(weighted_value, na.rm = TRUE),
                        .groups = "drop"
                        )%>%
                    dplyr::mutate(c0popcat7 = "All LHDs")

                overall %>%
                    dplyr::mutate_if(is.numeric, signif, 2) %>%
                    dplyr::mutate(
                        variable = x
                    )
            }
            )
        )

    

}