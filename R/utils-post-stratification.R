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
add_weights <- function(q_label, grps = NULL, popcat = 7) {

    grps <- c("core_denom", grps)

    if (popcat == 7) {
        grps <- c("c0popcat7", grps)
    } else if (popcat == 3) {
        grps <- c("c0popcat3", grps)
    }

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

    # survey_design <- survey::svydesign(
    #     id = ~1, # _n in Stata is equivalent to id = ~1 in R
    #     strata = ~c0popcat7,
    #     weights = ~core_denom,
    #     fpc = ~core_denom,
    #     data = master_data,
    #     nest = TRUE # to account for nested strata
    # )

    # rep_srv <- survey::as.svrepdesign(survey_design)
    # survey::svymean(~c3q15, rep_srv)
}

build_categ_weight <- \(q_label) {

        grps <- c("c0popcat7", "core_denom", "weights")

        master_data %>%
            haven::as_factor() %>%
            dplyr::left_join(add_weights(q_label), by = c("c0popcat7", "core_denom")) %>%
            dplyr::filter(!is.na(.data[[q_label]])) %>%
            dplyr::group_by_at(grps) %>%
            dplyr::count(.data[[q_label]], name = "counts") %>%
            dplyr::ungroup() %>%
            dplyr::mutate(value = counts) %>%
            dplyr::mutate(weighted_value = counts * weights) %>%
            dplyr::group_by_at(q_label) %>%
            dplyr::summarise(
                Sum = sum(weighted_value, na.rm = TRUE),
                .groups = "drop"
                ) %>%
            dplyr::mutate(Percentage = round(Sum/sum(Sum, na.rm = TRUE)*100,1))
    }

test_tenure_fig4.1 <- function() {

    results_c4q26 <- build_categ_weight("c4q26") %>%
        filter(c4q26 == "part-time") %>%
        select("Question" = c4q26, "Percentage") %>%
        mutate(Question = "Part-time position")

    results_c4q28 <- build_categ_weight("c4q28") %>%
        filter(c4q28 == "Hispanic/Latino") %>%
        select("Question" = c4q28, "Percentage") %>%
        mutate(Question = "Hispanic / Latino")

    results_racenotwhite <- build_categ_weight("racenotwhite") %>%
        filter(racenotwhite == 1) %>%
        select("Question" = racenotwhite, "Percentage") %>%
        mutate(Question = "Race other than White")

    results_c4q29 <- build_categ_weight("c4q29") %>%
        filter(c4q29 == "female") %>%
        select("Question" = c4q29, "Percentage") %>%
        mutate(Question = "Female")

    bind_rows(results_c4q26, results_c4q28, results_racenotwhite, results_c4q29) %>%
        mutate(Percentage = round(Percentage, 0))

}

test_tenure_fig4.2 <- function() {
    build_categ_weight("age_cat") %>%
        select("Question" = age_cat, "Percentage") %>%
        mutate(Question = case_when(
            Question == "Under 40" ~ "Younger than 40",
            Question == "40-49 years" ~ "40-49",
            Question == "50-59 years" ~ "50-59",
            Question == "60-69 years" ~ "60-69",
            Question == "70 or older" ~ "Older than 70"
        )) %>%
        mutate(Percentage = round(Percentage, 0))
}

test_tenure_fig4.4 <- function() {
    master_data_w <- master_data %>%
        haven::as_factor() %>%
        dplyr::left_join(add_weights("tenure"), by = c("c0popcat7", "core_denom")) %>%
        dplyr::mutate(value = tenure) %>%
        dplyr::mutate(weighted_value = tenure * weights)

    master_data_w %>%
        dplyr::summarise(
            Mean = mean(weighted_value, na.rm = TRUE),
            .groups = "drop"
            )%>%
        dplyr::mutate(c0popcat7 = "All LHDs")
}

test_tenure_fig4.5 <- function() {
    build_categ_weight("c4q34") %>%
        select("Question" = c4q34, "Percentage") %>%
        mutate(Question = case_when(
            Question == "associates" ~ "Associate's",
            Question == "bachelors" ~ "Bachelor's",
            Question == "masters" ~ "Master's",
            Question == "doctorate" ~ "Doctoral"
        )) %>%
        mutate(Percentage = round(Percentage, 0))
}

test_tenure_fig4.5 <- function() {
    medical <- build_categ_weight("c4q502a") %>%
        select("Question" = c4q502a, "Percentage") %>%
        filter(Question == "checked") %>%
        mutate(Question = "Medical")

    nursing <- build_categ_weight("c4q502b") %>%
        select("Question" = c4q502b, "Percentage") %>%
        filter(Question == "checked") %>%
        mutate(Question = "Nursing")

    public_health <- build_categ_weight("c4q502c") %>%
        select("Question" = c4q502c, "Percentage") %>%
        filter(Question == "checked") %>%
        mutate(Question = "Public Health")
        
    bind_rows(medical, nursing, public_health) %>%
        mutate(Percentage = round(Percentage, 0))
}

test_tenure_fig5.1 <- function() {
    build_categ_weight("FTE_numcat") %>%
        select("Question" = FTE_numcat, "Percentage") %>%
        mutate(Question = case_when(
            Question == "<5" ~ "Fewer than 5",
            Question == "200+" ~ "200 or more",
            TRUE ~ Question
        )) %>%
        mutate(Percentage = round(Percentage, 0))
}

test_tenure_fig5.2 <- function() {

    combine_with_split <- \(q_label) {
        master_data_w <- master_data %>%
            haven::as_factor() %>%
            dplyr::left_join(add_weights(q_label), by = c("c0popcat7", "core_denom")) %>%
            dplyr::mutate(value = .data[[q_label]]) %>%
            dplyr::mutate(weighted_value = .data[[q_label]] * weights)

        overall <- master_data_w %>%
            dplyr::filter(!is.na(weighted_value)) %>%
            dplyr::summarise(
                Mean = mean(weighted_value, na.rm = TRUE),
                Median = weighted.median(value, weights, na.rm = TRUE),
                .groups = "drop"
                )%>%
            dplyr::mutate(c0popcat7 = "All LHDs") %>%
            mutate(Mean = round(Mean, 0)) %>%
            mutate(Median = round(Median, 0))

        split_lhd <- master_data_w %>%
            dplyr::filter(!is.na(value)) %>%
            dplyr::group_by(c0popcat7) %>%
            dplyr::summarise(
                Mean = mean(value, na.rm = TRUE),
                Median = median(value, na.rm = TRUE),
                .groups = "drop"
                )

        dplyr::bind_rows(overall, split_lhd)
    
    }

    c5q36 <- combine_with_split("c5q36") %>%
        select(
            "Category" = c0popcat7, 
            Mean_Employees = "Mean", 
            Median_Employees = "Median"
            )
    c5q37 <- combine_with_split("c5q37") %>%
        select(
            "Category" = c0popcat7, 
            Mean_FTEs = "Mean", 
            Median_FTEs = "Median"
            )

    c5q36 %>%
        left_join(c5q37, by = "Category")

}

test_fig5.6 <- function() {

    master_data_w_c5q36 <- master_data %>%
        haven::as_factor() %>%
        dplyr::left_join(add_weights("c5q36"), by = c("c0popcat7", "core_denom")) %>%
        dplyr::mutate(value = c5q36) %>%
        dplyr::mutate(weighted_value = c5q36 * weights) %>%
        dplyr::filter(!is.na(weighted_value)) %>%
        dplyr::summarise(
            N = n(),
            Total = sum(weighted_value, na.rm = TRUE),
            .groups = "drop"
            )%>%
        dplyr::mutate(c0popcat7 = "All LHDs")
    
    master_data_w_c5q37 <- master_data %>%
        haven::as_factor() %>%
        dplyr::left_join(add_weights("c5q37"), by = c("c0popcat7", "core_denom")) %>%
        dplyr::mutate(value = c5q37) %>%
        dplyr::mutate(weighted_value = c5q37 * weights) %>%
        dplyr::filter(!is.na(weighted_value)) %>%
        dplyr::summarise(
            N = n(),
            Total = sum(weighted_value, na.rm = TRUE),
            .groups = "drop"
            )%>%
        dplyr::mutate(c0popcat7 = "All LHDs")

    bind_rows(master_data_w_c5q36, master_data_w_c5q37) %>%
        mutate(Total = signif(Total, 3))
}


test_fig5.8 <- function() {

    master_data_w <- master_data %>%
        haven::as_factor() %>%
        dplyr::left_join(add_weights("fte_bypop", popcat = 3), by = c("c0popcat3", "core_denom")) %>%
        dplyr::mutate(value = fte_bypop) %>%
        dplyr::mutate(weighted_value = fte_bypop * weights)

    all <- master_data_w %>%
        dplyr::filter(!is.na(weighted_value)) %>%
        dplyr::summarise(
            Mean = mean(weighted_value, na.rm = TRUE),
            .groups = "drop"
            )%>%
        dplyr::mutate(c0popcat3 = "All LHDs") %>%
        mutate(c0popcat3 = "All LHDs")

    split <- master_data_w %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::group_by(c0popcat3) %>%
        dplyr::summarise(
            Mean = mean(value, na.rm = TRUE),
            .groups = "drop"
            )
    
    bind_rows(all, split)
}

test_fig5.10 <- function() {
    
    build_categ_weight("jobsgoneany") %>%
        select("Question" = jobsgoneany, "Percentage") %>%
        filter(Question == 1) %>%
        mutate(Question = "Job losses due to layoffs and/or attrition, over time") %>%
        mutate(Percentage = round(Percentage, 0))

}

test_fig5.12 <- function() {

    q_sel <- tibble::tibble(
        question_label = c(
            "Agency leadership",
            "Animal control worker",
            "Behavioral health staff",
            "Business and financial operations staff",
            "Community health worker",
            "Environmental health worker",
            "Epidemiologist/statistician",
            "Health educator",
            "Information systems specialist",
            "Laboratory worker",
            "Licensed practical or vocational nurse",
            "Nusing aide and home health aide",
            "Nutritionist",
            "Office and administrative support staff",
            "Oral health care professional",
            "Prepareness staff",
            "Public health physician",
            "Public information professional",
            "Registered nurse"
            ),
        question_id = c(
            "c5q63a", "c5q62a", "c5q52a", "c5q64a", 
            "c5q61a", "c5q55a", "c5q47a", "c5q48a", 
            "c5q50a", "c5q60a", "c5q57a", "c5q58a",
            "c5q49a", "c5q65a", "c5q59a", "c5q56a",
            "c5q44a", "c5q51a", "c5q43a")
    )
    
    all <- do.call(
        bind_rows,
            apply(
            q_sel,
            1,
            \(x) {
              
                build_categ_weight(x[2]) %>%
                    mutate(Question = x[1]) %>%
                    filter(question_id == "yes") %>%
                    mutate(c0popcat7 = "All LHDs") %>%
                    select("Question", "c0popcat7", "Percentage")
            }
        )
    )

    split <- do.call(
        bind_rows,
            apply(
            q_sel,
            1,
            \(x) {
                cat("Processing: ", x[1], " -- ", x[2], "\n")
                master_data %>%
                    select(c0popcat7, result = as.character(x[2])) %>%
                    mutate(Question = as.character(x[1])) %>%
                    dplyr::filter(!is.na(result)) %>%
                    dplyr::group_by(c0popcat7, Question) %>%
                    dplyr::count(result) %>%
                    mutate(Percentage = round(n/sum(n)*100, 1)) %>%
                    ungroup() %>%
                    filter(result == 1) %>%
                    haven::as_factor() %>%
                    select(Question, c0popcat7, Percentage)
            }
        )
    )

    bind_rows(all, split) %>%
        tidyr::pivot_wider(names_from = c0popcat7, values_from = Percentage)

}

test_fig5.13 <- function() {

    q_sel <- tibble::tibble(
        question_label = c(
            "Agency leadership",
            "Animal control worker",
            "Behavioral health staff",
            "Business and financial operations staff",
            "Community health worker",
            "Environmental health worker",
            "Epidemiologist/statistician",
            "Health educator",
            "Information systems specialist",
            "Laboratory worker",
            "Licensed practical or vocational nurse",
            "Nusing aide and home health aide",
            "Nutritionist",
            "Office and administrative support staff",
            "Oral health care professional",
            "Prepareness staff",
            "Public health physician",
            "Public information professional",
            "Registered nurse"
            ),
        question_id = c(
            "c5q63b", "c5q62b", "c5q52b", "c5q64b", 
            "c5q61b", "c5q55b", "c5q47b", "c5q48b", 
            "c5q50b", "c5q60b", "c5q57b", "c5q58b",
            "c5q49b", "c5q65b", "c5q59b", "c5q56b",
            "c5q44b", "c5q51b", "c5q43b")
    )
    
    all <- do.call(
        bind_rows,
            apply(
                q_sel,
                1,
                \(x) {
                    q_id <- as.character(x[2])
                    master_data %>%
                        haven::as_factor() %>%
                        left_join(add_weights(q_id), by = c("c0popcat7", "core_denom")) %>%
                        filter(!is.na(.data[[q_id]])) %>%
                        mutate(value = .data[[q_id]]) %>%
                        mutate(value_weighted = .data[[q_id]]*weights)%>%
                        summarise(
                            N = n(),
                            Total_Weighted = sum(value_weighted, na.rm = TRUE),
                            Total = sum(value, na.rm = TRUE)
                        ) %>%
                        mutate(Question = as.character(x[1]))
                }
            )
        )

}

test_expenditures_fig6.2_2022 <- function(signif_digits = 3) {

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
        filter(!is.na(value)) %>%
        dplyr::group_by(c0popcat7) %>%
        dplyr::summarise(
            Mean = mean(value, na.rm = TRUE),
            `25th percentile` = quantile(value, 0.25, na.rm = TRUE),
            `50th percentile (Median)` = median(value, na.rm = TRUE),
            `75th percentile` = quantile(value, 0.75, na.rm = TRUE),
            .groups = "drop"
            )

    dplyr::bind_rows(overall, split_lhd) %>%
        dplyr::mutate_if(is.numeric, signif, signif_digits)
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