
#' @importFrom dplyr filter mutate
#' @importFrom purrr map_dfr
#' @noRd
report2022_leadership_fig4.1 <- function() {

    questions <- c("c4q26", "c4q28", "c4q29", "racenotwhite")
    
    map_dfr(questions, return_survey_weighted_counts) %>%
        mutate(Answer = case_when(
            .data$Question == "racenotwhite" & .data$Answer == 1 ~ "racenotwhite",
            .data$Question == "racenotwhite" & .data$Answer == 0 ~ "racewhite",
            TRUE ~ .data$Answer)) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        filter(
            .data$Answer %in% c("part-time", "Hispanic/Latino", "female", "racenotwhite")
        )
}

#' @importFrom dplyr mutate
#' @noRd
report2022_leadership_fig4.2 <- function() {
    return_survey_weighted_counts("age_cat") %>%
        mutate(AnswerLabel = label_remap[as.character(.data$Answer)])
}

#' @importFrom dplyr bind_rows mutate
#' @noRd
report2022_leadership_fig4.4 <- function() {

    all_lhds <- return_survey_weighted_statistics("tenure")
    split_lhds <- return_survey_weighted_statistics("tenure", grps = "c0popcat3", popcat = "c0popcat3")

    bind_rows(all_lhds, split_lhds) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)])


}

#' @importFrom dplyr bind_rows mutate
#' @noRd
report2022_leadership_fig4.5 <- function() {
    
    all_lhds <- return_survey_weighted_counts("c4q34")
    split_lhds <- return_survey_weighted_counts("c4q34", grps = "c0popcat3", popcat = "c0popcat3")
    split_urb <- return_survey_weighted_counts("c4q34", grps = "urbanrural2022", popcat = "c0popcat3")

    bind_rows(all_lhds, split_lhds, split_urb) %>%
        mutate(AnswerLabel = label_remap[as.character(.data$Answer)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)])
}

#' @importFrom dplyr bind_rows filter mutate
#' @importFrom purrr map_dfr pmap_dfr
#' @noRd
report2022_leadership_fig4.6 <- function() {

    args1 <- list(
        c("c4q502a", "c4q502b", "c4q502c"), 
        grps = "c0popcat3", 
        popcat = "c0popcat3"
        )

    args2 <- list(
        c("c4q502a", "c4q502b", "c4q502c"), 
        grps = "urbanrural2022", 
        popcat = "c0popcat3"
        )
    
    all_lhds <- map_dfr(args1[[1]], return_survey_weighted_counts)
    split_lhds <- pmap_dfr(args1, return_survey_weighted_counts)
    split_urb <- pmap_dfr(args2, return_survey_weighted_counts)
    
    bind_rows(all_lhds, split_lhds, split_urb) %>%
        filter(.data$Answer == "checked") %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)])
   
}

#' @importFrom dplyr mutate case_when
#' @noRd
report2022_workforce_fig5.1 <- function() {

    return_survey_weighted_counts("FTE_numcat") %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        mutate(AnswerLabel = case_when(
            .data$Answer == "<5" ~ "Fewer than 5",
            .data$Answer == "200+" ~ "200 or more",
            TRUE ~ .data$Answer
        ))
}

#' @importFrom dplyr bind_rows mutate rename select all_of
#' @importFrom tidyr pivot_wider
#' @noRd
report2022_workforce_fig5.2 <- function() {

    # does not match on the actual non weighted data
    # when grouping

    args <- list(
        c("c5q36", "c5q37"), 
        grps = "c0popcat7", 
        popcat = "c0popcat7",
        wtype = "weights_static"
        )

    sel_cols <- c(
        "GroupLabel", 
        "AnswerLabel", 
        "Mean", 
        "Median"
        )
    
    all_lhds <- purrr::map_dfr(args[[1]], return_survey_weighted_statistics)
    split_lhds <- purrr::pmap_dfr(args, return_survey_weighted_statistics)

    bind_rows(all_lhds, split_lhds) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        rename(Median = "Quantile_q50") %>%
        select(all_of(sel_cols)) %>%
        pivot_wider(names_from = .data$AnswerLabel, values_from = c(.data$Mean, .data$Median))

}

#' @importFrom dplyr bind_rows mutate rename select all_of
#' @importFrom tidyr pivot_wider
#' @importFrom purrr pmap_dfr
#' @noRd
report2022_workforce_fig5.6 <- function() {

    args <- list(
        c("c5q36", "c5q37"), 
        wtype = c("weights_static", "weights_static")
        )
    
    pmap_dfr(args, return_survey_weighted_statistics) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)])
   
}

#' @importFrom dplyr bind_rows mutate
#' @noRd
report2022_workforce_fig5.8 <- function() {

    all_lhds <- return_survey_weighted_statistics("fte_bypop")
    split_lhds <- return_survey_weighted_statistics("fte_bypop", grps = "c0popcat3", popcat = "c0popcat3")

    bind_rows(all_lhds, split_lhds) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)])
}

#' @importFrom dplyr mutate filter
#' @noRd
report2022_workforce_fig5.10 <- function() {

    return_survey_weighted_counts("jobsgoneany") %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        filter(.data$Answer == "1")

}

#' @importFrom dplyr bind_rows mutate select all_of
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#' @noRd
report2022_workforce_fig5.12 <- function() {

    # very small differences in the percentages

    args <- list(
        c("c5q63a", "c5q62a", "c5q52a", "c5q64a", 
        "c5q61a", "c5q55a", "c5q47a", "c5q48a", 
        "c5q50a", "c5q60a", "c5q57a", "c5q58a",
        "c5q49a", "c5q65a", "c5q59a", "c5q56a",
        "c5q44a", "c5q51a", "c5q43a"), 
        grps = "c0popcat7", 
        popcat = "c0popcat7"
        )

    all_lhds <- map_dfr(args[[1]], return_survey_weighted_counts)
    split_lhds <- pmap_dfr(args, return_survey_weighted_counts)
    
    sel_cols <- c(
        "GroupLabel", 
        "AnswerLabel", 
        "Percentage"
        )
    
    bind_rows(all_lhds, split_lhds) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        filter(.data$Answer == "yes") %>%
        select(all_of(sel_cols)) %>%
        pivot_wider(names_from = .data$GroupLabel, values_from = .data$Percentage)

}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate across where left_join select
#' @noRd
compare_results_fig5.12 <- function() {

    report <- tibble::tibble(
        AnswerLabel = c(
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
            "Nursing aide and home health aide", 
            "Nutritionist", 
            "Office and administrative support staff", 
            "Oral health care professional", 
            "Preparedness staff", 
            "Public health physician", 
            "Public information professional", 
            "Registered nurse"), 
        All_LHDs = c("0.81", "0.06", "0.14", "0.51", "0.38", "0.69", "0.37", "0.6", "0.19", "0.15", "0.34", "0.19", "0.46", "0.91", "0.14", "0.63", "0.25", "0.32", "0.92"), 
        Min25k = c("0.7", "0.05", "0.04", "0.31", "0.22", "0.52", "0.12", "0.38", "0.04", "0.04", "0.26", "0.14",  "0.24", "0.85", "0.05", "0.43", "0.09", "0.16", "0.85"), 
        `25k_49k` = c("0.83", "0.06", "0.08", "0.48", "0.33", "0.74", "0.25", "0.64", "0.13", "0.1", "0.3", "0.2", "0.44", "0.93", "0.11", "0.63", "0.18", "0.22", "0.96"), 
        `50k_99k` = c("0.88", "0.08", "0.23", "0.63", "0.45", "0.83", "0.53", "0.74", "0.16", "0.16", "0.4", "0.23", "0.62", "0.92", "0.17", "0.75", "0.32", "0.34", "0.97"), 
        `100k_249k` = c("0.95", "0.05", "0.15", "0.72", "0.53", "0.85", "0.72", "0.8", "0.35", "0.24", "0.4", "0.15", "0.68", "0.96", "0.24", "0.84", "0.40", "0.55", "0.98"), 
        `250k_499k` = c("0.98", "0.04", "0.42", "0.9", "0.72", "0.92", "0.84", "0.88", "0.62", "0.4", "0.5", "0.26", "0.76", "0.98", "0.24", "0.92", "0.6", "0.76", "0.96"), 
        `500k_999k` = c("1", "0.16", "0.49", "0.98", "0.89", "0.89", "1", "0.89", "0.84", "0.56", "0.71", "0.27", "0.87", "1", "0.36", "0.98", "0.72", "0.93", "1"), 
        `Plus1000k` = c("0.97", "0.17", "0.5", "1", "0.8", "0.77", "1", "0.93", "0.77", "0.83", "0.8", "0.4", "0.97", "1", "0.67", "1", "0.97", "0.83", "1")
        ) |>
        mutate(across(.data$All_LHDs:.data$Plus1000k, \(x) as.numeric(x))) |>
        mutate(across(where(is.numeric), \(x) {round(x * 100, 0)}))

    results <- report2022_workforce_fig5.12() |>
        mutate(across(where(is.numeric), \(x) {round(x, 0)}))

    colnames(results) <- colnames(report)

    report <- report |>
        left_join(results, by = "AnswerLabel") |>
        mutate(
            All_LHDs = paste0(.data$All_LHDs.x - .data$All_LHDs.y, "(", .data$All_LHDs.x, ", ", .data$All_LHDs.y, ")"),
            Min25k = paste0(.data$Min25k.x - .data$Min25k.y, "(", .data$Min25k.x, ", ", .data$Min25k.y, ")"),
            `25k_49k` = paste0(.data$`25k_49k.x` - .data$`25k_49k.y`, "(", .data$`25k_49k.x`, ", ", .data$`25k_49k.y`, ")"),
            `50k_99k` = paste0(.data$`50k_99k.x` - .data$`50k_99k.y`, "(", .data$`50k_99k.x`, ", ", .data$`50k_99k.y`, ")"),
            `100k_249k` = paste0(.data$`100k_249k.x` - .data$`100k_249k.y`, "(", .data$`100k_249k.x`, ", ", .data$`100k_249k.y`, ")"),
            `250k_499k` = paste0(.data$`250k_499k.x` - .data$`250k_499k.y`, "(", .data$`250k_499k.x`, ", ", .data$`250k_499k.y`, ")"),
            `500k_999k` = paste0(.data$`500k_999k.x` - .data$`500k_999k.y`, "(", .data$`500k_999k.x`, ", ", .data$`500k_999k.y`, ")"),
            Plus1000k = paste0(.data$Plus1000k.x - .data$Plus1000k.y, "(", .data$Plus1000k.x, ", ", .data$Plus1000k.y, ")")
        ) |>
        select(AnswerLabel, All_LHDs:Plus1000k)
}

#' @importFrom dplyr mutate
#' @importFrom purrr map_dfr
#' @noRd
report2022_workforce_fig5.13 <- function() {

    # still some issues with the 95% confidence interval calculation
    # doesn't match precesely the report

    args <- list(
        c(
            "newc5q63b", "newc5q62b", 
            "newc5q52b", "newc5q64b", 
            "newc5q61b", "newc5q55b", 
            "newc5q47b", "newc5q48b", 
            "newc5q50b", "newc5q60b", 
            "newc5q57b", "newc5q58b",
            "newc5q49b", "newc5q65b", 
            "newc5q59b", "newc5q56b",
            "newc5q44b", "newc5q51b", 
            "newc5q43b"),
        wtype = "weights_static"
        )

    pmap_dfr(args, return_survey_weighted_statistics) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        mutate(
            CI_L = .data$Total - .data$Total_SE,
            CI_R = .data$Total + .data$Total_SE
            )

}

#' @importFrom dplyr mutate bind_rows
#' @importFrom purrr map_dfr
#' @noRd
report2022_finance_fig6.2 <- function() {

    # still some issues with the percentile calculations
    # some rare cases don't match precesely the report

    all_lhds <- return_survey_weighted_statistics("c3q15")
    split_lhds <- return_survey_weighted_statistics("c3q15", grps = "c0popcat7", popcat = "c0popcat7")

    bind_rows(all_lhds, split_lhds) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        rename(
            `25th percentile` = "Quantile_q25", 
            `50th percentile (Median)` = "Quantile_q50", 
            `75th percentile` = "Quantile_q75"
            )

}

#' @importFrom dplyr mutate
#' @noRd
report2022_finance_fig6.6 <- function() {
    return_survey_weighted_statistics("totexp_percap") %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)])

}

#' @importFrom dplyr mutate
#' @noRd
report2022_finance_fig6.7 <- function() {
    
    args <- list(
        c(
            "c3q17p", "c3q17c", 
            "c3q17d", "c3q17e", 
            "c3q17r", "c3q17i", 
            "c3q17j", "c3q17v", 
            "c3q17h", "c3q17m"),
        wtype = "weights_static"
        )

    pmap_dfr(args, return_survey_weighted_statistics) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        mutate(Percentage = .data$Total/sum(.data$Total)*100)
    
}

#' @importFrom dplyr mutate
#' @importFrom purrr pmap_dfr
#' @noRd
report2022_finance_fig6.9 <- function() {

    args <- list(
        c(
        "local_percap",
        "state_percap",
        "fed_percap",
        "clinical_percap"),
        wtype = "weights_static"
    )

    all_lhds <- pmap_dfr(args, return_survey_weighted_statistics) %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)])

}

#' @importFrom dplyr filter mutate select all_of
#' @noRd
report2022_finance_fig6.10 <- function() {

    # special case, we need to exclude an answer to get the right percentage
    # perhaps a new argument in the function?

    sel_answer <- c(
        "less than previous year's budget",
        "greater than the previous year's budget")

    sel_cols <- c(
        "GroupLabel", 
        "AnswerLabel", 
        "Percentage")

    return_survey_weighted_counts("c10q301", wtype = "weights_static") %>%
        filter(.data$Answer != "do not know") %>%
        mutate(Percentage = .data$N/sum(.data$N) * 100) %>%
        mutate(AnswerLabel = label_remap[as.character(.data$Answer)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        filter(.data$Answer %in% sel_answer) %>%
        select(all_of(sel_cols))

}

#' @importFrom dplyr bind_rows mutate select all_of
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr pmap_dfr
#' @noRd
report2022_program_fig7.3 <- function() {

    args1 <- list(
        c("c6q147a", "c6q56a", "c6q55a", "c6q148a"), 
        grps = "c0popcat3", 
        popcat = "c0popcat3"
        )

     args2 <- list(
        c("c6q147a", "c6q56a", "c6q55a", "c6q148a"), 
        grps = "urbanrural2022", 
        popcat = "c0popcat3"
        )

    all_lhds <- map_dfr(args1[[1]], return_survey_weighted_counts)
    split_lhds <- pmap_dfr(args1, return_survey_weighted_counts)
    split_urb <- pmap_dfr(args2, return_survey_weighted_counts)
    
    sel_cols <- c(
        "GroupLabel", 
        "AnswerLabel", 
        "Percentage")

    bind_rows(all_lhds, split_lhds, split_urb) %>%
        filter(.data$Answer == "checked") %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        select(all_of(sel_cols)) %>%
        pivot_wider(names_from = .data$GroupLabel, values_from = .data$Percentage)
    
}

#' @importFrom dplyr bind_rows mutate select all_of
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr pmap_dfr
#' @noRd
report2022_program_fig7.4 <- function() {

    args1 <- list(
        c(
            "c6q59a", "c6q58a", "c6q57a", 
            "c6q63a", "c6q142a", "c6q62a", 
            "c6q60a", "c6q61a", "c6q67a",
            "c6q66a", "c6q65a"), 
        grps = "c0popcat3", 
        popcat = "c0popcat3"
        )

    args2 <- list(
        c(
            "c6q59a", "c6q58a", "c6q57a", 
            "c6q63a", "c6q142a", "c6q62a", 
            "c6q60a", "c6q61a", "c6q67a",
            "c6q66a", "c6q65a"), 
        grps = "urbanrural2022", 
        popcat = "c0popcat3"
        )

    all_lhds <- map_dfr(args1[[1]], return_survey_weighted_counts)
    split_lhds <- pmap_dfr(args1, return_survey_weighted_counts)
    split_urb <- pmap_dfr(args2, return_survey_weighted_counts)
    
    sel_cols <- c(
        "GroupLabel", 
        "AnswerLabel", 
        "Percentage")

    bind_rows(all_lhds, split_lhds, split_urb) %>%
        filter(.data$Answer == "checked") %>%
        mutate(AnswerLabel = q_remap[as.character(.data$Question)]) %>%
        mutate(GroupLabel = label_remap[as.character(.data$Group)]) %>%
        select(all_of(sel_cols)) %>%
        pivot_wider(names_from = .data$GroupLabel, values_from = .data$Percentage)
    
}