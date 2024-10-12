#' Prepare Boxplot Data with Filters
#'
#' This script generates boxplot data for different financial and demographic variables across various filters such as
#' urban-rural classification, government category, population category, and census region. The dataset contains data
#' for boxplots of revenue and expenditure-related variables for Local Health Departments (LHDs).
#'
#' @details
#' The script performs the following steps:
#' - Define the variables of interest, including total expenditures per capita (`totexp_percap`), non-weighted total revenue (`non_weighted_revenue_total`), and non-weighted revenue per capita (`non_weighted_revenue_percap`).
#' - Apply filters such as "urbanrural2022", "c0govcat", "c0popcat3", and "censusregion".
#' - Generate boxplot data for each combination of variable and filter, as well as unfiltered data.
#' - Save the resulting dataset using `usethis::use_data()`.
#'
#' @param boxplot_args A list containing the variables (`args`) and filters (`filters`) to be used for generating the boxplot data.
#'
#' @return Saves the dataset `boxplot_list`, which contains boxplot data for each variable-filter combination, as an `.rda` file in the package.
#'
#' @seealso [boxplot_data()] for the function used to generate the boxplot data.
#'
#' @examples
#' # Load all package functions
#' devtools::load_all()
#'
#' # Prepare and save boxplot data
#' boxplot_args <- list(
#'   args = c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap"),
#'   filters = c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
#' )
#'
#' @export
NULL

# Code to prepare `boxplot_data` dataset ----
devtools::load_all()
start_time <- Sys.time() # get start time

boxplot_args <- list(
  args = c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap"),
  filters = c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
)

boxplot_list <- list() # create an empty list

# Loop through variables and filters to generate boxplot data
for (i in 1:length(boxplot_args[[1]])) {
  for (j in 1:length(boxplot_args[[2]])) {

    arg1 <- boxplot_args[[1]][[i]]
    arg2 <- boxplot_args[[2]][[j]]

    # Generate boxplot data for each variable-filter combination
    boxplot_list[[paste0("boxplot_data_", arg1, "_", arg2)]] <- boxplot_data(arg1, arg2)

    # Generate boxplot data without filter (for "All LHDs")
    boxplot_list[[paste0("boxplot_data_", arg1, "_all")]] <- boxplot_data(arg1)
  }
}

# Remove names from each element in the list
for (i in seq_along(boxplot_list)) {
  names(boxplot_list[[i]]) <- NULL
}

# Print elapsed time
end_time <- Sys.time() - start_time # calculate time difference
print(end_time)

# Save the boxplot_list dataset
usethis::use_data(boxplot_list, overwrite = TRUE)
