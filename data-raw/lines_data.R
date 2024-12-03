#' Load and Prepare Lines Data
#'
#' This function reads and processes `lines_data` from a CSV file.
#' The data is loaded from `"data-raw/lines_data.csv"`, transformed, and saved as a dataset for internal package use.
#'
#' @return A processed `lines_data` object saved within the package for internal use.
#'
#' @details
#' This script reads a raw CSV file containing data about lines, applies data transformations,
#' and saves `lines_data` using `usethis::use_data()` for use within the package.
#'
#' @examples
#' \dontrun{
#'   # To prepare and save the data within the package
#'   lines_data()
#' }
#'
#' @seealso \code{\link[utils]{read.csv}}, \code{\link[usethis]{use_data}}
#'
#' @importFrom utils read.csv
#' @importFrom usethis use_data
#' @importFrom dplyr mutate case_when
#' @keywords internal
#' @name lines_data

  lines_data <- utils::read.csv("data-raw/time_data.csv") %>%
    dplyr::mutate(
      category = dplyr::case_when(
        category %in% c("Under 30", "30-39") ~ "Under 40",
        TRUE ~ category
      ),
      subtitle = dplyr::case_when(
        filter == "c0govcat" ~ "By Governance",
        filter == "urbanrural2022" ~ "By Rurality",
        filter == "c0popcat3" ~ "By LHD Size",
        filter == "censusregion" ~ "By Region",
        TRUE ~ ""
      )
    )

  # Save the processed data for package use
  usethis::use_data(lines_data, overwrite = TRUE)
