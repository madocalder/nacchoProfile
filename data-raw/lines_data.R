#' Load and Prepare Lines Data
#'
#' This function reads and prepares the `lines_data` from a CSV file.
#' The data is loaded from `"data-raw/lines_data.csv"` and then saved as a dataset for the package.
#'
#' @return The `lines_data` object is saved using `usethis::use_data()` for use within the package.
#'
#' @details
#' This script reads a raw CSV file containing data about lines and processes it for inclusion in the package.
#' The `lines_data` is subsequently documented and ready for use within the package.
#'
#' @examples
#' \dontrun{
#'   # To read the data and store it in the package
#'   lines_data <- utils::read.csv("data-raw/lines_data.csv")
#'   usethis::use_data(lines_data, overwrite = TRUE)
#' }
#'
#' @seealso \code{\link[utils]{read.csv}}, \code{\link[usethis]{use_data}}
#'
#' @importFrom utils read.csv
#' @importFrom usethis use_data
#' @export
# code to prepare lines data------

lines_data <- utils::read.csv("data-raw/time_data.csv")%>%
  dplyr::mutate(
    category = case_when(
    category %in% c("Under 30", "30-39") ~ "Under 40",
    TRUE ~ category)
  )

# save data ---------------------------------------------------------------
usethis::use_data(lines_data, overwrite = TRUE)
devtools::document()
