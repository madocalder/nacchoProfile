#' Formal Local Health Department (LHD) Partnerships and Collaborations Dataset
#'
#' A dataset containing information on formal partnerships and collaborations of Local Health Departments (LHDs)
#' with various healthcare, community-based, and government organizations over time. The data reflects the
#' proportion of LHDs reporting formal collaborations with different types of organizations across selected years.
#'
#' @format A data frame with 88 rows and 4 variables:
#' \describe{
#'   \item{group}{Type of organization that LHDs partnered with. One of three values:
#'                   \code{"Healthcare Partners"}, \code{"Community-Based Partners"}, or \code{"Government Partners"}.}
#'   \item{category}{The specific type of organization within each category, e.g., \code{"Emergency responders"},
#'                       \code{"K-12 schools"}, \code{"Local planning"}, etc.}
#'   \item{year}{Year of reported partnership data. Available years are \code{2008}, \code{2016}, \code{2019}, and \code{2022}.}
#'   \item{y}{Percentage of LHDs reporting a formal partnership with each organization type in the given year.
#'                     Values are in percentage form, and some may be \code{NA} if data for that year/partner type is missing.}
#' }
#' @details
#' This dataset captures the evolution of formal collaborations (those with shared resources, personnel, or formal
#' agreements) that LHDs had with various organizations from 2008 to 2022. The data shows trends across three main
#' partner categories:
#' \itemize{
#'   \item Healthcare Partners: Includes organizations like emergency responders, hospitals, and mental health providers.
#'   \item Community-Based Partners: Includes schools, universities, non-profits, and faith communities.
#'   \item Government Partners: Includes local planning departments, criminal justice systems, and transportation agencies.
#' }
#' @source Extracted from visual data in a provided chart.
#'
#' @examples
#' # Load the dataset
#' data(partnerships)
#'
#' # View the first few rows of the dataset
#' head(partnerships)
#'
#' # Summary of partnerships by category
#' summary(partnerships$category)
#'
#' @keywords data
#' @name partnerships
"partnerships"

partnerships <- function() {
  partnerships <- data.frame(
    group = c(
      # Healthcare Partners (7 partner types * 4 years = 28 entries)
      rep("Healthcare Partners", 4 * 7),
      # Community-Based Partners (8 partner types * 4 years = 32 entries)
      rep("Community-Based Partners", 4 * 8),
      # Government Partners (7 partner types * 4 years = 28 entries)
      rep("Government Partners", 4 * 7)
    ),
    category = c(
      # Healthcare Partners
      rep("Emergency responders", 4),
      rep("Hospitals", 4),
      rep("Community health centers", 4),
      rep("Physician practices/medical groups", 4),
      rep("Health insurers", 4),
      rep("Mental health/substance abuse providers", 4),
      rep("Veterinarians", 4),
      # Community-Based Partners
      rep("K-12 schools", 4),
      rep("Colleges or universities", 4),
      rep("Community-based non-profits", 4),
      rep("Cooperative extensions", 4),
      rep("Faith communities", 4),
      rep("Businesses", 4),
      rep("Media", 4),
      rep("Libraries", 4),
      # Government Partners
      rep("Local planning", 4),
      rep("Criminal justice system", 4),
      rep("Parks and recreation", 4),
      rep("Transportation", 4),
      rep("Economic and community development", 4),
      rep("Housing", 4),
      rep("Tribal government", 4)
    ),
    year = rep(c(2008, 2016, 2019, 2022), times = 7 + 8 + 7),
    y = c(
      # Healthcare Partners data (28 values)
      61, 39, 44, 52,
      57, 35, 47, 55,
      38, 26, 42, 43,
      38, 21, 30, 34,
      24, 23, 32, 27,
      NA, 19, 34, 32,
      NA, 14, 19, 20,
      # Community-Based Partners data (32 values)
      59, 38, 49, 55,
      45, 30, 53, 52,
      34, 18, 39, 41,
      28, 10, 23, 22,
      19, 8, 19, 25,
      18, 7, 26, 30,
      13, 8, 12, 15,
      11, 7, 16, 22,
      # Government Partners data (28 values)
      26, 10, 18, 18,
      19, 14, 25, 30,
      17, 9, 17, 20,
      11, 9, 12, 15,
      11, 7, 15, 18,
      NA, 7, 15, 21,
      NA, 7, 12, 15
    )
  )

  # Save the processed data for package use
  usethis::use_data(partnerships, overwrite = TRUE)
}

# Call this function to create and save the `partnerships` data frame in the package
partnerships()
