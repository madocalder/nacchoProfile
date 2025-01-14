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


## Complete data frame with all years
partnerships <- data.frame(
  group = c(
    # Healthcare Partners
    rep("Healthcare Partnerships", 7 * 4),
    # Community-Based Partners
    rep("Community-Based Partnerships", 8 * 4),
    # Government Partners
    rep("Government Partnerships", 7 * 4)
  ),
  category = c(
    # Healthcare Partners
    rep("Emergency responders", 4),
    rep("Hospitals", 4),
    rep("Physician practices/medical groups", 4),
    rep("Community health centers", 4),
    rep("Mental health/substance abuse providers", 4),
    rep("Veterinarians", 4),
    # Community-Based Partners
    rep("K-12 schools", 4),
    rep("Community-based non-profits", 4),
    rep("Media", 4),
    rep("Businesses", 4),
    rep("Faith communities", 4),
    rep("Colleges or universities", 4),
    rep("Libraries", 4),
    # Government Partners
    rep("Criminal justice system", 4),
    rep("Economic and community development", 4),
    rep("Local planning", 4),
    rep("Housing", 4),
    rep("Parks and recreation", 4),
    rep("Transportation", 4),
    rep("Tribal government", 4)
  ),
  year = rep(c(2008, 2016, 2019, 2022), times = 6 + 7 + 7),
  y = c(
    # Healthcare Partners
    61, 39, 44, 52,
    57, 35, 47, 55,
    38, 26, 42, 43,
    38, 21, 30, 34,
    24, 23, 32, 32,
    NA, 19, 34, 20,
    # Community-Based Partners
    59, 38, 49, 55,
    45, 30, 53, 52,
    34, 18, 39, 41,
    28, 10, 23, 22,
    19, 8, 19, 25,
    18, 7, 26, 30,
    13, 8, 12, 15,
    # Government Partners
    26, 10, 18, 18,
    19, 14, 25, 30,
    17, 9, 17, 20,
    11, 9, 12, 15,
    11, 7, 15, 18,
    NA, 7, 15, 21,
    NA, 7, 12, 15
  ),
  Percent_Informal = c(
    # Healthcare Partners
    rep(NA, 3), 98, rep(NA, 3), 98, rep(NA, 3), 95,
    rep(NA, 3), 93, rep(NA, 3), 91, rep(NA, 3), 76,
    # Community-Based Partners
    rep(NA, 3), 99, rep(NA, 3), 97, rep(NA, 3), 95,
    rep(NA, 3), 94, rep(NA, 3), 93, rep(NA, 3), 89,
    rep(NA, 3), 83,
    # Government Partners
    rep(NA, 3), 88, rep(NA, 3), 88, rep(NA, 3), 83,
    rep(NA, 3), 81, rep(NA, 3), 79, rep(NA, 3), 71,
    rep(NA, 3), 47
  ),
  Percent_Formal = c(
    # Healthcare Partners
    rep(NA, 3), 79, rep(NA, 3), 75, rep(NA, 3), 46,
    rep(NA, 3), 56, rep(NA, 3), 53, rep(NA, 3), 22,
    # Community-Based Partners
    rep(NA, 3), 78, rep(NA, 3), 64, rep(NA, 3), 28,
    rep(NA, 3), 49, rep(NA, 3), 40, rep(NA, 3), 64,
    rep(NA, 3), 33,
    # Government Partners
    rep(NA, 3), 46, rep(NA, 3), 41, rep(NA, 3), 40,
    rep(NA, 3), 35, rep(NA, 3), 33, rep(NA, 3), 29,
    rep(NA, 3), 20
  )
)

partnerships <- partnerships %>%
  mutate(variable = case_when(
    group == "Healthcare Partnerships" ~ "health_part",
    group == "Community-Based Partnerships" ~ "comm_part",
    group == "Government Partnerships" ~ "gov_part"  # Assign NA for non-Partnerships categorys
  ))

  # Save the processed data for package use
  usethis::use_data(partnerships, overwrite = TRUE)

