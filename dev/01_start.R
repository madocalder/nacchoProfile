# NACCHO's Profile Study Application
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\

golem::fill_desc(
  pkg_name = "nacchoProfile", # The Name of the package containing the App
  pkg_title = "NACCHO Profile Study Dashboard", # The Title of the package containing the App
  pkg_description = "A Shiny application that visualizes findings from NACCHO's National Profile of Local Health Departments.", # The Description of the package containing the App
  author_first_name = "Lindsay", # Your First Name
  author_last_name = "Jorgenson", # Your Last Name
  author_email = "ljorgenson@astho.org", # Your Email
  repo_url = NULL, # The URL of the GitHub Repo (optional),
  pkg_version = "0.0.0.9000" # The Version of the package containing the App
)

## Set {golem} options ----
golem::set_golem_options()

## Install the required dev dependencies ----
golem::install_dev_deps()

## Add license and readme
# usethis::use_proprietary_license("@National Association of City & County Health Officials (NACCHO), all rights reserved") # You can set another license here
# We decided to make this application open-source under GPL-3
usethis::use_gpl_license(version = 3, include_future = TRUE)

usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
usethis::use_code_of_conduct(contact = "ljorgenson@astho.org")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
golem::use_favicon("inst/app/www/favicon.ico") # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
