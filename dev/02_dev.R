# NACCHO's Profile Study Application
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add Packages
usethis::use_package("highcharter")
usethis::use_package("bslib")
usethis::use_package("shinyWidgets")
usethis::use_package("fontawesome")

## Add modules ----
golem::add_module(name = "key_findings", with_test = TRUE, open = FALSE)
golem::add_module(name = "data_stories", with_test = TRUE, open = FALSE)
golem::add_module(name = "workforce", with_test = TRUE, open = FALSE)
golem::add_module(name = "finance", with_test = TRUE, open = FALSE)
golem::add_module(name = "services", with_test = TRUE, open = FALSE)
golem::add_module(name = "policy", with_test = TRUE, open = FALSE)
golem::add_module(name = "topic_areas", with_test = TRUE, open = FALSE)
golem::add_module(name = "playground", with_test = TRUE, open = FALSE)


## Add helper functions ----
golem::add_fct("helpers", with_test = TRUE, open = FALSE)
golem::add_utils("helpers", with_test = TRUE, open = FALSE)




# come back ---------------------------------------------------------------


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("my_styles")
golem::add_sass_file("custom")


## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("nacchoProfile")
devtools::build_vignettes()



## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##



# come back up ------------------------------------------------------------


## (You'll need GitHub there)
usethis::use_github()


# GitHub Actions
usethis::use_github_action()
# pr-commands
# check-standard


# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
