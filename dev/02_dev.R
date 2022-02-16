# Building a Prod-Ready, Robust Shiny Application.
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
## Add one line by package you want to add as dependency
usethis::use_package( "magrittr ")
usethis::use_package( "yaml")
usethis::use_package( "vroom" )
usethis::use_package( "dplyr" )
usethis::use_package( "tidyr" )
usethis::use_package( "tibble" )
usethis::use_package( "ggplot2")
usethis::use_package( "stringr" )
usethis::use_package( "data.table")
usethis::use_package( "bslib" )
usethis::use_package( "iheatmapr" )

usethis::use_package( "gridExtra" )

usethis::use_package( "r2d3")
usethis::use_package("DT")

usethis::use_package( "plotly")
usethis::use_package( "shinyjs")
usethis::use_package( "shinycssloaders")
usethis::use_package( "bsplus" )
usethis::use_package( "htmlwidgets" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "cohort_overview.js" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "cohort_overview.css" )

## Add internal datasets ----
## If you have data in your package
#usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation
usethis::use_pkgdown()
## Vignette ----
usethis::use_vignette("config")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
#usethis::use_github()
#usethis::use_travis()
#usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

