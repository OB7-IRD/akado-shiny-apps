# Building a Prod-Ready, Robust Shiny Application.
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
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "AkadoR", # The Name of the package containing the App
  pkg_title = "AkadoR", # The Title of the package containing the App
  pkg_description = "The AKADO application performs a series of tests on the data to check its consistency. The aim is to determine the quality of the data according to specific business rules defined by experts.", # The Description of the package containing the App
  # authors = person(
  #   given = "Oceane", # Your First Name
  #   family = "Bouhineau", # Your Last Name
  #   email = "oceane.bouhineau@ird.fr", # Your email
  #   comment = c(ORCID = "0000-0001-7418-2925"),
  #   role = c("aut", "cre") # Your role (here author/creator)
  # ),
  author_first_name = "Oceane", # Your First Name
  author_last_name = "Bouhineau", # Your Last Name
  author_email = "oceane.bouhineau@ird.fr", # Your Email
  author_orcid = "0000-0001-7418-2925",
  repo_url = "https://forge.ird.fr/marbec/ob7/quality/akador", # The URL of the GitHub Repo (optional),
  pkg_version = "0.0.0.9000" # The Version of the package containing the App
)

## Set {golem} options ----
golem::set_golem_options()

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_ccby_license() # You can set another license here
usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon("inst/app/www/akado.png") # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
