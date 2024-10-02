# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_shinyappsio_file()

## Deploy to Posit Connect or ShinyApps.io ----

## In command line:
rsconnect::deployApp(
  appName = "cohhio-youth-data-dashboard",
  appTitle = "COHHIO Youth Data Dashboard",
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R",
    ".Renviron",
    ".Rbuildignore",
    "hkey.RDS"
  ),
  account = "ohiobalanceofstatecoc",
  appId =  "7831250",
  lint = FALSE,
  forceUpdate = TRUE
)
