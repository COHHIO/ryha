# Read _brand.yml
brand <- yaml::read_yaml(app_sys("_brand.yml"))

# Store color palette in a global object
palette <- brand$color$palette

#' Placeholder Function for Import Declaration
#'
#' `additional_required_packages()` is intentionally left empty. Its purpose is
#' to declare package dependencies via roxygen2's import tag for packages that
#' don't have explicit namespaced calls in the codebase. This allows
#' attachment::att_amend_desc() to detect these dependencies and add them to
#' the DESCRIPTION file, making them available for renv::snapshot().
#'
#' @details
#' pkgload is required for automatic deployment via GitHub Actions.
#'
#' @import pkgload
#' @noRd
additional_required_packages <- function() {}
