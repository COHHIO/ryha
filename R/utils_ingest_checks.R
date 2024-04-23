# The following family of functions define the checks that will be run during
# the ETL process

#' Ensure All HMIS Data was Uploaded
#'
#' @details Compare the file names within the uploaded .zip file to the expected
#' file names (stored in the 'HMISmetadata' data object within this R package)
#'
#' @param dir The directory path containing the extracted files from the
#' uploaded .zip file.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{valid}: A logical value indicating whether all expected files
#'   are present in the uploaded directory.
#'   \item \code{missing_file_names}: A character vector containing the names of
#'   files that are expected but missing in the uploaded directory.
#' }
#'
#' @export
check_file_names <- function(dir) {

  # Retrieve the full paths to each individual file extracted from the .zip file
  paths <- fs::dir_info(dir) |>
    dplyr::pull(path)

  # Retrieve the related directory
  dir <- dirname(paths) |> unique()

  # Remove the directory from the path, so that we are just left with the file
  # names themselves (e.g., "path/to/data.csv" --> "data.csv")
  file_names <- paths |>
    stringr::str_replace(
      pattern = paste0(dir, "/"),
      replacement = ""
    )

  # Assume that there were no discrepancies in the uploaded file names
  valid <- TRUE

  # Compare the character vector of file names from the .zip file to the
  # file names we expect based on the 'HMISmetadata' data in this R package
  missing_from_upload <- setdiff(
    x = HMISmetadata$FileName[HMISmetadata$Required == "Y"],
    y = file_names
  )

  # If at least 1 file was found to be missing from the uploaded .zip file...
  if (length(missing_from_upload) > 0) {

    # ... set the "valid" flag to FALSE
    valid <- FALSE

  }

  # Return the "valid" flag and accompanying message (if applicable)
  out <- list(
    valid = valid,
    missing_file_names = missing_from_upload
  )

  return(out)

}

# Helper function that converts a character vector to an HTML bullet-point list
vec_to_ul <- function(vec) {

  # Create the individual bullet-points
  bullets <- vec |>
    purrr::map(.f = function(x) shiny::tags$li(x))

  # Return the un-ordered list ("ul") containing the individual bullet-points
  shiny::tagList(
    shiny::tags$ul(
      bullets
    )
  )

}
