

# The following family of functions define the ETL process for converting each
# raw .csv file into an arrow table that is ready to be written to the data lake


#' Ingest "Client.csv" file and perform ETL prep for data lake
#'
#' @param file String, the full path to the .csv file
#' @param submission_id Integer, the Submission ID associated with this upload
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the data lake as a .parquet file
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Client.csv"
#'
#' read_client(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_client <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "CLIENT" data lake table
    col_select = c(
      PersonalID,
      SSN,
      SSNDataQuality,
      DOB,
      DOBDataQuality
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      SSNDataQuality = readr::col_integer(),
      DOB = readr::col_date(),
      DOBDataQuality = readr::col_integer()
    )
  ) |>
    # join the relevant codes from 'SSNDataQualityCodes'
    dplyr::left_join(
      SSNDataQualityCodes,
      by = c("SSNDataQuality" = "Code")
    ) |>
    # Replace the codes in 'SSNDataQuality' column with their descriptions
    dplyr::mutate(
      SSNDataQuality = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # join the relevant codes from 'DOBDataQualityCodes'
    dplyr::left_join(
      DOBDataQualityCodes,
      by = c("DOBDataQuality" = "Code")
    ) |>
    # Replace the codes in 'DOBDataQuality' column with their descriptions
    dplyr::mutate(
      DOBDataQuality = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::select(SubmissionID, dplyr::everything())

  return(data)

}



read_gender <- function(file, submission_id) {

  # Here's some old code that might help you with this ETL

  # data <- readr::read_csv(
  #   file = file,
  # only read in columns needed for "GENDER" data lake table
  #   col_select = c(
  #     PersonalID,
  #     Female:Questioning
  #   ) |>

  # column type specifications go here...

  #   tidyr::pivot_longer(
  #     cols = -PersonalID,
  #     names_to = "Gender",
  #     values_to = "Status",
  #     values_transform = list(Status = as.integer)
  #   ) |>
  #   dplyr::filter(Status == 1L) |>
  #   dplyr::select(-Status)

  # join relevant codes to get plain-english descriptions...

  # add the 'SubmissionID' as the first column in the data

  # return(data)

}



read_ethnicity <- function(file, submission_id) {

  # data <- readr::read_csv(
  #   file = file,
  # only read in columns needed for "GENDER" data lake table
  #   col_select = c(
  #     PersonalID,
  #     AmIndAKNative:Ethnicity
  #   ) |>

  # column type specifications go here...

  # dplyr::rename(HispanicLatino = Ethnicity) |>
  # tidyr::pivot_longer(
  #   cols = -PersonalID,
  #   names_to = "Ethnicity",
  #   values_to = "Status",
  #   values_transform = list(Status = as.integer)
  # ) |>
  #   dplyr::filter(Status == 1L) |>
  #   dplyr::select(-Status)

  # join relevant codes to get plain-english descriptions...

  # add the 'SubmissionID' as the first column in the data

  # return(data)

}



read_veteran <- function(file, submission_id) {

  # data <- readr::read_csv(
  #   file = file,
  # only read in columns needed for "GENDER" data lake table
  #   col_select = c(
  #     PersonalID,
  #     VeteranStatus
  #   ) |>

  # column type specifications go here...

  # join relevant codes to get plain-english descriptions...

  # add the 'SubmissionID' as the first column in the data

  # return(data)

}
