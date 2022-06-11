

# The following family of functions define the ETL process for converting each
# raw .csv file into an arrow table that is ready to be written to the database


#' Ingest "Client.csv" file and perform ETL prep for "CLIENT" database table
#'
#' @param file String, the full path to the .csv file
#' @param submission_id Integer, the Submission ID associated with this upload
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "CLIENT" database table
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
    # only read in columns needed for "CLIENT" database table
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
    # replace the codes in 'SSNDataQuality' column with their descriptions
    dplyr::mutate(
      SSNDataQuality = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # join the relevant codes from 'DOBDataQualityCodes'
    dplyr::left_join(
      DOBDataQualityCodes,
      by = c("DOBDataQuality" = "Code")
    ) |>
    # replace the codes in 'DOBDataQuality' column with their descriptions
    dplyr::mutate(
      DOBDataQuality = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}



#' Ingest "Client.csv" file and perform ETL prep for "GENDER" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "GENDER" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Client.csv"
#'
#' read_gender(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_gender <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "GENDER" database table
    col_select = c(
      PersonalID,
      Female:GenderNone
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character()
    )
  ) |>
    # pivot gender columns from wide to long
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Gender",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    # keep only "1" (affirmative) status values
    dplyr::filter(Status == 1L)|>
    dplyr::select(-Status) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}



#' Ingest "Client.csv" file and perform ETL prep for "ETHNICITY" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "ETHNICITY" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Client.csv"
#'
#' read_ethnicity(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_ethnicity <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "ETHNICITY" database table
    col_select = c(
      PersonalID,
      AmIndAKNative:Ethnicity
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character()
    )
  ) |>
    # pivot ethnicity columns from wide to long
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Ethnicity",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    # keep only "1" (affirmative) status values
    dplyr::filter(Status == 1L) |>
    dplyr::select(-Status) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}


#' Ingest "Client.csv" file and perform ETL prep for "VETERAN" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "VETERAN" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Client.csv"
#'
#' read_veteran(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_veteran <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "VETERAN" database table
    col_select = c(
      PersonalID,
      VeteranStatus
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      VeteranStatus = readr::col_integer()
    )
  ) |>
    # join the relevant codes from 'GeneralCodes'
    dplyr::left_join(
      GeneralCodes,
      by = c("VeteranStatus" = "Code")
    ) |>
    # replace the codes in 'VeteranStatus' column with their descriptions
    dplyr::mutate(
      VeteranStatus = Description,
      Description = NULL  # drop 'Description' column
    )|>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}


#' Ingest "Disabilities.csv" file and perform ETL prep for "DISABILITIES" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "DISABILITIES" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Disabilities.csv"
#'
#' read_disabilities(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_disabilities <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "DISABILITIES" database table
    col_select = c(
      PersonalID,
     # InformationDate,
      DisabilityResponse,
      IndefiniteAndImpairs
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
     # InformationDate = readr::col_date()
    )
  ) |>
    # keep only "1" (affirmative) DisabilityResponse AND IndefiniteAndImpairs values
    dplyr::filter(DisabilityResponse == 1L & IndefiniteAndImpairs == 1L ) |>
    dplyr::select(-DisabilityResponse) |>
    # join the relevant codes from 'GeneralCodes'
    dplyr::left_join(
      GeneralCodes,
      by = c("IndefiniteAndImpairs" = "Code")
    ) |>
    # keeping last occurrences based on latest dates, using unique function to avoid duplicates
    unique(fromLast=T) |>
    # replace the codes in 'IndefiniteAndImpairs' column with their descriptions
    dplyr::mutate(
      IndefiniteAndImpairs = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}


