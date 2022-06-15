

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
      InformationDate:DisabilityResponse
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date()
    )
  ) |>
    # keep only "1" (affirmative) DisabilityResponse
    dplyr::filter(DisabilityResponse == 1L) |>
    dplyr::select(-DisabilityResponse) |>
    # join the relevant codes from 'DisabilityCodes'
    dplyr::left_join(
      DisabilityTypeCodes,
      by = c("DisabilityType" = "Code")
    ) |>
    # replace the codes in 'DisabilityType' column with their descriptions
    dplyr::mutate(
      DisabilityType = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}



#' Ingest "EmploymentEducation.csv" file and perform ETL prep for "EDUCATION" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "EDUCATION" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/EmploymentEducation.csv"
#'
#' read_education(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_education <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "EDUCATION" database table
    col_select = c(
      PersonalID,
      EmploymentEducationID,
      InformationDate:SchoolStatus,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      EmploymentEducationID = readr::col_character(),
      InformationDate = readr::col_date(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # join the relevant codes from 'LastGradeCompletedCodes'
    dplyr::left_join(
      LastGradeCompletedCodes,
      by = c("LastGradeCompleted" = "Code")
    ) |>
    # replace the codes in 'LastGradeCompleted' column with their descriptions
    dplyr::mutate(
      LastGradeCompleted = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # join the relevant codes from 'SchoolStatusCodes'
    dplyr::left_join(
      SchoolStatusCodes,
      by = c("SchoolStatus" = "Code")
    ) |>
    # replace the codes in 'SchoolStatus' column with their descriptions
    dplyr::mutate(
      SchoolStatus = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    dplyr::left_join(
      DataCollectionStageCodes,
      by = c("DataCollectionStage" = "Code")
    ) |>
    # replace the codes in 'DataCollectionStage' column with their descriptions
    dplyr::mutate(
      DataCollectionStage = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # Mutate datetime into just date in 'DateUpdate' column , to match 'infromationDate' column
    dplyr::mutate(
      DateUpdatedDate = as.POSIXct(DateUpdated, format = "%Y-%d-%m %H:%M:%S"),
      #change the class format of 'DateUpdated' column to date class
      DateUpdated = as.Date(format(DateUpdatedDate, "%Y-%d-%m")),
      DateUpdatedDate = NULL   # drop 'DateUpdatedDate' column
    )
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}


#' Ingest "EmploymentEducation.csv" file and perform ETL prep for "EMPLOYMENT" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "EMPLOYMENT" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/EmploymentEducation.csv"
#'
#' read_employment(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_employment <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "EMPLOYMENT" database table
    col_select = c(
      PersonalID,
      EmploymentEducationID,
      InformationDate,
      Employed:NotEmployedReason,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      EmploymentEducationID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date(),
      DateUpdated = readr::col_datetime() # this has to be called with character because of the date and time updated structure. hence can't be dealt as date but as charachter
    )
  ) |>
    # join the relevant codes from 'EducationCodes'
    dplyr::left_join(
      Employment_Yes_Status,
      by = c("EmploymentType" = "Code")
    ) |>
    # replace the codes in 'EmploymentType' column with their descriptions
    dplyr::mutate(
      EmploymentType = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # join the relevant codes from 'EducationCodes'
    dplyr::left_join(
      Employment_NO_Status,
      by = c("NotEmployedReason" = "Code")
    ) |>
    # replace the codes in 'NotEmployedReason' column with their descriptions
    dplyr::mutate(
      NotEmployedReason = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    #unite EmploymentType and NotEmployedReason to an Employment Column
    tidyr::unite("Employment", EmploymentType,NotEmployedReason, na.rm = TRUE) |>
    # Only keep "1" (affirmative) and "0" (non-formative) status values of employed
    dplyr::filter(Employed == 1L | Employed == 0L) |>
    dplyr::select(-Employed) |>
    dplyr::left_join(
      DataCollectionStageCodes,
      by = c("DataCollectionStage" = "Code")
    ) |>
      # replace the codes in 'DataCollectionStage' column with their descriptions
    dplyr::mutate(
      DataCollectionStage = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # Mutate datetime into just date in 'DateUpdate' column , to match 'infromationDate' column
    dplyr::mutate(
      DateUpdatedDate = as.POSIXct(DateUpdated, format = "%Y-%d-%m %H:%M:%S"),
      #change the class format of 'DateUpdated' column to date class
      DateUpdated = as.Date(format(DateUpdatedDate, "%Y-%d-%m")),
      DateUpdatedDate = NULL   # drop 'DateUpdatedDate' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "Enrollment.csv" file and perform ETL prep for "HOUSEHOLD" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "HOUSEHOLD" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Enrollment.csv"
#'
#' read_enrollment(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_enrollment <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "HOUSEHOLD" database table
    col_select = c(
      HouseholdID,
      PersonalID,
      EntryDate,
      RelationshipToHoH:DateToStreetESSH
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      HouseholdID = readr::col_character(),
      PersonalID = readr::col_character(),
      EntryDate = readr::col_date(),
      DateToStreetESSH = readr::col_date()
    )
  )

  return(data)

}

#' Ingest "Project.csv" file and perform ETL prep for "PROGRAM" database table
#'
#' @param file String, the full path to the .csv file
#' @param ProgramID Integer, the Submission ID associated with this upload
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "PROGRAM" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Project.csv"
#'
#' read_program(
#'   file = path,
#'   program_id = 1L
#' )
#'
#' }
read_program <- function(file, program_id) {

  data <- readr::read_csv(
    file = "C:/Users/yaniv/Desktop/KA/hudx-111_YWCA/Project.csv",
    # only read in columns needed for "PROGRAM" database table
    col_select = c(
      ProjectID,
      ProjectName
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
  return(data)

}
