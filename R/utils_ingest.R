

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
      DOBDataQuality,
      AmIndAKNative:VeteranStatus
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      SSN = readr::col_character(),
      SSNDataQuality = readr::col_integer(),
      DOB = readr::col_date()
    )
  ) |>
    # replace the "SSN/DOBDataQuality" codes with the plain-English description
    dplyr::mutate(
      SSNDataQuality = SSNDataQualityCodes$Description[match(x = SSNDataQuality, table = SSNDataQualityCodes$Code)],
      DOBDataQuality = DOBDataQualityCodes$Description[match(x = DOBDataQuality, table = DOBDataQualityCodes$Code)]
    ) |>
    # rename the "Ethnicity" column to "HispanicLatinaox"
    dplyr::rename(HispanicLatinaox = Ethnicity) |>
    # replace the "AmIndAKNative:VeteranStatus" codes with the plain-English
    # description
    dplyr::mutate(
      dplyr::across(
        .cols = AmIndAKNative:VeteranStatus,
        .fns = function(x) GeneralCodes$Description[match(x = x, table = GeneralCodes$Code)]
      )
    ) |>
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
      DisabilitiesID:DisabilityResponse
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      InformationDate = readr::col_date(),
      DisabilityType = readr::col_integer(),
      DisabilityResponse = readr::col_integer()
    )
  ) |>
    # create and bind a new column called 'SubstanceAbuse'
     cbind(SubstanceAbuse = NA) |>
    #filter data based on DisabilityType and values of DisabilityResponse other than zero
    dplyr::filter(DisabilityType == 10L | DisabilityResponse != 0L) |>
    # join the relevant codes from 'DisabilityTypeCodes'
    dplyr::left_join(
      DisabilityTypeCodes,
      by = c("DisabilityType" = "Code")
    ) |>
    # replace and mutate SubstanceAbuse based on DisabilityResponse 5:9 and 10 and change column DisabilityType with Description
    dplyr::mutate( SubstanceAbuse = replace( DisabilityResponse, DisabilityType != 10, NA),
                   DisabilityResponse = replace( DisabilityResponse, DisabilityType == 10, NA),
                   DisabilityType = Description,
                   Description = NULL
    ) |>
    # join the relevant codes from 'SubstanceUseDisorderCodes'
    dplyr::left_join(
      SubstanceUseDisorderCodes,
      by = c("SubstanceAbuse" = "Code")
    ) |>
    # replace the codes in 'SubstanceAbuse' column with their descriptions
    dplyr::mutate(
      SubstanceAbuse = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # join the relevant codes from 'GeneralCodes'
    dplyr::left_join(
      GeneralCodes,
      by = c("DisabilityResponse" = "Code")
    ) |>
    # replace the codes in 'DisabilityResponse' column with their descriptions
    dplyr::mutate(
      DisabilityResponse = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    #unite DisabilityResponse and SubstanceAbuse columns
    tidyr::unite("DisabilityResponse", DisabilityResponse,SubstanceAbuse, na.rm = TRUE) |>
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
      EmploymentEducationID:SchoolStatus,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      LastGradeCompleted = readr::col_integer(),
      DataCollectionStage = readr::col_integer(),
      SchoolStatus = readr::col_integer(),
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
    ) |>
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
      EmploymentEducationID:InformationDate,
      Employed:DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      Employed = readr::col_integer(),
      EmploymentType = readr::col_integer(),
      NotEmployedReason = readr::col_integer(),
      DataCollectionStage = readr::col_integer(),
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

#' Ingest "CurrentLivingSituation.csv" file and perform ETL prep for "LIVING" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "LIVING" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/CurrentLivingSituation.csv"
#'
#' read_living(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_living <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "LIVING" database table
    col_select = c(
      CurrentLivingSitID:CurrentLivingSituation,
      LeaveSituation14Days
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      InformationDate = readr::col_date(),
      CurrentLivingSituation = readr::col_integer(),
      LeaveSituation14Days = readr::col_integer()
    )
  ) |>
    # join the relevant codes from 'Appendix_A_Codes' for CurrentLivingSituation
    dplyr::left_join(
      LSOL_SituationsCodes,
      by = c("CurrentLivingSituation" = "Code")
    ) |>
    # replace the codes in 'CurrentLivingSituation' column with their descriptions
    dplyr::mutate(
      CurrentLivingSituation = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # join the relevant codes from 'Appendix_A_Codes' for LeaveSituation14Days
    dplyr::left_join(
      LSOL_SituationsCodes,
      by = c("LeaveSituation14Days" = "Code")
    ) |>
    # replace the codes in 'LeaveSituation14Days' column with their descriptions
    dplyr::mutate(
      LeaveSituation14Days = Description,
      Description = NULL   # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "HealthAndDV.csv" file and perform ETL prep for "HEALTH" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "HEALTH" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/HealthAndDV.csv"
#'
#' read_health(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_health <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "HEALTH" database table
    col_select = c(
      HealthAndDVID:InformationDate,
      GeneralHealthStatus:DueDate
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      HealthAndDVID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date(),
      DueDate = readr::col_date()
    )
  ) |>
    # replace the codes in 'PregnancyStatus' column with their DueDates If pregnant
    dplyr::mutate(
      PregnancyStatus = DueDate,
      DueDate = NULL  # drop 'Description' column
    ) |>
    # join the relevant codes from 'HealthStatusCodes'
    dplyr::left_join(
      HealthStatusCodes,
      by = c("MentalHealthStatus" = "Code")
    ) |>
    # replace the codes in 'MentalHealthStatus' column with their Descriptions
    dplyr::mutate(
      MentalHealthStatus = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # join the relevant codes from 'HealthStatusCodes'
    dplyr::left_join(
      HealthStatusCodes,
      by = c("DentalHealthStatus" = "Code")
    ) |>
    # replace the codes in 'DentalHealthStatus' column with their Descriptions
    dplyr::mutate(
      DentalHealthStatus = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # join the relevant codes from 'HealthStatusCodes'
    dplyr::left_join(
      HealthStatusCodes,
      by = c("GeneralHealthStatus" = "Code")
    ) |>
    # replace the codes in 'GeneralHealthStatus' column with their Descriptions
    dplyr::mutate(
      GeneralHealthStatus = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "HealthAndDV.csv" file and perform ETL prep for "DOMESTIC_VIOLENCE" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "DOMESTIC_VIOLENCE" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/HealthAndDV.csv"
#'
#' read_domesticViolence(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_domesticViolence <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "HEALTH" database table
    col_select = c(
      HealthAndDVID:CurrentlyFleeing
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      HealthAndDVID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date()
    )
  ) |>
    # keep only "1" (affirmative) DomesticViolenceVictim
    dplyr::filter(DomesticViolenceVictim == 1L) |>
    dplyr::select(-DomesticViolenceVictim) |>
    #if yes for domestic violence victim/survivor
    # join the relevant codes from 'DVStatusCodes'
    dplyr::left_join(
      DVStatusCodes,
      by = c("WhenOccurred" = "Code")
    ) |>
    # replace the codes in 'WhenOccurred' column with their Descriptions
    dplyr::mutate(
      WhenOccurred = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # join the relevant codes from 'GeneralCodes'
    dplyr::left_join(
      GeneralCodes,
      by = c("CurrentlyFleeing" = "Code")
    ) |>
    # replace the codes in 'CurrentlyFleeing' column with their Descriptions
    dplyr::mutate(
      CurrentlyFleeing = Description,
      Description = NULL  # drop 'Description' column
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


#' Ingest "Services.csv" file and perform ETL prep for "SERVICES" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "SERVICES" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Services.csv"
#'
#' read_services(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_services <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "Services" database table
    col_select = c(
      ServicesID:DateProvided,
      TypeProvided,
      ReferralOutcome
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      DateProvided = readr::col_date(),
      TypeProvided = readr::col_integer(),
      ReferralOutcome = readr::col_integer()
    )
  ) |>
    # join the relevant codes from 'RecordTypeRHYServicesCodes' Used in Services.csv when RecordType = 142 (RHY service).
    dplyr::left_join(
      RecordTypeRHYServicesCodes,
      by = c("TypeProvided" = "Code")
    ) |>
    # replace the codes in 'TypeProvided' column with their descriptions
    dplyr::mutate(
      TypeProvided = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    #uncertain about the index object data sets, hence unsure about this part of the code.
    # not sure to join the relevant codes from 'ReferralsServiceCodes' or from 'PathReferralOutcomeCodes'
    #not sure if I should have this section period since we deal with type provided 142 and not 161?
    dplyr::left_join(
      # ?? # ReferralsOutcomeCodes,
      PathReferralOutcomeCodes,
      by = c("ReferralOutcome" = "Code")
    ) |>
    # replace the codes in 'ReferralOutcome' column with their descriptions
    dplyr::mutate(
      ReferralOutcome = Description,
      Description = NULL  # drop 'Description' column
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "Project.csv" file and perform ETL prep for "PROGRAM" database table
#'
#' @param file String, the full path to the .csv file
#' @param ProjectID Integer, the Project ID associated with pulling Project information
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "PROGRAM" database table. "PROGRAM" database table shows ProjectName and ProjectType based on ProjectID
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Project.csv"
#'
#' read_project(
#'   file = path,
#'   program_id = 1L
#' )
#'
#' }
read_project <- function(file, submission_id) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROGRAM" database table
    col_select = c(
      ProjectID,
      OrganizationID,
      ProjectName,
      ProjectType
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      ProjectType = readr::col_integer()
    )
  ) |>
    # replace the "ProjectType" codes with the plain-English description
    dplyr::mutate(
      ProjectType = ProjectTypeCodes$Description[match(x = ProjectType, table = ProjectTypeCodes$Code)]
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

}




