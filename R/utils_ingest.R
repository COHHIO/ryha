

# The following family of functions define the ETL process for converting each
# raw .csv file into an arrow table that is ready to be written to the database


#' Lookup Plain-English Definitions from Integer Codes
#'
#' @description This is a helper function that gets used in most downstream
#'   `read_*()` functions to replace integer codes with their associated
#'   plain-english definitions (character strings) *in place*
#'
#' @param var A tidy-select specification of a column variable
#' @param codes An R dataset
#'
#' @return A vector of character strings, representing the plain-english
#'   definitions based upon the integer values found in the input `var` column
#'
#' @noRd
lookup_codes <- function(var, codes) {

  codes$Description[match(x = {{ var }}, table = codes$Code)]

}


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
      SSNDataQuality = lookup_codes(
        var = SSNDataQuality,
        codes = SSNDataQualityCodes
      ),
      DOBDataQuality = lookup_codes(
        var = DOBDataQuality,
        codes = DOBDataQualityCodes
      )
    ) |>
    # rename the "Ethnicity" column to "HispanicLatinaox"
    dplyr::rename(HispanicLatinaox = Ethnicity) |>
    # replace the "AmIndAKNative:VeteranStatus" codes with the plain-English
    # description
    dplyr::mutate(
      dplyr::across(
        .cols = AmIndAKNative:VeteranStatus,
        .fns = function(x) lookup_codes(var = x, codes = GeneralCodes)
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
    # join the relevant codes from 'DisabilityTypeCodes'
    dplyr::mutate(
      DisabilityType = lookup_codes(
        var = DisabilityType,
        codes = DisabilityTypeCodes
      )
    ) |>
    dplyr::mutate(
      DisabilityResponse = dplyr::if_else(
        DisabilityType == "Substance Use Disorder",
        lookup_codes(var = DisabilityResponse, SubstanceUseDisorderCodes),
        lookup_codes(var = DisabilityResponse, GeneralCodes),
      )
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
      EmploymentEducationID:SchoolStatus,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      InformationDate = readr::col_date(),
      LastGradeCompleted = readr::col_integer(),
      SchoolStatus = readr::col_integer(),
      DataCollectionStage = readr::col_integer(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # re-code the integer codes in each attribute column with the related
    # description
    dplyr::mutate(
      LastGradeCompleted = lookup_codes(
        var = LastGradeCompleted,
        codes = LastGradeCompletedCodes
      ),
      SchoolStatus = lookup_codes(
        var = SchoolStatus,
        codes = SchoolStatusCodes
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
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
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # re-code the integer codes in each attribute column with the related
    # description
    dplyr::mutate(
      Employed = lookup_codes(
        var = Employed,
        codes = GeneralCodes
      ),
      EmploymentType = lookup_codes(
        var = EmploymentType,
        codes = EmploymentTypeCodes
      ),
      NotEmployedReason = lookup_codes(
        var = NotEmployedReason,
        codes = NotEmployedReasonCodes
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "CurrentLivingSituation.csv" file and perform ETL prep for "LIVING"
#' database table
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
    # join the relevant codes from 'CurrentLivingSituationCodes' dataset
    dplyr::mutate(
      CurrentLivingSituation = lookup_codes(
        var = CurrentLivingSituation,
        codes = LivingCodes
      ),
      LeaveSituation14Days = lookup_codes(
        var = LeaveSituation14Days,
        codes = GeneralCodes
      )
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}



#' Ingest "HealthAndDV.csv" file and perform ETL prep for "HEALTH" database
#' table
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
      GeneralHealthStatus:MentalHealthStatus
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
    # replace the codes in 'GeneralHealthStatus:MentalHealthStatus' columns with
    # their descriptions
    dplyr::mutate(
      dplyr::across(
        .cols = GeneralHealthStatus:MentalHealthStatus,
        .fns = function(x) lookup_codes(var = x, codes = HealthStatusCodes)
      )
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "HealthAndDV.csv" file and perform ETL prep for "DOMESTIC_VIOLENCE"
#' database table
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
#' read_domestic_violence(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_domestic_violence <- function(file, submission_id) {

  data <- readr::read_csv(
    file = file,
    # only read in columns needed for "DOMESTIC_VIOLENCE" database table
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
    # join the descriptions from the lookup codes
    dplyr::mutate(
      DomesticViolenceVictim = lookup_codes(
        var = DomesticViolenceVictim,
        codes = GeneralCodes
      ),
      WhenOccurred = lookup_codes(
        var = WhenOccurred,
        codes = WhenDVOccurredCodes
      ),
      CurrentlyFleeing = lookup_codes(
        var = CurrentlyFleeing,
        codes = GeneralCodes
      )
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

  return(data)

}

#' Ingest "Enrollment.csv" file and perform ETL prep for "ENROLLMENT" database
#' table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "ENROLLMENT" database table
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
    # only read in columns needed for "ENROLLMENT" database table
    col_select = c(
      EnrollmentID:DisablingCondition,
      MoveInDate,
      ReferralSource,
      RunawayYouth:IncarceratedParent
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      ProjectID = readr::col_character(),
      EntryDate = readr::col_date(),
      HouseholdID = readr::col_character(),
      DateToStreetESSH = readr::col_date(),
      MoveInDate = readr::col_date(),
      SexualOrientationOther = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      RelationshipToHoH = lookup_codes(
        var = RelationshipToHoH,
        codes = RelationshipToHoHCodes
      ),
      LivingSituation = lookup_codes(
        var = LivingSituation,
        codes = LivingCodes
      ),
      LengthOfStay = lookup_codes(
        var = LengthOfStay,
        codes = LengthOfStayCodes
      ),
      TimesHomelessPastThreeYears = lookup_codes(
        var = TimesHomelessPastThreeYears,
        codes = TimesHomelessPastThreeYearsCodes
      ),
      MonthsHomelessPastThreeYears = lookup_codes(
        var = MonthsHomelessPastThreeYears,
        codes = MonthsHomelessPastThreeYearsCodes
      ),
      ReferralSource = lookup_codes(
        var = ReferralSource,
        codes = ReferralSourceCodes
      ),
      SexualOrientation = lookup_codes(
        var = SexualOrientation,
        codes = SexualOrientationCodes
      ),
      dplyr::across(
        .cols = c(ChildWelfareYears, JuvenileJusticeYears),
        .fns = function(x) lookup_codes(var = x, codes = RHYNumberOfYearsCodes)
      ),
      dplyr::across(
        .cols = c(
          LOSUnderThreshold:PreviousStreetESSH,
          DisablingCondition,
          RunawayYouth,
          FormerWardChildWelfare,
          FormerWardJuvenileJustice,
          UnemploymentFam:IncarceratedParent
        ),
        .fns = function(x) lookup_codes(var = x, codes = GeneralCodes)
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

#' Ingest "Project.csv" file and perform ETL prep for "PROJECT" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "PROJECT" database table
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
#'   submission_id = 1L
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



#' Ingest "Exit.csv" file and perform ETL prep for "EXIT" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "EXIT" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Exit.csv"
#'
#' read_exit(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_exit <- function(file, submission_id) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROGRAM" database table
    col_select = c(
      ExitID,
      EnrollmentID,
      PersonalID,
      ExitDate:OtherDestination,
      ProjectCompletionStatus,
      ExchangeForSex:PosCommunityConnections
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      ExitID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      ExitDate = readr::col_date()
    )
  ) |>
    # replace the "ProjectType" codes with the plain-English description
    dplyr::mutate(
     Destination = lookup_codes(
       var = Destination,
       codes = LivingCodes
     )
    ) |>
    # add the 'SubmissionID' as the first column in the data
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::relocate(SubmissionID, .before = dplyr::everything())

}

