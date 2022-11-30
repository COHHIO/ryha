

# The following family of functions define the ETL process for converting each
# raw .csv file into an arrow table that is ready to be written to the database


#' Look up Plain-English Definitions from Integer Codes
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
#'   file = path
#' )
#'
#' }
read_client <- function(file) {

  client <- readr::read_csv(
    file = file,
    # only read in columns needed for "CLIENT" database table
    col_select = c(
      PersonalID,
      SSN,
      SSNDataQuality,
      DOB,
      DOBDataQuality,
      AmIndAKNative:VeteranStatus,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      SSN = readr::col_character(),
      SSNDataQuality = readr::col_integer(),
      DOB = readr::col_date(),
      DateUpdated = readr::col_datetime()
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
    janitor::clean_names(case = "snake")

  # Hash the SSN values
  ssns_hashed <- c()

  for (s in client$ssn) {

    ssns_hashed <- append(
      ssns_hashed,
      hash(s, key = readRDS(here::here("hkey.RDS")))
    )

  }

  client$ssn <- ssns_hashed

  return(client)

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
read_disabilities <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "DISABILITIES" database table
    col_select = c(
      DisabilitiesID:DisabilityResponse,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      InformationDate = readr::col_date(),
      DisabilityType = readr::col_integer(),
      DisabilityResponse = readr::col_integer(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # join the relevant codes from 'DisabilityTypeCodes'
    dplyr::mutate(
      DisabilityType = lookup_codes(
        var = DisabilityType,
        codes = DisabilityTypeCodes
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
    ) |>
    dplyr::mutate(
      DisabilityResponse = dplyr::if_else(
        DisabilityType == "Substance Use Disorder",
        lookup_codes(var = DisabilityResponse, SubstanceUseDisorderCodes),
        lookup_codes(var = DisabilityResponse, GeneralCodes),
      )
    ) |>
    janitor::clean_names(case = "snake")

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
read_education <- function(file) {

  readr::read_csv(
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
    janitor::clean_names(case = "snake")

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
read_employment <- function(file) {

  readr::read_csv(
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
    janitor::clean_names(case = "snake")

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
read_living <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "LIVING" database table
    col_select = c(
      CurrentLivingSitID:CurrentLivingSituation,
      LeaveSituation14Days,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      InformationDate = readr::col_date(),
      CurrentLivingSituation = readr::col_integer(),
      LeaveSituation14Days = readr::col_integer(),
      DateUpdated = readr::col_datetime()
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
    janitor::clean_names(case = "snake")

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
read_health <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "HEALTH" database table
    col_select = c(
      HealthAndDVID:InformationDate,
      GeneralHealthStatus:PregnancyStatus,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      HealthAndDVID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date(),
      DueDate = readr::col_date(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # replace the codes in 'GeneralHealthStatus:MentalHealthStatus' columns with
    # their descriptions
    dplyr::mutate(
      dplyr::across(
        .cols = GeneralHealthStatus:MentalHealthStatus,
        .fns = function(x) lookup_codes(var = x, codes = HealthStatusCodes)
      ),
      PregnancyStatus = lookup_codes(
        var = PregnancyStatus,
        codes = GeneralCodes
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
    ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::rename(health_and_dv_id = health_and_dvid)

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
read_domestic_violence <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "DOMESTIC_VIOLENCE" database table
    col_select = c(
      HealthAndDVID:CurrentlyFleeing,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      HealthAndDVID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date(),
      DateUpdated = readr::col_datetime()
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
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
    ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::rename(health_and_dv_id = health_and_dvid)

}


#' Ingest "IncomeBenefits.csv" file and perform ETL prep for "INCOME"
#' database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "INCOME" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/IncomeBenefits.csv"
#'
#' read_income(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_income <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "INCOME" database table
    col_select = c(
      IncomeBenefitsID:OtherIncomeSourceIdentify,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      IncomeBenefitsID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date(),
      TotalMonthlyIncome = readr::col_double(),
      EarnedAmount = readr::col_double(),
      UnemploymentAmount = readr::col_double(),
      SSIAmount = readr::col_double(),
      SSDIAmount = readr::col_double(),
      VADisabilityServiceAmount = readr::col_double(),
      VADisabilityNonServiceAmount = readr::col_double(),
      PrivateDisabilityAmount = readr::col_double(),
      WorkersCompAmount = readr::col_double(),
      TANFAmount = readr::col_double(),
      GAAmount = readr::col_double(),
      SocSecRetirementAmount = readr::col_double(),
      PensionAmount = readr::col_double(),
      ChildSupportAmount = readr::col_double(),
      AlimonyAmount = readr::col_double(),
      OtherIncomeAmount = readr::col_double(),
      OtherIncomeSourceIdentify = readr::col_character(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # replace the integer codes with the plain-English description
    dplyr::mutate(
      dplyr::across(
        .cols = !dplyr::ends_with(
          match = c(
            "ID",
            "Date",
            "Amount",
            "Identify",
            "DataCollectionStage",
            "DateUpdated",
            "TotalMonthlyIncome"
          ),
          ignore.case = FALSE
        ),
        .fns = function(x) lookup_codes(var = x, codes = GeneralCodes)
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
    ) |>
    janitor::clean_names(case = "snake")

}


#' Ingest "IncomeBenefits.csv" file and perform ETL prep for "BENEFITS"
#' database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "BENEFITS" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/IncomeBenefits.csv"
#'
#' read_benefits(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_benefits <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "BENEFITS" database table
    col_select = c(
      IncomeBenefitsID:InformationDate,
      BenefitsFromAnySource:OtherInsuranceIdentify,
      DataCollectionStage,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      IncomeBenefitsID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      InformationDate = readr::col_date(),
      OtherBenefitsSourceIdentify = readr::col_character(),
      OtherInsuranceIdentify = readr::col_character(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # replace the integer codes with the plain-English description
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("Reason"),
        .fns = function(x) lookup_codes(var = x, codes = ReasonNotInsuredCodes)
      ),
      dplyr::across(
        .cols = !dplyr::ends_with(
          match = c(
            "ID",
            "Date",
            "Identify",
            "Reason",
            "DataCollectionStage",
            "DateUpdated"
          ),
          ignore.case = FALSE
        ),
        .fns = function(x) lookup_codes(var = x, codes = GeneralCodes)
      ),
      DataCollectionStage = lookup_codes(
        var = DataCollectionStage,
        codes = DataCollectionStageCodes
      )
    ) |>
    janitor::clean_names(case = "snake")

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
read_enrollment <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "ENROLLMENT" database table
    col_select = c(
      EnrollmentID:DisablingCondition,
      MoveInDate,
      ReferralSource,
      RunawayYouth:IncarceratedParent,
      DateUpdated
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
      SexualOrientationOther = readr::col_character(),
      DateUpdated = readr::col_datetime()
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
    ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::rename(orig_project_id = project_id)

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
read_services <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "Services" database table
    col_select = c(
      ServicesID:DateProvided,
      TypeProvided,
      ReferralOutcome,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      DateProvided = readr::col_date(),
      TypeProvided = readr::col_integer(),
      ReferralOutcome = readr::col_integer(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # replace the codes in 'TypeProvided' column with their descriptions
    dplyr::mutate(
      TypeProvided = lookup_codes(
        var = TypeProvided,
        codes = ServiceCodes
      ),
      ReferralOutcome = lookup_codes(
        var = TypeProvided,
        codes = PATHReferralOutcomeCodes
      )
    ) |>
    janitor::clean_names(case = "snake")

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
read_project <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROJECT" database table
    col_select = c(
      ProjectID,
      OrganizationID,
      ProjectName,
      ProjectType,
      OperatingStartDate
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      ProjectType = readr::col_integer(),
      OperatingStartDate = readr::col_date()
    )
  ) |>
    # replace the "ProjectType" codes with the plain-English description
    dplyr::mutate(
      ProjectType = lookup_codes(
        var = ProjectType,
        codes = ProjectTypeCodes
      )
    ) |>
    janitor::clean_names(case = "snake")

}


#' Ingest "Organization.csv" file and perform ETL prep for "PROJECT" database table
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
#' path <- "path/to/Organization.csv"
#'
#' read_organization(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_organization <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROJECT" database table
    col_select = c(
      OrganizationID,
      OrganizationName
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character()
    )
  ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::rename(orig_organization_id = organization_id)

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
read_exit <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROGRAM" database table
    col_select = c(
      ExitID,
      EnrollmentID,
      PersonalID,
      ExitDate:OtherDestination,
      ProjectCompletionStatus,
      ExchangeForSex:PosCommunityConnections,
      DateUpdated
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      ExitID = readr::col_character(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      ExitDate = readr::col_date(),
      OtherDestination = readr::col_character(),
      DateUpdated = readr::col_datetime()
    )
  ) |>
    # replace the integer codes with the plain-English description
    dplyr::mutate(
      Destination = lookup_codes(
        var = Destination,
        codes = LivingCodes
      ),
      ProjectCompletionStatus = lookup_codes(
        var = ProjectCompletionStatus,
        codes = ProjectCompletionStatusCodes
      ),
      dplyr::across(
        .cols = c(
          ExchangeForSex,
          ExchangeForSexPastThreeMonths,
          AskedOrForcedToExchangeForSex:GroupCounseling,
          PostExitCounselingPlan,
          DestinationSafeClient
        ),
        .fns = function(x) lookup_codes(var = x, codes = GeneralCodes)
      ),
      CountOfExchangeForSex = lookup_codes(
        var = CountOfExchangeForSex,
        codes = CountExchangeForSexCodes
      ),
      dplyr::across(
        .cols = DestinationSafeWorker:PosCommunityConnections,
        .fns = function(x) lookup_codes(var = x, codes = WorkerResponseCodes)
      )
    ) |>
    janitor::clean_names(case = "snake")

}


#' Ingest "Export.csv" file and perform ETL prep for "PROJECT" and "SUBMISSION"
#' database tables
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "PROJECT" and "SUBMISSION" database tables
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Export.csv"
#'
#' read_export(
#'   file = path,
#'   submission_id = 1L
#' )
#'
#' }
read_export <- function(file) {

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROGRAM" database table
    col_select = c(
      ExportID,
      SourceContactFirst,
      SourceContactLast,
      SourceContactEmail,
      ExportStartDate:SoftwareName
    ),
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      ExportStartDate = readr::col_date(),
      ExportEndDate = readr::col_date()
    )
  ) |>
    janitor::clean_names(case = "snake")

}


#' Hash a value with a key
#'
#' @param x A character string
#' @param key A hashing dictionary key
#'
#' @return An hashed string
#'
#' @examples
#' \dontrun{
#'
#' hash("blah", key = my_key)
#'
#' }
hash <- function(x, key) {

  stringr::str_replace_all(x, "-", "")

  if (!is.na(x) & nchar(x) == 9) {

    x_sep <- strsplit(x, split = "")[[1]]

    out <- c()

    for (s in x_sep) {

      out <- append(out, key[[s]])

    }

    out |> paste(collapse = "")

  } else {

    NA

  }

}
