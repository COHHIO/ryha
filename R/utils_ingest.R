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

  expected_colnames <- c(
    "PersonalID",
    "SSN",
    "SSNDataQuality",
    "DOB",
    "DOBDataQuality",
    "AmIndAKNative",
    "Asian",
    "BlackAfAmerican",
    "HispanicLatinaeo",
    "MidEastNAfrican",
    "NativeHIPacific",
    "White",
    "RaceNone",
    "Woman",
    "Man",
    "NonBinary",
    "CulturallySpecific",
    "Transgender",
    "Questioning",
    "DifferentIdentity",
    "GenderNone",
    "DifferentIdentityText",
    "VeteranStatus",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  client <- readr::read_csv(
    file = file,
    # only read in columns needed for "CLIENT" database table
    col_select = expected_colnames,
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
    # replace codes with the plain-English description
    dplyr::mutate(
      dplyr::across(
        .cols = c(
          AmIndAKNative,
          Asian,
          BlackAfAmerican,
          HispanicLatinaeo,
          MidEastNAfrican,
          NativeHIPacific,
          White,
          Woman,
          Man,
          NonBinary,
          CulturallySpecific,
          Transgender,
          Questioning,
          DifferentIdentity
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesCodes)
      )
    ) |>
    # replace codes with the plain-English description
    dplyr::mutate(
      dplyr::across(
        .cols = c(
          RaceNone,
          GenderNone
        ),
        .fns = function(x) lookup_codes(var = x, codes = RaceGenderNoneCodes)
      )
    ) |>
    # replace codes with the plain-English description
    dplyr::mutate(
      dplyr::across(
        .cols = c(
          VeteranStatus
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesReasonsForMissingDataCodes)
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
#'   file = path
#' )
#'
#' }
read_disabilities <- function(file) {

  expected_colnames <- c(
    "DisabilitiesID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "DisabilityType",
    "DisabilityResponse",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "DISABILITIES" database table
    col_select = expected_colnames,
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
        lookup_codes(var = DisabilityResponse, NoYesReasonsForMissingDataCodes),
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
#'   file = path
#' )
#'
#' }
read_education <- function(file) {

  expected_colnames <- c(
    "EmploymentEducationID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "LastGradeCompleted",
    "SchoolStatus",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "EDUCATION" database table
    col_select = expected_colnames,
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
#'   file = path
#' )
#'
#' }
read_employment <- function(file) {

  expected_colnames <- c(
    "EmploymentEducationID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "Employed",
    "EmploymentType",
    "NotEmployedReason",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "EMPLOYMENT" database table
    col_select = expected_colnames,
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
        codes = NoYesReasonsForMissingDataCodes
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
#'   file = path
#' )
#'
#' }
read_living <- function(file) {

  expected_colnames <- c(
    "CurrentLivingSitID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "CurrentLivingSituation",
    "LeaveSituation14Days",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "LIVING" database table
    col_select = expected_colnames,
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
        codes = NoYesReasonsForMissingDataCodes
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
#'   file = path
#' )
#'
#' }
read_health <- function(file) {

  expected_colnames <- c(
    "HealthAndDVID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "GeneralHealthStatus",
    "DentalHealthStatus",
    "MentalHealthStatus",
    "PregnancyStatus",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "HEALTH" database table
    col_select = expected_colnames,
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
        .cols = c(
          GeneralHealthStatus,
          DentalHealthStatus,
          MentalHealthStatus
        ),
        .fns = function(x) lookup_codes(var = x, codes = HealthStatusCodes)
      ),
      PregnancyStatus = lookup_codes(
        var = PregnancyStatus,
        codes = NoYesReasonsForMissingDataCodes
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
#'   file = path
#' )
#'
#' }
read_domestic_violence <- function(file) {

  expected_colnames <- c(
    "HealthAndDVID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "DomesticViolenceSurvivor",
    "WhenOccurred",
    "CurrentlyFleeing",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "DOMESTIC_VIOLENCE" database table
    col_select = expected_colnames,
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
      DomesticViolenceSurvivor = lookup_codes(
        var = DomesticViolenceSurvivor,
        codes = NoYesReasonsForMissingDataCodes
      ),
      WhenOccurred = lookup_codes(
        var = WhenOccurred,
        codes = WhenDVOccurredCodes
      ),
      CurrentlyFleeing = lookup_codes(
        var = CurrentlyFleeing,
        codes = NoYesReasonsForMissingDataCodes
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
#'   file = path
#' )
#'
#' }
read_income <- function(file) {

  expected_colnames <- c(
    "IncomeBenefitsID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "IncomeFromAnySource",
    "TotalMonthlyIncome",
    "Earned",
    "EarnedAmount",
    "Unemployment",
    "UnemploymentAmount",
    "SSI",
    "SSIAmount",
    "SSDI",
    "SSDIAmount",
    "VADisabilityService",
    "VADisabilityServiceAmount",
    "VADisabilityNonService",
    "VADisabilityNonServiceAmount",
    "PrivateDisability",
    "PrivateDisabilityAmount",
    "WorkersComp",
    "WorkersCompAmount",
    "TANF",
    "TANFAmount",
    "GA",
    "GAAmount",
    "SocSecRetirement",
    "SocSecRetirementAmount",
    "Pension",
    "PensionAmount",
    "ChildSupport",
    "ChildSupportAmount",
    "Alimony",
    "AlimonyAmount",
    "OtherIncomeSource",
    "OtherIncomeAmount",
    "OtherIncomeSourceIdentify",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "INCOME" database table
    col_select = expected_colnames,
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
      IncomeFromAnySource = lookup_codes(
        var = IncomeFromAnySource,
        codes = NoYesReasonsForMissingDataCodes
      ),
      dplyr::across(
        .cols = !dplyr::ends_with(
          match = c(
            "ID",
            "Date",
            "Amount",
            "Identify",
            "DataCollectionStage",
            "DateUpdated",
            "IncomeFromAnySource",
            "TotalMonthlyIncome"
          ),
          ignore.case = FALSE
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesMissingCodes)
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
#'   file = path
#' )
#'
#' }
read_benefits <- function(file) {

  expected_colnames <- c(
    "IncomeBenefitsID",
    "EnrollmentID",
    "PersonalID",
    "InformationDate",
    "BenefitsFromAnySource",
    "SNAP",
    "WIC",
    "TANFChildCare",
    "TANFTransportation",
    "OtherTANF",
    "OtherBenefitsSource",
    "OtherBenefitsSourceIdentify",
    "InsuranceFromAnySource",
    "Medicaid",
    "NoMedicaidReason",
    "Medicare",
    "NoMedicareReason",
    "SCHIP",
    "NoSCHIPReason",
    "VHAServices",
    "NoVHAReason",
    "EmployerProvided",
    "NoEmployerProvidedReason",
    "COBRA",
    "NoCOBRAReason",
    "PrivatePay",
    "NoPrivatePayReason",
    "StateHealthIns",
    "NoStateHealthInsReason",
    "IndianHealthServices",
    "NoIndianHealthServicesReason",
    "OtherInsurance",
    "OtherInsuranceIdentify",
    "DataCollectionStage",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "BENEFITS" database table
    col_select = expected_colnames,
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
        .cols = dplyr::ends_with("AnySource"),
        .fns = function(x) lookup_codes(var = x, codes = NoYesReasonsForMissingDataCodes)
      ),
      dplyr::across(
        .cols = dplyr::ends_with("Reason"),
        .fns = function(x) lookup_codes(var = x, codes = ReasonNotInsuredCodes)
      ),
      dplyr::across(
        .cols = !dplyr::ends_with(
          match = c(
            "ID",
            "Date",
            "AnySource",
            "Identify",
            "Reason",
            "DataCollectionStage",
            "DateUpdated"
          ),
          ignore.case = FALSE
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesMissingCodes)
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
#'   file = path
#' )
#'
#' }
read_enrollment <- function(file) {

  expected_colnames <- c(
    "EnrollmentID",
    "PersonalID",
    "ProjectID",
    "EntryDate",
    "HouseholdID",
    "RelationshipToHoH",
    "EnrollmentCoC",
    "LivingSituation",
    "LengthOfStay",
    "LOSUnderThreshold",
    "PreviousStreetESSH",
    "DateToStreetESSH",
    "TimesHomelessPastThreeYears",
    "MonthsHomelessPastThreeYears",
    "DisablingCondition",
    "MoveInDate",
    "ReferralSource",
    "RunawayYouth",
    "SexualOrientation",
    "SexualOrientationOther",
    "FormerWardChildWelfare",
    "ChildWelfareYears",
    "ChildWelfareMonths",
    "FormerWardJuvenileJustice",
    "JuvenileJusticeYears",
    "JuvenileJusticeMonths",
    "UnemploymentFam",
    "MentalHealthDisorderFam",
    "PhysicalDisabilityFam",
    "AlcoholDrugUseDisorderFam",
    "InsufficientIncome",
    "IncarceratedParent",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "ENROLLMENT" database table
    col_select = expected_colnames,
    # define schema types
    col_types = readr::cols(
      .default = readr::col_integer(),
      EnrollmentID = readr::col_character(),
      PersonalID = readr::col_character(),
      ProjectID = readr::col_character(),
      EntryDate = readr::col_date(),
      HouseholdID = readr::col_character(),
      EnrollmentCoC = readr::col_character(),
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
        .cols = c(
          ChildWelfareYears,
          JuvenileJusticeYears
        ),
        .fns = function(x) lookup_codes(var = x, codes = RHYNumberOfYearsCodes)
      ),
      dplyr::across(
        .cols = c(
          LOSUnderThreshold,
          PreviousStreetESSH,
          UnemploymentFam,
          MentalHealthDisorderFam,
          PhysicalDisabilityFam,
          AlcoholDrugUseDisorderFam,
          InsufficientIncome,
          IncarceratedParent
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesMissingCodes)
      ),
      dplyr::across(
        .cols = c(
          DisablingCondition,
          RunawayYouth,
          FormerWardChildWelfare,
          FormerWardJuvenileJustice
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesReasonsForMissingDataCodes)
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
#'   file = path
#' )
#'
#' }
read_services <- function(file) {

  expected_colnames <- c(
    "ServicesID",
    "EnrollmentID",
    "PersonalID",
    "DateProvided",
    "TypeProvided",
    "ReferralOutcome",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "Services" database table
    col_select = expected_colnames,
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
        var = ReferralOutcome,
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
#'   file = path
#' )
#'
#' }
read_project <- function(file) {

  expected_colnames <- c(
    "ProjectID",
    "OrganizationID",
    "ProjectName",
    "ProjectType",
    "OperatingStartDate"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROJECT" database table
    col_select = expected_colnames,
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

#' Ingest "ProjectCoC.csv" file and perform ETL prep for "PROJECT COC" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "PROJECT COC" database table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/ProjectCoC.csv"
#'
#' read_project_coc(
#'   file = path
#' )
#' }
read_project_coc <- function(file) {
  expected_colnames <- c(
    "ProjectCoCID",
    "ProjectID",
    "CoCCode",
    "Geocode",
    "Address1",
    "Address2",
    "City",
    "State",
    "ZIP",
    "GeographyType",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROJECT COC" database table
    col_select = expected_colnames,
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      DateUpdated = readr::col_date()
    )
  ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::rename(
      # improve default column names
      "project_coc_id" = "project_co_cid",
      "coc_code" = "co_c_code",
      # change project_id to orig_project_id (as project_id has a different meaning in the app)
      "orig_project_id" = "project_id"
    ) |>
    # Add county column
    dplyr::left_join(CountyCodes, by = "geocode") |>
    # Handle missing county codes
    tidyr::replace_na(list(county = "Missing")) |> 
    # Place county column after geocode
    dplyr::relocate(county, .after = geocode)
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
#'   file = path
#' )
#'
#' }
read_organization <- function(file) {

  expected_colnames <- c(
    "OrganizationID",
    "OrganizationName"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROJECT" database table
    col_select = expected_colnames,
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
#'   file = path
#' )
#'
#' }
read_exit <- function(file) {

  # Handle column names inconsistencies in the HMIS database systems
  file_colnames <- readr::read_csv(
    file = file,
    n_max = 0,
    show_col_types = FALSE
  ) |> colnames()

  WorkplaceViolenceThreats <- intersect(
    file_colnames,
    c("WorkplaceViolenceThreats", "WorkPlaceViolenceThreats")
  )

  if (length(WorkplaceViolenceThreats) == 0)
    rlang::abort("Error: No matching column names found for Workplace Violence Threats.")

  WorkplacePromiseDifference <- intersect(
    file_colnames,
    c("WorkplacePromiseDifference", "WorkPlacePromiseDifference")
  )

  if (length(WorkplacePromiseDifference) == 0)
    rlang::abort("Error: No matching column names found for Workplace Promise Difference.")

  expected_colnames <- c(
    "ExitID",
    "EnrollmentID",
    "PersonalID",
    "ExitDate",
    "Destination",
    "OtherDestination",
    "ProjectCompletionStatus",
    "ExchangeForSex",
    "ExchangeForSexPastThreeMonths",
    "CountOfExchangeForSex",
    "AskedOrForcedToExchangeForSex",
    "AskedOrForcedToExchangeForSexPastThreeMonths",
    WorkplaceViolenceThreats,
    WorkplacePromiseDifference,
    "CoercedToContinueWork",
    "LaborExploitPastThreeMonths",
    "CounselingReceived",
    "IndividualCounseling",
    "FamilyCounseling",
    "GroupCounseling",
    "SessionCountAtExit",
    "PostExitCounselingPlan",
    "SessionsInPlan",
    "DestinationSafeClient",
    "DestinationSafeWorker",
    "PosAdultConnections",
    "PosPeerConnections",
    "PosCommunityConnections",
    "DateUpdated"
  )

  check_colnames(file, expected_colnames)

  # Ingest file
  exit <- readr::read_csv(
    file = file,
    # only read in columns needed for "PROGRAM" database table
    col_select = expected_colnames,
    # define schema types
    # use !!object := readr::col_*() if object stores a column name
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
        .cols = dplyr::all_of(
          c(
            "ExchangeForSex",
            "ExchangeForSexPastThreeMonths",
            "AskedOrForcedToExchangeForSex",
            "AskedOrForcedToExchangeForSexPastThreeMonths",
            WorkplaceViolenceThreats,
            WorkplacePromiseDifference,
            "CoercedToContinueWork",
            "LaborExploitPastThreeMonths",
            "DestinationSafeClient"
          )
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesReasonsForMissingDataCodes)
      ),
      dplyr::across(
        .cols = c(
          CounselingReceived,
          IndividualCounseling,
          FamilyCounseling,
          GroupCounseling,
          PostExitCounselingPlan
        ),
        .fns = function(x) lookup_codes(var = x, codes = NoYesMissingCodes)
      ),
      CountOfExchangeForSex = lookup_codes(
        var = CountOfExchangeForSex,
        codes = CountExchangeForSexCodes
      ),
      dplyr::across(
        .cols = c(
          DestinationSafeWorker,
          PosAdultConnections,
          PosPeerConnections,
          PosCommunityConnections
        ),
        .fns = function(x) lookup_codes(var = x, codes = WorkerResponseCodes)
      )
    ) |>
    janitor::clean_names(case = "snake")

  # If necessary, rename the column "workplace_violence_threats" to
  # "work_place_violence_threats" (this stems from differences in the HMIS
  # database systems)
  if ("workplace_violence_threats" %in% colnames(exit)) {

    exit <- exit |>
      dplyr::rename(work_place_violence_threats = workplace_violence_threats)

  }

  # If necessary, rename the column "work_place_promise_difference" to
  # "workplace_promise_difference" (this stems from differences in the HMIS
  # database systems)
  if ("work_place_promise_difference" %in% colnames(exit)) {

    exit <- exit |>
      dplyr::rename(workplace_promise_difference = work_place_promise_difference)

  }

  return(exit)

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
#'   file = path
#' )
#'
#' }
read_export <- function(file) {

  expected_colnames <- c(
    "ExportID",
    "SourceContactFirst",
    "SourceContactLast",
    "SourceContactEmail",
    "ExportStartDate",
    "ExportEndDate",
    "SoftwareName"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "PROGRAM" database table
    col_select = expected_colnames,
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character(),
      ExportStartDate = readr::col_date(),
      ExportEndDate = readr::col_date()
    )
  ) |>
    janitor::clean_names(case = "snake")

}

#' Ingest "Funder.csv" file and perform ETL prep for "FUNDER" database table
#'
#' @inheritParams read_client
#'
#' @return A data frame, containing the transformed data to be written out to
#'   the "FUNDER" database tables
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "path/to/Funder.csv"
#'
#' read_funder(
#'   file = path
#' )
#' }
read_funder <- function(file) {
  expected_colnames <- c(
    "ProjectID",
    "Funder",
    "OtherFunder"
  )

  check_colnames(file, expected_colnames)

  readr::read_csv(
    file = file,
    # only read in columns needed for "FUNDER" database table
    col_select = expected_colnames,
    # define schema types
    col_types = readr::cols(
      .default = readr::col_character()
    )
  ) |>
    # replace the integer codes with the plain-English description
    dplyr::mutate(
      Funder = lookup_codes(
        var = Funder,
        codes = FunderCodes
      )
    ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::rename(orig_project_id = project_id)
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

#' Check colnames
#'
#' `check_colnames()` compares a set of expected column names with the actual
#' column names in a .csv file and errors if any of the expected column names
#' is not found in the file.
#'
#' @param file String. Full path to a .csv file.
#' @param expected_colnames Character. Set of expected column names.
#'
#' @return `check_colnames()` does not return any value. It either produces an
#' error or not, so what matters is its side effect.
check_colnames <- function(file, expected_colnames) {

  # List columns in the file
  file_colnames <- readLines(file, n = 1) |>
    strsplit(",") |>
    unlist()

  # Determine which expected columns are not present in the file
  missing_columns <- setdiff(expected_colnames, file_colnames)

  if (length(missing_columns) > 0) {

    # Inform the user which columns are missing from the file
    rlang::abort(
      glue::glue(
        "The following column(s) are missing in { basename(file) }:<br>
        { paste0(missing_columns, collapse = ', ') }"
      )
    )

  }

}
