## code to prepare `HMISmetadata` dataset goes here


# This table provides some metadata about the HMIS data
HMISmetadata <- tibble::tribble(
  ~FileName, ~Required,
  "Affiliation.csv", "Y",
  "Assessment.csv", "Y",
  "AssessmentQuestions.csv", "Y",
  "AssessmentResults.csv", "Y",
  "Client.csv", "Y",
  "CurrentLivingSituation.csv", "Y",
  "Disabilities.csv", "Y",
  "EmploymentEducation.csv", "Y",
  "Enrollment.csv", "Y",
  "Event.csv", "Y",
  "Exit.csv", "Y",
  "Export.csv", "Y",
  "Funder.csv", "Y",
  "HealthAndDV.csv", "Y",
  "IncomeBenefits.csv", "Y",
  "Inventory.csv", "Y",
  "Organization.csv", "Y",
  "Project.csv", "Y",
  "ProjectCoC.csv", "Y",
  "Services.csv", "Y",
  "User.csv", "Y",
  "YouthEducationStatus.csv", "Y",
)

usethis::use_data(HMISmetadata, overwrite = TRUE)
