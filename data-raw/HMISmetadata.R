## code to prepare `HMISmetadata` dataset goes here


# This table provides some metadata about the HMIS data
HMISmetadata <- tibble::tribble(
  ~FileName, ~Required,
  "Affiliation.csv", "N",
  "Assessment.csv", "N",
  "AssessmentQuestions.csv", "N",
  "AssessmentResults.csv", "N",
  "CEParticipation.csv", "N",
  "Client.csv", "Y",
  "CurrentLivingSituation.csv", "Y",
  "Disabilities.csv", "Y",
  "EmploymentEducation.csv", "Y",
  "Enrollment.csv", "Y",
  "EnrollmentCoC.csv", "N",
  "Event.csv", "N",
  "Exit.csv", "Y",
  "Export.csv", "Y",
  "Funder.csv", "N",
  "HealthAndDV.csv", "Y",
  "HMISParticipation.csv", "N",
  "IncomeBenefits.csv", "Y",
  "Inventory.csv", "N",
  "Organization.csv", "Y",
  "Project.csv", "Y",
  "ProjectCoC.csv", "Y",
  "Services.csv", "Y",
  "User.csv", "N",
  "YouthEducationStatus.csv", "N"
)

usethis::use_data(HMISmetadata, overwrite = TRUE)
