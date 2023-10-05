## code to prepare `ExitCodes` dataset goes here

ProjectCompletionStatusCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Completed project",
  2L, "Youth voluntarily left early",
  3L, "Youth was expelled or otherwise involuntarily discharged from project"
)

CountExchangeForSexCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "1-3",
  2L, "4-7",
  3L, "8-11",
  4L, "12 or more",
  8L, "Client doesn't know",
  9L, "Client prefers not to answer",
  99L, "Data not collected"
)

WorkerResponseCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Yes",
  2L, "Worker does not know"
)


usethis::use_data(
  ProjectCompletionStatusCodes,
  CountExchangeForSexCodes,
  WorkerResponseCodes,
  overwrite = TRUE
)
