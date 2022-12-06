

dir <- "data/stage_for_db/hudx-111_1651251197/"

submission_id <- 2L

project <- readr::read_csv(
  file = paste0(dir, "/Project.csv"),
  col_select = ProjectID,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

export <- readr::read_csv(
  file = paste0(dir, "/Export.csv"),
  col_select = c(
    ExportID,
    SoftwareName,
    # SourceContactFirst,
    # SourceContactLast,
    # SourceContactEmail,
    ExportStartDate,
    ExportEndDate
  ),
  col_types = readr::cols(
    .default = readr::col_character(),
    ExportStartDate = readr::col_date(),
    ExportEndDate = readr::col_date()
  )
)

submission <- tibble::tibble(
  submission_id = submission_id,
  DateTimeSubmitted = Sys.time(),
  DateSubmitted = Sys.Date()
) |>
  dplyr::bind_cols(project) |>
  dplyr::bind_cols(export)

client <- read_client(
  file = paste0(dir, "/Client.csv"),
  submission_id = submission_id
)

disabilities <- read_disabilities(
  file = paste0(dir, "/Disabilities.csv"),
  submission_id = submission_id
)

current_living_situation <- read_living(
  file = paste0(dir, "/CurrentLivingSituation.csv"),
  submission_id = submission_id
)

employment <- read_employment(
  file = paste0(dir, "/EmploymentEducation.csv"),
  submission_id = submission_id
)

education <- read_education(
  file = paste0(dir, "/EmploymentEducation.csv"),
  submission_id = submission_id
)

# Create the table, 'client'
DBI::dbWriteTable(
  conn = con,
  name = "employment",
  value = employment |> janitor::clean_names(case = "snake"),
  append = TRUE
)
