

dir <- "data/stage_for_db/hudx-111_YWCA/"

submission_id <- 3L

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
    SourceContactFirst,
    SourceContactLast,
    SourceContactEmail,
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

# Create the table, 'client'
DBI::dbWriteTable(
  conn = con,
  name = "submission",
  value = submission |> janitor::clean_names(case = "snake"),
  append = TRUE
)
