# organization_filter_global <- c(
#   "YWCA",
#   "FrontLine Service"
# )

input <- list(

  project_filter_global = c(
    "YWCA - ODH",
    "FL - ODH"
  ),

  min_entry_date_filter_global = as.Date("2021-01-01"),

  age_filter_global = c(18, 24),

  gender_filter_global = "female",

  ethnicity_filter_global = c(
    "black_af_american", "asian"
  )

)


# active_start_date <- as.Date("2021-01-01")
#
# active_end_date <- as.Date("2021-09-30")


# Filter individual datasets
enrollment_filtered <- enrollment |>
  dplyr::filter(entry_date <= active_end_date) |>
  tibble::as_tibble()

exit_filtered <- exit |>
  dplyr::filter(exit_date >= active_start_date) |>
  tibble::as_tibble()


personal_id_filter <- project |>
  dplyr::filter(project_name %in% project_filter_global) |>
  dplyr::select(project_id) |>
  dplyr::inner_join(
    enrollment,
    by = "project_id"
  ) |>
  dplyr::distinct(personal_id, software_name) |>
  dplyr::inner_join(
    client |>
      dplyr::filter(
        age > age_filter_global
      ),
    by = c("personal_id", "software_name")
  ) |>
  dplyr::inner_join(
    gender |>
      dplyr::filter(
        gender %in% gender_filter_global
      ),
    by = c("personal_id", "software_name")
  ) |>
  dplyr::inner_join(
    ethnicity |>
      dplyr::filter(
        ethnicity %in% ethnicity_filter_global
      ),
    by = c("personal_id", "software_name")
  ) |>
  dplyr::distinct(personal_id, software_name)


disabilities_filtered <- disabilities |>
  dplyr::inner_join(
    personal_id_filter,
    by = c("personal_id", "software_name")
  )

