# organization_filter_global <- c(
#   "YWCA",
#   "FrontLine Service"
# )

project_filter_global <- c(
  "YWCA - ODH",
  "FL - ODH"
)

age_filter_global <- 18

gender_filter_global <- "female"

ethnicity_filter_global <- c(
  "black_af_american", "asian"
)




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

