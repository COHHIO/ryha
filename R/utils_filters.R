filter_one_enrollment_per_group <- function(data, grouping_vars) {
    data |> 
      # Group data
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
      # Apply filters one at the time until we are left with one enrollment per person-organization
      ## Keep enrollment(s) without exit date (or with the most recent exit date if all enrollments have an exit date)
      dplyr::mutate(
        aux_exit = dplyr::case_when(
          is.na(exit_date) ~ as.Date("9999-01-01"),
          TRUE ~ exit_date
        )
      ) |>
      dplyr::filter(aux_exit == max(aux_exit)) |>
      dplyr::select(-aux_exit) |>
      ## Keep enrollment(s) that have the latest entry date
      dplyr::filter(entry_date == max(entry_date)) |>
      ## Keep enrollment(s) that have the latest date updated
      dplyr::filter(date_updated == max(date_updated)) |>
      ## Keep enrollment with the highest enrollment_id
      dplyr::filter(enrollment_id == max(enrollment_id)) |>
      # Ungroup data
      dplyr::ungroup()
}
