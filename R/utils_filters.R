#' Filter most recent enrollment per group
#'
#' `filter_most_recent_enrollment_per_group()` filters a dataset to retain only the most
#' recent enrollment for each group, based on a series prioritization criteria
#'
#' @param data A data frame containing enrollment information
#' @param grouping_vars A character vector specifying the grouping variables
#' to define groups within the data.
#'
#' @details
#' The most recent enrollment is determined by the following prioritization criteria,
#' applied sequentially:
#'
#' 1. Retain enrollment(s) without an exit date or with the most recent exit date.
#' 2. Retain enrollment(s) with the most recent entry date.
#' 3. Retain enrollment(s) with the most recent `date_updated`.
#' 4. Retain the enrollment with the highest `enrollment_id` as a final tie-breaker.
#'
#' @return A data frame with one enrollment per group
#'
#' @examples
#' \dontrun{
#' mock_data <- tibble::tribble(
#'     ~organization_id, ~personal_id, ~enrollment_id, ~entry_date, ~exit_date, ~date_updated,
#'     1L, 1L, 1000L, "2022-01-01", "2022-12-31", "2022-12-31",
#'     1L, 1L, 1001L, "2023-01-01", NA, "2023-01-01"
#' ) |>
#'     dplyr::mutate(
#'         dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
#'     )
#'
#' filter_most_recent_enrollment_per_group(mock_data, c("personal_id", "organization_id"))
#' }
filter_most_recent_enrollment_per_group <- function(data, grouping_vars) {
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
        ## Keep enrollment(s) that have the most recent entry date
        dplyr::filter(entry_date == max(entry_date)) |>
        ## Keep enrollment(s) that have the most recent date updated
        dplyr::filter(date_updated == max(date_updated)) |>
        ## Keep enrollment with the highest enrollment_id
        dplyr::filter(enrollment_id == max(enrollment_id)) |>
        # Ungroup data
        dplyr::ungroup()
}
