# ---------------------------------------------------------------------------
# build_funding_leaving_sample(): one row per student, entry funding -> leaving
#
# Joins each student's first observed first-year wave (their funding-influence
# answers) to two later outcomes: considered_leaving (their first continuing
# wave's self-report) and left_early (behavioural, stopped claiming before the
# expected finish recorded in the classified trajectory table). One row per
# student; an outcome is NA where it doesn't apply (no continuing wave seen, or
# expected finish beyond the fully-observed window 2025), so each model later
# self-selects its valid rows. Reused by the rate_by cross-tabs and the models.
# ---------------------------------------------------------------------------
build_funding_leaving_sample <- function(long, traj) {
  entry <- long |>
    dplyr::filter(first_year == TRUE) |>
    dplyr::group_by(UniqueID) |>
    dplyr::slice_min(year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      UniqueID, entry_year = year, course, grant_influence,
      funding_imp_uni  = suppressWarnings(as.integer(funding_influence_uni)),
      funding_imp_crse = suppressWarnings(as.integer(funding_influence_course)),
      grant_helps_stay = stringr::str_detect(
        dplyr::coalesce(grant_difference, ""),
        stringr::regex("stay on the course", ignore_case = TRUE))
    )

  considered <- long |>
    dplyr::filter(first_year == FALSE, !is.na(leave_course)) |>
    dplyr::group_by(UniqueID) |>
    dplyr::slice_min(year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(UniqueID, considered_leaving = leave_course)

  behaviour <- traj |>
    dplyr::transmute(
      UniqueID, last_wave, expected_finish,
      left_early = dplyr::if_else(expected_finish <= 2025,
                                  last_wave < expected_finish, NA))

  entry |>
    dplyr::left_join(considered, by = "UniqueID") |>
    dplyr::left_join(behaviour,  by = "UniqueID")
}