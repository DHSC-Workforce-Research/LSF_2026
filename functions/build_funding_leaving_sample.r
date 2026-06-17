# ---------------------------------------------------------------------------
# build_funding_leaving_sample(): one row per student, entry funding answers +
# the raw trajectory ingredients the outcome definitions need. Outcome flags
# are added separately by define_leaving_outcomes(); this just gathers inputs.
# ---------------------------------------------------------------------------
build_funding_leaving_sample <- function(long, traj) {
  progress("  sample: entry (first-year) funding records ...")
  entry <- long |>
    dplyr::filter(first_year == TRUE) |>
    dplyr::group_by(UniqueID) |>
    dplyr::slice_min(year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      UniqueID, entry_year = year, course,
      fund_availability, grant_influence,
      funding_imp_uni  = suppressWarnings(as.integer(funding_influence_uni)),
      funding_imp_crse = suppressWarnings(as.integer(funding_influence_course)),
      grant_helps_stay = stringr::str_detect(
        dplyr::coalesce(grant_difference, ""),
        stringr::regex("stay on the course", ignore_case = TRUE)))

  progress("  sample: first continuing-wave leave_course ...")
  considered <- long |>
    dplyr::filter(first_year == FALSE, !is.na(leave_course)) |>
    dplyr::group_by(UniqueID) |>
    dplyr::slice_min(year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(UniqueID, considered_leaving = leave_course)

  progress("  sample: trajectory fields ...")
  trj <- traj |>
    dplyr::transmute(UniqueID, first_wave, last_wave, n_waves,
                     course_first_year_wave, expected_finish, ever_considered_leaving)

  progress("  sample: joining ...")
  out <- entry |>
    dplyr::left_join(considered, by = "UniqueID") |>
    dplyr::left_join(trj,        by = "UniqueID")
  progress("  sample: done, ", nrow(out), " students")
  out
}