# ---------------------------------------------------------------------------
# build_course_lengths(): data-driven course length lookup for len_overrides.
# Length = rounded median of (intended start-work year - first-year wave) per
# course, i.e. how long students on that course actually expect to take. Only
# trusts courses with enough students (min_n); the rest fall back to the
# default 3 in course_length(). Clamped to 2-4 years. Returns a named vector
# ready to pass as classify_outcome(..., len_overrides = ...).
# ---------------------------------------------------------------------------
build_course_lengths <- function(traj, min_n = 20L) {
  traj |>
    dplyr::mutate(swd     = suppressWarnings(as.integer(intended_start_date)),
                  implied = swd - course_first_year_wave) |>
    dplyr::filter(!is.na(implied), implied >= 1, implied <= 6) |>
    dplyr::group_by(course) |>
    dplyr::summarise(n = dplyr::n(), len = round(stats::median(implied)), .groups = "drop") |>
    dplyr::filter(n >= min_n) |>
    dplyr::mutate(len = as.integer(pmin(pmax(len, 2L), 4L))) |>
    dplyr::select(course, len) |>
    tibble::deframe()
}