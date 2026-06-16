# ---------------------------------------------------------------------------
# build_trajectories(): long student-year panel -> one row per student
#
# The longitudinal payoff. Each student answered in one or more waves; this
# collapses those waves into a single trajectory row. The rollup rules differ
# by field on purpose:
#   - identity (college, course) and forward-looking fields (confidence,
#     finish_on_time, destination, start date) take the LATEST wave's value,
#     because that is the student's most current state.
#   - considered-leaving is an EVER rollup: TRUE if they reported it in any
#     wave, because once a student has considered leaving that is the signal
#     we care about, even if a later wave looks settled.
#   - entry is the EARLIEST wave seen, plus the wave they flagged first_year.
#
# Health warning the data forces on us: leave_course and finish_on_time are
# self-reported intentions per wave, not observed outcomes. This builds a
# trajectory of stated intention, not an administrative record of who left or
# who completed. Treat it as such downstream.
# ---------------------------------------------------------------------------
build_trajectories <- function(long, id = "UniqueID") {

  # latest non-NA value of x, ordered by year within the student
  latest <- function(x, yr) {
    ok <- !is.na(x)
    if (!any(ok)) return(NA_character_)
    x[ok][which.max(yr[ok])]
  }

  # earliest year where a TRUE/FALSE flag reads TRUE (NA if never)
  first_true_year <- function(flag, yr) {
    hit <- yr[!is.na(flag) & flag == "TRUE"]
    if (length(hit) == 0) NA_integer_ else min(hit)
  }

  # union of a multi-select column across waves, "; " separated (NA if empty)
  union_seen <- function(x) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0) NA_character_ else paste(unique(vals), collapse = "; ")
  }

  long |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::summarise(
      first_wave              = min(year),
      last_wave               = max(year),
      n_waves                 = dplyr::n_distinct(year),
      waves                   = paste(sort(unique(year)), collapse = ","),
      course_first_year_wave  = first_true_year(first_year, year),
      college                 = latest(college, year),
      course                  = latest(course, year),
      confidence_latest       = latest(confidence, year),
      ever_considered_leaving = any(leave_course == "TRUE", na.rm = TRUE),
      leave_factors_ever      = union_seen(leave_factors),
      expects_finish_on_time  = latest(finish_on_time, year),
      intended_destination    = latest(work_intensions, year),
      intended_start_date     = latest(start_work_date, year),
      .groups = "drop"
    )
}