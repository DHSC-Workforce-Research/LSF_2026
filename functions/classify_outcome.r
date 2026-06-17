# ---------------------------------------------------------------------------
# classify_outcome(): two readings of course exit, anchored on course length
#
# Compulsory-to-claim means absence from a wave is a decent proxy for "no
# longer an eligible student", i.e. left the course by finishing or dropping.
# We separate those two by comparing the last wave a student claimed against
# their expected finish year = observed entry year + course length - 1.
#
# We ONLY classify students whose first year we actually saw (first_year flag),
# because for anyone already mid-course when the survey began we never observed
# the start and can't place a finish. Those are flagged no_entry_observed.
#
#   outcome_literal    blunt: not in the final wave = "left". A diagnostic.
#   outcome_cautious   active / completed / dropped_out / censored (would finish
#                      beyond the data) / no_entry_observed (left-censored).
#
# Anchored on course length, NOT start_work_date: the old start_work_date anchor
# was post-graduation and flaky, which is what collapsed completion to ~1%.
# ---------------------------------------------------------------------------
classify_outcome <- function(traj,
                             panel_max_year,
                             default_len   = 3L,
                             len_overrides = NULL,
                             grace         = 0L,
                             active_lag    = 1L) {
  traj |>
    dplyr::mutate(
      entry_year      = course_first_year_wave,                       # observed year 1 only
      len             = course_length(course, default_len, len_overrides),
      expected_finish = entry_year + len - 1L,

      outcome_literal = dplyr::if_else(
        last_wave >= panel_max_year, "active", "left"
      ),

      outcome_cautious = dplyr::case_when(
        last_wave  >= panel_max_year - active_lag ~ "active",
        is.na(entry_year)                         ~ "no_entry_observed",
        expected_finish > panel_max_year          ~ "censored",
        last_wave  >= expected_finish - grace      ~ "completed",
        TRUE                                       ~ "dropped_out"
      )
    )
}