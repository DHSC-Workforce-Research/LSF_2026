# ---------------------------------------------------------------------------
# define_leaving_outcomes(): all leaving definitions in ONE editable place.
# Input: student-level df with last_wave, expected_finish, course_first_year_wave,
# considered_leaving (first continuing wave), ever_considered_leaving.
# Behavioural flags are only defined for students whose expected finish falls
# inside the fully-observed window (<= 2025); otherwise NA (they haven't had the
# chance to be seen finishing). Edit the conditions here, nowhere else.
# ---------------------------------------------------------------------------
define_leaving_outcomes <- function(df) {
  dplyr::mutate(df,
    obs_window  = !is.na(expected_finish) & expected_finish <= 2025,
    years_early = expected_finish - last_wave,

    completed          = dplyr::if_else(obs_window, last_wave >= expected_finish, NA),
    left_before_finish = dplyr::if_else(obs_window, last_wave <  expected_finish, NA),
    left_2y_plus_early = dplyr::if_else(obs_window, years_early >= 2,            NA),
    left_after_year1   = dplyr::if_else(obs_window,
                            last_wave == course_first_year_wave &
                            expected_finish > course_first_year_wave, NA),

    considered_first    = considered_leaving,         # first continuing wave
    considered_ever     = ever_considered_leaving,    # any continuing wave
    considered_and_left = considered_ever & dplyr::coalesce(left_before_finish, FALSE)
  )
}