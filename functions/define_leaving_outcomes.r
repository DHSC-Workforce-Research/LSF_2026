# ---------------------------------------------------------------------------
# define_leaving_outcomes(): every leaving definition in ONE editable place.
# Each flag: plain meaning, then the technical condition. Behavioural flags are
# only defined inside the fully-observed window (expected finish <= panel_max-1),
# else NA. Self-report flags only exist for students actually asked (a continuing
# wave). Edit conditions here and nowhere else.
# panel_max is derived from the data (latest wave seen), so the window and the
# one-wave gate extend themselves when a new wave lands. Nothing is hard-coded.
# Needs: last_wave, expected_finish, course_first_year_wave, n_waves,
#        considered_leaving (first continuing wave), ever_considered_leaving.
# ---------------------------------------------------------------------------
define_leaving_outcomes <- function(df) {
  panel_max <- max(df$last_wave, na.rm = TRUE)   # latest wave in the data (e.g. 2026)

  dplyr::mutate(df,
    # Fully-observed window: course should have finished at least one wave before
    # the latest data we hold (the -1 keeps a conservative buffer for students
    # whose expected finish is the very last wave and so are ambiguous).
    obs_window  = !is.na(expected_finish) & expected_finish <= panel_max - 1L,
    years_early = expected_finish - last_wave,

    # COMPLETION
    # Stayed to the finish line (last_wave >= expected_finish).
    reached_finish        = dplyr::if_else(obs_window, last_wave >= expected_finish, NA),

    # BEHAVIOURAL LEAVING, loosest -> strictest
    # Left before finishing, any amount early (last_wave < expected_finish). Noisiest: includes non-reclaim.
    left_before_finish    = dplyr::if_else(obs_window, last_wave <  expected_finish, NA),
    # Stopped only in the final year (years_early == 1). Isolates likely non-claim noise.
    left_final_year_only  = dplyr::if_else(obs_window, years_early == 1, NA),
    # Left well before the end, 2+ years early (years_early >= 2). Stronger dropout signal.
    left_2y_plus_early    = dplyr::if_else(obs_window, years_early >= 2, NA),
    # Left very early, 3+ years early (years_early >= 3). Strongest finish-anchored signal.
    left_3y_plus_early    = dplyr::if_else(obs_window, years_early >= 3, NA),
    # Dropped out after first year (last_wave == course_first_year_wave & had further to go).
    left_after_year1      = dplyr::if_else(obs_window,
                              last_wave == course_first_year_wave &
                              expected_finish > course_first_year_wave, NA),
    # Claimed once and never again (n_waves == 1). Definition-light, no finish
    # needed. GATED so a brand-new entrant in the final wave (who has had no
    # chance to reappear) is NA rather than a false "leaver". Keeps anyone who
    # entered before the last observed wave and so had at least one opportunity
    # to return.
    one_wave_only         = dplyr::if_else(course_first_year_wave < panel_max,
                                           n_waves == 1L, NA),

    # SELF-REPORT (intention; continuing students only)
    # Thought about leaving, first continuing wave (leave_course == TRUE, first year-2+ response).
    considered_first      = considered_leaving,
    # Ever thought about leaving, any continuing wave (ever_considered_leaving), gated to those asked.
    considered_ever       = dplyr::if_else(is.na(considered_leaving), NA, ever_considered_leaving),

    # INTENTION -> BEHAVIOUR
    # Thought about leaving and then left (considered_ever & left_before_finish).
    considered_and_left   = considered_ever & dplyr::coalesce(left_before_finish, FALSE),
    # Thought about leaving but finished anyway (considered_ever & reached_finish): retained-despite-doubt.
    considered_but_stayed = considered_ever & dplyr::coalesce(reached_finish, FALSE)
  )
}