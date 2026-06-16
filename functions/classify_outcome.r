# ---------------------------------------------------------------------------
# classify_outcome(): infer a course exit from panel attrition
#
# The data has no "did this student leave" field, but the panel leaks it: a
# student stops appearing either because they DROPPED OUT or because they
# GRADUATED, and those are opposites. A 3-year course inside a 2020-2026 window
# means finishing IS disappearing, so naive "absent = dropout" would brand most
# SUCCESSFUL students as leavers. We avoid that by anchoring on each student's
# expected finish year and splitting the exit:
#   active      seen in the final panel wave            -> still enrolled
#   completed   last seen at/after expected finish      -> left because done
#   dropped_out last seen before expected finish        -> left early (candidate)
#   censored    would finish beyond our data window      -> can't conclude
#   unknown     no basis to place an expected finish     -> unclassifiable
#
# expected finish = stated start_work_date, else first-year wave + course_len.
# Every threshold is an argument so the assumptions are visible and tunable.
# This is INFERENCE FROM ATTRITION, not an observed leaving record.
# ---------------------------------------------------------------------------
classify_outcome <- function(traj,
                             panel_max_year,
                             course_len = 3L,
                             grace      = 0L) {
  traj |>
    dplyr::mutate(
      expected_finish = dplyr::coalesce(
        suppressWarnings(as.integer(intended_start_date)),
        course_first_year_wave + course_len - 1L
      ),
      outcome = dplyr::case_when(
        last_wave >= panel_max_year              ~ "active",
        is.na(expected_finish)                   ~ "unknown",
        expected_finish > panel_max_year         ~ "censored",
        last_wave >= expected_finish - grace     ~ "completed",
        TRUE                                     ~ "dropped_out"
      )
    )
}