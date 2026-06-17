# ===========================================================================
# 04_validate_outcomes.r  -  diagnostics on the constructed dropout flag:
#   (a) WHERE in the course do the 'dropouts' actually stop claiming?
#   (b) does our dropout flag agree with the leave_course self-report?
# ===========================================================================
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr)
library(readr)

traj <- read_csv(
  file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"),
  show_col_types = FALSE
)

classifiable <- traj |>
  filter(outcome_cautious %in% c("completed", "dropped_out"))

# --- (a) gap between last claim and expected finish -----------------------
# gap = last_wave - expected_finish.  0 or positive = stopped at/after finish
# (completed).  -1 = stopped one year early, -2 = two years early, etc.
# A big spike at -1 means people routinely stop claiming in their FINAL year
# (placement-heavy year / one-off payment / didn't reapply), i.e. completion
# misread as dropout, which would JUSTIFY grace = 1. A flat spread across
# -1,-2,-3 means genuine early leaving and we leave grace alone.
cat("gap = last_wave - expected_finish (classifiable only):\n")
print(
  classifiable |>
    mutate(gap = last_wave - expected_finish) |>
    count(gap) |>
    arrange(gap)
)

# --- (b) convergent validity vs the self-report --------------------------
# Do students we CALL dropped report 'considered leaving' more often than
# those we call completed? If yes, two independent signals corroborate and the
# flag is real. If the shares are the same, our constructed flag is noise.
cat("\never_considered_leaving by constructed outcome:\n")
print(
  classifiable |>
    count(outcome_cautious, ever_considered_leaving) |>
    group_by(outcome_cautious) |>
    mutate(share = round(n / sum(n), 3)) |>
    ungroup()
)