# ===========================================================================
# 03_classify_outcomes.r  -  read the trajectory table, attach two readings of
#                            course exit (literal + cautious), show the split
#                            and the dropout rate among the classifiable, then
#                            save the classified table
# ===========================================================================
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr)
library(readr)

# --- read the per-student trajectory table built by 02 ------------------
traj <- read_csv(
  file.path(derived_dir(), "lsf_trajectories_2020_2026.csv"),
  show_col_types = FALSE
)

# --- classify: literal (face value) and cautious (course-length anchored) -
panel_max <- max(traj$last_wave)
course_lengths <- build_course_lengths(traj)                 # <- NEW: data-driven lengths
traj <- classify_outcome(traj, panel_max_year = panel_max,   # <- add the len_overrides arg
                         len_overrides = course_lengths)

# --- view 1: the full cautious split, including the honest censored / -----
#     no_entry_observed buckets we can't classify
cat("panel_max_year:", panel_max, "\n\n")
cat("CAUTIOUS split (all students):\n")
print(count(traj, outcome_cautious))

# --- view 2: the validation -----------------------------------------------
#     of the students we CAN call (entry seen, reached expected finish),
#     what share dropped vs completed? Want this near 25-33%.
cat("\nDropout rate AMONG THE CLASSIFIABLE (completed + dropped only):\n")
print(
  traj |>
    filter(outcome_cautious %in% c("completed", "dropped_out")) |>
    count(outcome_cautious) |>
    mutate(share = round(n / sum(n), 3))
)

# --- view 3: literal vs cautious, to see how 'left' decomposes ------------
cat("\nLITERAL vs CAUTIOUS:\n")
print(count(traj, outcome_literal, outcome_cautious))

# --- save the classified trajectory table ---------------------------------
write_csv(traj, file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"))
cat("\nwritten to", derived_dir(), "\n")