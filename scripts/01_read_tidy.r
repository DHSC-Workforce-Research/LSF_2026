# =====================================================================
# 01_read_tidy.R
# Raw LSF workbooks  ->  one tidy analysis table.
#
# Four stages, one pass:
#   A. ASSEMBLE   four shared workbook blocks -> one wide panel (+ QA)
#   B. LENGTHEN   wide panel -> one row per student per year answered
#   C. COLLAPSE   student-years -> one trajectory row per student, with a
#                 course-length-anchored reading of course exit attached
#   D. MODEL SET  student-level table: entry answers, leaving-outcome flags,
#                 financial confidence, university, bursary components
#
# Writes only to the secure derived folder, never the repo. This script
# prepares data; it computes no results and states none.
# Run:  source("scripts/01_read_tidy.R")
# =====================================================================

# --- setup -----------------------------------------------------------
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr)
deriv <- derived_dir()

# --- A. assemble + clean + QA ----------------------------------------
# The extract arrived as four positional blocks (block 2 lost its header).
# stitch_panel() stacks them and recovers the header; clean_lsf() repairs the
# GBP-symbol encoding, squishes whitespace, and turns blanks into NA. Values
# stay character on purpose: typing is an analysis decision, taken in stage D.
progress("A. assembling the four workbook blocks ...")
wide <- stitch_panel(panel_files()) |> clean_lsf()
print(check_panel(wide))              # hard QA: row / id / column counts must match

# --- B. lengthen -----------------------------------------------------
# Wide holds each student once, every question repeated behind a "YYYY-" prefix.
# reshape_long() turns that into one row per student-year and drops years a
# student did not answer. Persist it: stage D re-reads it with type guessing on.
progress("B. reshaping to one row per student-year ...")
long <- reshape_long(wide)
write_csv(long, file.path(deriv, "lsf_panel_long_2020_2026.csv"))

# --- C. collapse to trajectories + classify exit ---------------------
# One row per student. build_trajectories() applies field-specific rollups
# (latest value for current state, earliest observed first year for entry, an
# ever-flag for considered-leaving). classify_outcome() anchors an exit reading
# on course length, which build_course_lengths() estimates from the data itself.
progress("C. building trajectories and classifying course exit ...")
traj           <- build_trajectories(long)
panel_max      <- max(traj$last_wave, na.rm = TRUE)
course_lengths <- build_course_lengths(traj)
traj <- classify_outcome(traj, panel_max_year = panel_max, len_overrides = course_lengths)
write_csv(traj, file.path(deriv, "lsf_trajectories_classified_2020_2026.csv"))

# --- D. student-level analysis table ---------------------------------
# Re-read the long panel with type guessing on (TRUE/FALSE -> logical, 1-5
# scales -> integer), taking only the columns the analysis needs. This types
# the model inputs correctly without hand-coercion, and keeps the read light.
progress("D. building the student-level analysis table ...")
long_typed <- read_csv(
  file.path(deriv, "lsf_panel_long_2020_2026.csv"),
  col_select = c(UniqueID, year, first_year, course,
                 fund_availability, grant_influence,
                 funding_influence_uni, funding_influence_course,
                 grant_difference, leave_course,
                 confidence, college, grants_applied),
  show_col_types = FALSE)

# entry answers + every leaving-outcome definition (the functions own the logic)
sample <- build_funding_leaving_sample(long_typed, traj) |>
  define_leaving_outcomes()

# financial confidence: first continuing-wave value per student (asked from year 2)
confidence_cw <- long_typed |>
  filter(first_year == FALSE, !is.na(confidence)) |>
  arrange(UniqueID, year) |>
  group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |>
  select(UniqueID, confidence)

# entry-wave university and bursary components (extras on top of the base grant)
entry_extras <- long_typed |>
  filter(first_year == TRUE) |>
  group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(
    UniqueID, college,
    parental   = str_detect(coalesce(grants_applied, ""), regex("parental",   ignore_case = TRUE)),
    specialist = str_detect(coalesce(grants_applied, ""), regex("specialist", ignore_case = TRUE)),
    regional   = str_detect(coalesce(grants_applied, ""), regex("regional",   ignore_case = TRUE))) |>
  mutate(n_extras = parental + specialist + regional)

sample <- sample |>
  left_join(confidence_cw, by = "UniqueID") |>
  left_join(entry_extras,  by = "UniqueID")

# --- save the single analysis table ----------------------------------
saveRDS(sample, file.path(deriv, "lsf_analysis_sample.rds"))
progress("done: ", nrow(sample), " students  ->  lsf_analysis_sample.rds")