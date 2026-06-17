# ===========================================================================
# 05_transition_test.r  -  does reporting "considered leaving" in year t
#   predict LEAVING (gone next year / never returning) among continuing
#   students still expected to be enrolled? Validates leave_course against
#   behaviour, controlling for the exposure artefact that broke the earlier
#   'ever' rollup.
#
#   Scope: continuing student-years (leave_course asked), t = 2021-2024 so t+1
#   is a fully-collected wave (2020 = thin launch, 2026 = partial), and
#   t+1 <= expected_finish so we test leaving-before-finishing, not graduation.
# ===========================================================================
library(dplyr); library(readr)
purrr::walk(list.files("functions", full.names = TRUE), source)

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
                 show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"),
                 show_col_types = FALSE)

present <- long |> distinct(UniqueID, year) |> mutate(present = TRUE)
anchor  <- traj |> select(UniqueID, expected_finish, last_wave)

trans <- long |>
  filter(!is.na(leave_course), year >= 2021, year <= 2024) |>      # asked; t+1 is a full wave
  distinct(UniqueID, year, leave_course) |>
  mutate(year_next = year + 1L) |>
  left_join(anchor, by = "UniqueID") |>
  filter(!is.na(expected_finish), year_next <= expected_finish) |> # still expected enrolled
  left_join(present |> rename(year_next = year), by = c("UniqueID", "year_next")) |>
  mutate(absent_next   = is.na(present),       # gone the very next wave
         gone_for_good = last_wave == year)    # never appears again after t

cat("Does 'considered leaving' in year t predict leaving?\n")
cat("(continuing students, t = 2021-2024, still expected enrolled in t+1)\n\n")
print(
  trans |>
    group_by(considered_leaving = leave_course) |>
    summarise(
      n             = n(),
      absent_next   = round(mean(absent_next),   3),
      gone_for_good = round(mean(gone_for_good), 3),
      .groups = "drop"
    )
)