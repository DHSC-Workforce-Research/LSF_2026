# scripts/97_validate_course_length.r  -  does the course-median length predict
#   an individual's own declared length well? Calibration check on survivors.
library(dplyr); library(readr); library(tibble)
purrr::walk(list.files("functions", full.names = TRUE), source)

traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"),
                 show_col_types = FALSE)

# the prediction: course-median length (what we'd use when we don't know the individual)
course_len <- build_course_lengths(traj) |> enframe(name = "course", value = "course_median_len")

# the realisation: each student's OWN declared length (needs start_work_date)
declared <- traj |>
  mutate(swd = suppressWarnings(as.integer(intended_start_date)),
         own_len = swd - course_first_year_wave) |>
  filter(!is.na(own_len), own_len >= 1, own_len <= 6) |>
  inner_join(course_len, by = "course") |>
  mutate(err = own_len - course_median_len)

cat("distribution of (own declared length - course median):\n")
declared |> count(err) |> mutate(pct = round(n / sum(n), 3)) |> arrange(err) |> print()

cat("\nheadline accuracy of the course-median guess:\n")
declared |>
  summarise(n       = n(),
            exact   = round(mean(err == 0),     3),   # spot on
            within1 = round(mean(abs(err) <= 1), 3),  # within a year
            mae     = round(mean(abs(err)),      2)) |># mean absolute error, years
  print()