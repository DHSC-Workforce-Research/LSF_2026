# ===========================================================================
# scripts/p1b_programme_outcomes.r
#
# Per-programme leaving-before-finishing rate alongside placement hours, for
# the diverging-lollipop slide. The pack otherwise only has outcomes by band,
# which hides the programme-level picture. AGGREGATE only, n<10 suppressed.
#
#   source("scripts/p1b_programme_outcomes.r", encoding = "UTF-8")
# Output -> outputs_dir()/placement_hours_pack_YYYYMMDD/P1_outcomes_by_programme.csv
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({ library(dplyr); library(readr) })

MIN_N <- 10L
stamp <- format(Sys.Date(), "%Y%m%d")
pack  <- file.path(outputs_dir(), paste0("placement_hours_pack_", stamp))
dir.create(pack, showWarnings = FALSE, recursive = TRUE)

progress("p1b: per-programme leaving rate + hours ...")
stud <- placement_sample(quiet = TRUE)
stud <- mutate(stud, left_before_finish = to_01(left_before_finish))

prog <- stud |>
  filter(!is.na(programme_code)) |>
  group_by(programme_code, programme_name, course_family, hours_per_year) |>
  summarise(n_raw = sum(!is.na(left_before_finish)),
            pct   = 100 * mean(left_before_finish, na.rm = TRUE),
            .groups = "drop") |>
  mutate(pct_left_before_finish = if_else(n_raw < MIN_N, NA_real_, round(pct, 1)),
         n = if_else(n_raw < MIN_N, NA_integer_, as.integer(n_raw))) |>
  arrange(desc(hours_per_year)) |>
  select(programme_code, programme_name, course_family,
         hours_per_year, n, pct_left_before_finish)

write_csv(prog, file.path(pack, "P1_outcomes_by_programme.csv"))
progress("p1b: done -> ", file.path(pack, "P1_outcomes_by_programme.csv"))