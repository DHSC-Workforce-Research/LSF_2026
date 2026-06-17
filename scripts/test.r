traj |>
  mutate(swd = suppressWarnings(as.integer(intended_start_date)),
         implied_len = swd - course_first_year_wave) |>
  filter(!is.na(implied_len), implied_len >= 1, implied_len <= 6) |>
  group_by(course) |>
  summarise(n = n(),
            median_len = median(implied_len),
            p25 = quantile(implied_len, .25),
            p75 = quantile(implied_len, .75),
            .groups = "drop") |>
  arrange(desc(n)) |>
  print(n = Inf)