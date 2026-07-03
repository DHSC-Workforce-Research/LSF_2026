# =====================================================================
# 16_university_robust.r  -  is the university leaving spread real, or
# the same course-length artefact? Compare anchored vs anchor-free.
# Run:  source("scripts/16_university_robust.r")
# =====================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(readr)
  purrr::walk(list.files("functions", full.names = TRUE), source)
}))
smp <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_3spec_sample.rds")))
lm  <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
                col_select = c("UniqueID", "year", "first_year", "college"), show_col_types = FALSE)
entry <- lm |> filter(first_year == TRUE) |>
  group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(UniqueID, college = as.character(college))
smp <- left_join(smp, entry, by = "UniqueID")

spread <- function(v) {
  t <- smp |> filter(!is.na(.data[[v]]), !is.na(college)) |>
    group_by(college) |> summarise(n = n(), r = 100 * mean(.data[[v]]), .groups = "drop") |>
    filter(n >= 200)
  data.frame(measure = v, n_unis = nrow(t),
             p10 = round(quantile(t$r, .10), 1), median = round(quantile(t$r, .50), 1),
             p90 = round(quantile(t$r, .90), 1), sd_between = round(sd(t$r), 1))
}
uni_robust <- bind_rows(spread("left_before_finish"), spread("one_wave_only"))
print(uni_robust, row.names = FALSE)
message("object: uni_robust")