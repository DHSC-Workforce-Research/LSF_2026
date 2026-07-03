# =====================================================================
# 17_bursary_components.r  -  do bursary components / their number predict
# leaving, net of subject and cohort? Anchor-free outcome (one_wave_only).
# NB: components are eligibility markers (parental = has children,
# specialist = shortage subject, regional = place), not money doses.
# Run:  source("scripts/17_bursary_components.r")
# =====================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(stringr); library(fixest)
  purrr::walk(list.files("functions", full.names = TRUE), source)
}))
smp <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_3spec_sample.rds")))

# components from the panel at entry
lm <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
               col_select = c("UniqueID", "year", "first_year", "grants_applied"), show_col_types = FALSE)
entry <- lm |> filter(first_year == TRUE) |>
  group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(UniqueID, g = coalesce(as.character(grants_applied), ""))
smp <- left_join(smp, entry, by = "UniqueID") |>
  mutate(parental     = as.integer(str_detect(g, regex("parental",   ignore_case = TRUE))),
         specialist   = as.integer(str_detect(g, regex("specialist", ignore_case = TRUE))),
         regional     = as.integer(str_detect(g, regex("regional",   ignore_case = TRUE))),
         n_extras     = parental + specialist + regional,     # 0-3 on top of the universal grant
         n_components = 1L + n_extras)                          # 1-4 total

# (a) leaving by NUMBER of components, both outcomes for contrast
by_count <- smp |> filter(!is.na(one_wave_only)) |>
  group_by(n_components) |>
  summarise(n = n(),
            one_wave    = round(100 * mean(one_wave_only), 1),
            left_before = round(100 * mean(left_before_finish, na.rm = TRUE), 1), .groups = "drop")
cat("=== (a) leaving by NUMBER of components (1 = base grant only) ===\n")
print(as.data.frame(by_count), row.names = FALSE); cat("\n")

# adjusted odds ratios via fixest (FE absorbed), anchor-free outcome
ortab <- function(terms, fe, outcome = "one_wave_only") {
  f <- as.formula(paste0(outcome, " ~ ", terms, " | ", fe))
  m <- tryCatch(feglm(f, family = binomial, data = smp, warn = FALSE, notes = FALSE), error = function(e) NULL)
  if (is.null(m)) return(data.frame(term = "(unidentified/dropped)", OR = NA, lo = NA, hi = NA, adjust = fe))
  ct <- as.data.frame(coeftable(m))
  data.frame(term = rownames(ct), OR = round(exp(ct$Estimate), 3),
             lo = round(exp(ct$Estimate - 1.96 * ct[, "Std. Error"]), 3),
             hi = round(exp(ct$Estimate + 1.96 * ct[, "Std. Error"]), 3), adjust = fe)
}

cat("=== (b) each component, adjusted (specialist/regional absorbed by course FE) ===\n")
comp_or <- bind_rows(
  ortab("parental + specialist + regional", "entry_year"),            # cohort only: all three estimable
  ortab("parental + specialist + regional", "course + entry_year"))   # + subject: parental is the clean one
print(comp_or, row.names = FALSE); cat("\n")

cat("=== (c) number of extras (0-3) as a dose, adjusted ===\n")
dose_or <- bind_rows(
  ortab("n_extras", "entry_year"),
  ortab("n_extras", "course + entry_year"))
print(dose_or, row.names = FALSE)
message("objects: by_count, comp_or, dose_or")