# ===========================================================================
# 06_funding_retention_link.r  -  ASSOCIATIONAL: does first-year funding
#   influence relate to later considered-leaving? Links each student's entry
#   funding answers (first_year == TRUE wave) to their FIRST continuing wave's
#   leave_course (one clean obs per student, no exposure bias).
#
#   NOT causal: students for whom the grant was decisive are more financially
#   marginal to begin with, so any association is confounded by selection.
# ===========================================================================
library(dplyr); library(readr); library(stringr)
purrr::walk(list.files("functions", full.names = TRUE), source)

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
                 show_col_types = FALSE)

# --- entry record: each student's first observed first-year wave ----------
entry <- long |>
  filter(first_year == TRUE) |>
  group_by(UniqueID) |>
  slice_min(year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    UniqueID,
    entry_year       = year,
    grant_influence,                                              # grant influenced decision to enter
    funding_imp_uni  = suppressWarnings(as.integer(funding_influence_uni)),
    funding_imp_crse = suppressWarnings(as.integer(funding_influence_course)),
    grant_helps_stay = str_detect(coalesce(grant_difference, ""),
                                  regex("stay on the course", ignore_case = TRUE))
  )

# --- outcome: each student's FIRST continuing wave, did they consider leaving
outcome <- long |>
  filter(first_year == FALSE, !is.na(leave_course)) |>
  group_by(UniqueID) |>
  slice_min(year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(UniqueID, cont_year = year, considered_leaving = leave_course)

# --- link: students with BOTH an entry record and a later continuing wave -
b <- entry |>
  inner_join(outcome, by = "UniqueID") |>
  filter(cont_year > entry_year)

cat("students with entry + later continuing observation:", nrow(b), "\n")
cat("overall considered-leaving rate:", round(mean(b$considered_leaving), 3), "\n\n")

cat("by grant_influence (grant influenced the decision to enrol):\n")
print(rate_by(b, "grant_influence"))

cat("\nby grant_helps_stay (picked 'helps me stay on the course & complete'):\n")
print(rate_by(b, "grant_helps_stay"))

cat("\nby funding importance to WHERE to study (1-5):\n")
print(rate_by(b, "funding_imp_uni"))

cat("\nby funding importance to WHAT to study (1-5):\n")
print(rate_by(b, "funding_imp_crse"))