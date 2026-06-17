# ===========================================================================
# 08_leaving_regression.r  -  logistic regression of considered-leaving on
#   entry funding influence, net of course and cohort composition.
#   ASSOCIATIONAL not causal: financial precarity (the real confounder) is
#   unobserved; course/cohort FEs remove composition only.
# ===========================================================================
library(dplyr); library(readr); library(stringr); library(broom)
purrr::walk(list.files("functions", full.names = TRUE), source)

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
                 show_col_types = FALSE)

# each student's entry (first-year) funding answers
entry_funding <- long |>
  filter(first_year == TRUE) |>
  group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(UniqueID, entry_year = year, course,
            grant_influence,
            funding_imp_crse = suppressWarnings(as.integer(funding_influence_course)),
            grant_helps_stay = str_detect(coalesce(grant_difference, ""),
                                          regex("stay on the course", ignore_case = TRUE)))

# each student's first continuing-wave answer to "considered leaving?"
considered_leaving_first <- long |>
  filter(first_year == FALSE, !is.na(leave_course)) |>
  group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(UniqueID, considered_leaving = leave_course)

# one row per student: entry funding linked to their later leaving answer
funding_leaving <- entry_funding |>
  inner_join(considered_leaving_first, by = "UniqueID") |>
  filter(!is.na(funding_imp_crse))

# funding importance as a FACTOR (ref = 1), grant_influence, net of course + cohort
leaving_model <- glm(
  considered_leaving ~ factor(funding_imp_crse) + grant_influence +
    factor(course) + factor(entry_year),
  family = binomial, data = funding_leaving
)

# adjusted odds ratios + 95% CIs for the funding terms (course dummies hidden)
tidy(leaving_model) |>
  filter(str_detect(term, "funding_imp_crse|grant_influence")) |>
  transmute(term,
            OR = round(exp(estimate), 2),
            lo = round(exp(estimate - 1.96 * std.error), 2),
            hi = round(exp(estimate + 1.96 * std.error), 2),
            p  = signif(p.value, 2)) |>
  print(n = Inf)


  