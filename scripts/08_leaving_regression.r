# ===========================================================================
# 08_leaving_regression.r  -  logistic models of leaving on entry funding
#   dependence, with a side-by-side odds-ratio table. ASSOCIATIONAL not causal
#   (financial precarity, the real confounder, is unobserved; course/cohort
#   FEs remove composition only).
# ===========================================================================
library(dplyr); library(readr); library(stringr); library(purrr)
library(broom); library(modelsummary)
purrr::walk(list.files("functions", full.names = TRUE), source)
progress("reading data ...")
long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)

progress("building analysis sample ...")
sample <- build_funding_leaving_sample(long, traj)

progress("fitting model 1/3 (raw) ...")
model_raw <- glm(considered_leaving ~ I(funding_imp_crse >= 4) + grant_influence,
                 family = binomial, data = sample)
progress("fitting model 2/3 (considered + course & cohort FE) ...")
model_adjusted <- glm(considered_leaving ~ I(funding_imp_crse >= 4) + grant_influence +
                        factor(course) + factor(entry_year),
                      family = binomial, data = sample)
progress("fitting model 3/3 (left-early + course & cohort FE) ...")
model_behaviour <- glm(left_early ~ I(funding_imp_crse >= 4) + grant_influence +
                         factor(course) + factor(entry_year),
                       family = binomial, data = sample)

out_file <- file.path(outputs_dir(), "lsf_models.html")
progress("rendering table (Wald CIs) -> ", out_file)
modelsummary(
  list("Considered leaving (raw)"        = model_raw,
       "Considered leaving (+ controls)" = model_adjusted,
       "Left early (+ controls)"         = model_behaviour),
  exponentiate = TRUE, statistic = "conf.int", ci_method = "wald",
  stars = c("*" = .05, "**" = .01, "***" = .001),
    coef_map = c(
    "I(funding_imp_crse >= 4)TRUE" = "Funding critical to choice (4-5 of 5)",
    "grant_influenceTRUE"          = "Grant influenced enrolment"),
  gof_map = "nobs",
  title = "Odds of leaving, by entry funding dependence",
  notes = "Odds ratios, 95% CI (Wald) in brackets. Course and cohort fixed effects included where noted, not shown. Associational, not causal.",
  output = out_file
)
progress("done.")