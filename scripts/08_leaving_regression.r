# ===========================================================================
# 08_leaving_regression.r  -  logistic models of leaving on entry funding
#   dependence, with a side-by-side odds-ratio table. ASSOCIATIONAL not causal
#   (financial precarity, the real confounder, is unobserved; course/cohort
#   FEs remove composition only).
# ===========================================================================
library(dplyr); library(readr); library(stringr); library(purrr)
library(broom); library(modelsummary)
purrr::walk(list.files("functions", full.names = TRUE), source)

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)

sample <- build_funding_leaving_sample(long, traj)

model_raw       <- glm(considered_leaving ~ factor(funding_imp_crse) + grant_influence,
                       family = binomial, data = sample)
model_adjusted  <- glm(considered_leaving ~ factor(funding_imp_crse) + grant_influence +
                         factor(course) + factor(entry_year),
                       family = binomial, data = sample)
model_behaviour <- glm(left_early ~ factor(funding_imp_crse) + grant_influence +
                         factor(course) + factor(entry_year),
                       family = binomial, data = sample)

# fast Wald CIs for modelsummary, so it doesn't hang on profile likelihood with
# the ~70 course dummies. Kept inline (not in functions/) on purpose: it's a
# global override of how modelsummary tidies every glm, so it stays scoped here.
tidy_custom.glm <- function(x, ...) {
  s <- coef(summary(x))
  data.frame(term      = rownames(s),
             estimate  = s[, "Estimate"],
             conf.low  = s[, "Estimate"] - 1.96 * s[, "Std. Error"],
             conf.high = s[, "Estimate"] + 1.96 * s[, "Std. Error"],
             p.value   = s[, "Pr(>|z|)"])
}

modelsummary(
  list("Considered leaving (raw)"        = model_raw,
       "Considered leaving (+ controls)" = model_adjusted,
       "Left early (+ controls)"         = model_behaviour),
  exponentiate = TRUE, statistic = "conf.int",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c(
    "factor(funding_imp_crse)2" = "Funding importance 2",
    "factor(funding_imp_crse)3" = "Funding importance 3",
    "factor(funding_imp_crse)4" = "Funding importance 4",
    "factor(funding_imp_crse)5" = "Funding importance 5 (critical)",
    "grant_influenceTRUE"       = "Grant influenced enrolment"),
  gof_map = "nobs",
  title = "Odds of leaving, by entry funding dependence",
  notes = "Odds ratios, 95% CI in brackets. Course and cohort fixed effects included where noted, not shown. Associational, not causal.",
  output = file.path(outputs_dir(), "lsf_models.html")
)