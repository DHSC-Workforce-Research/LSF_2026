# ===========================================================================
# 08_leaving_regression.r  -  logistic models of leaving on entry funding
#   dependence, now CONTROLLING for financial confidence ("able to cover living
#   expenses next year", 1-5), the observed proxy for financial precarity.
#   All specs on one common sample so only the regressor set changes.
# ===========================================================================
library(dplyr); library(readr); library(stringr); library(purrr); library(broom); library(modelsummary)
purrr::walk(list.files("functions", full.names = TRUE), source)

fin_var <- "confidence"

progress("reading data ...")
long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)

progress("building sample + joining confidence ...")
sample <- build_funding_leaving_sample(long, traj)
# interim join (delete once folded into build_funding_leaving_sample)
conf_cw <- long |> filter(first_year == FALSE, !is.na(confidence)) |>
  arrange(UniqueID, year) |> group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |>
  select(UniqueID, confidence)
sample <- left_join(sample, conf_cw, by = "UniqueID")
stopifnot(fin_var %in% names(sample))

# same-sample base: only the regressor set changes across columns
base_cc <- sample |>
  filter(!is.na(.data[[fin_var]]), !is.na(funding_imp_crse), !is.na(grant_influence),
         !is.na(course), !is.na(entry_year))
progress("  n = ", nrow(base_cc), " (was ", nrow(sample), ")")

fe   <- "factor(course) + factor(entry_year)"
conf <- paste0("factor(", fin_var, ")")
f    <- function(lhs, rhs) as.formula(paste(lhs, "~", rhs))

progress("fitting ...")
m_raw  <- glm(f("considered_leaving", "I(funding_imp_crse >= 4) + grant_influence"),
              family = binomial, data = base_cc)
m_fe   <- glm(f("considered_leaving", paste("I(funding_imp_crse >= 4) + grant_influence +", fe)),
              family = binomial, data = base_cc)
m_conf <- glm(f("considered_leaving", paste("I(funding_imp_crse >= 4) + grant_influence +", fe, "+", conf)),
              family = binomial, data = base_cc)
b_fe   <- glm(f("left_early", paste("I(funding_imp_crse >= 4) + grant_influence +", fe)),
              family = binomial, data = base_cc)
b_conf <- glm(f("left_early", paste("I(funding_imp_crse >= 4) + grant_influence +", fe, "+", conf)),
              family = binomial, data = base_cc)

out_file <- file.path(outputs_dir(), "lsf_models.html")
modelsummary(
  list("Considered (raw)"             = m_raw,
       "Considered (+ FE)"            = m_fe,
       "Considered (+ FE + fin.conf)" = m_conf,
       "Left early (+ FE)"            = b_fe,
       "Left early (+ FE + fin.conf)" = b_conf),
  exponentiate = TRUE, statistic = "conf.int", ci_method = "wald",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c("I(funding_imp_crse >= 4)TRUE" = "Funding critical to choice (4-5 of 5)",
               "grant_influenceTRUE"          = "Grant influenced enrolment"),
  gof_map = "nobs",
  title = "Odds of leaving, before and after controlling for financial confidence",
  notes = paste0("Odds ratios, 95% Wald CI. One common sample (n = ", nrow(base_cc),
                 "). Course/cohort FE and confidence dummies included where noted, not shown."),
  output = out_file)
progress("done -> ", out_file)