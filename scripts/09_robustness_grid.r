# ===========================================================================
# 09_robustness_grid.r  -  cross every funding predictor with every leaving
#   definition (logistic, course + cohort FE), pull each predictor's odds ratio,
#   and lay them in one robustness matrix. ASSOCIATIONAL not causal.
# ===========================================================================
library(dplyr); library(readr); library(stringr); library(purrr); library(tidyr); library(broom)
purrr::walk(list.files("functions", full.names = TRUE), source)

progress("reading data ...")
long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)

progress("building sample + outcome flags ...")
sample <- build_funding_leaving_sample(long, traj) |> define_leaving_outcomes()

predictors <- c("fund_availability", "grant_influence",
                "I(funding_imp_crse >= 4)", "I(funding_imp_uni >= 4)", "grant_helps_stay")

outcomes <- c("reached_finish", "left_before_finish", "left_final_year_only",
              "left_2y_plus_early", "left_3y_plus_early", "left_after_year1",
              "one_wave_only", "considered_first", "considered_ever",
              "considered_and_left", "considered_but_stayed")

fit_one <- function(pred, outcome) {
  tryCatch({
    f <- as.formula(paste0(outcome, " ~ ", pred, " + factor(course) + factor(entry_year)"))
    m <- glm(f, family = binomial, data = sample)
    term <- paste0(pred, "TRUE")
    co <- coef(summary(m))
    if (!term %in% rownames(co)) return(NULL)
    est <- co[term, "Estimate"]; se <- co[term, "Std. Error"]
    tibble(predictor = pred, outcome = outcome,
           OR = exp(est), lo = exp(est - 1.96*se), hi = exp(est + 1.96*se),
           p = co[term, "Pr(>|z|)"], n = stats::nobs(m))
  }, error = function(e) NULL)
}

progress("fitting ", length(predictors) * length(outcomes), " models ...")
grid <- tidyr::expand_grid(pred = predictors, outcome = outcomes) |>
  purrr::pmap(\(pred, outcome) { progress("  ", pred, " ~ ", outcome); fit_one(pred, outcome) }) |>
  purrr::list_rbind()

matrix_tbl <- grid |>
  mutate(cell = sprintf("%.2f [%.2f, %.2f]", OR, lo, hi)) |>
  select(predictor, outcome, cell) |>
  pivot_wider(names_from = outcome, values_from = cell)

print(matrix_tbl, width = Inf)
write_csv(grid,       file.path(outputs_dir(), "lsf_robustness_grid_long.csv"))
write_csv(matrix_tbl, file.path(outputs_dir(), "lsf_robustness_grid_matrix.csv"))
progress("done -> ", outputs_dir())