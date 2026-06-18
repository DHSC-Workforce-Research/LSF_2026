# ===========================================================================
# 10_communicate.r  -  presentation outputs: (1) exponentiated regression table
#   of the focal predictor across leaving definitions, (2) DHSC-styled forest
#   plot across every predictor x definition. ASSOCIATIONAL, not causal.
# ===========================================================================
# install.packages("ggplot2")  # if needed
library(dplyr); library(readr); library(stringr); library(purrr)
library(broom); library(modelsummary); library(ggplot2)
purrr::walk(list.files("functions", full.names = TRUE), source)

progress("building sample ...")
long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
sample <- build_funding_leaving_sample(long, traj) |> define_leaving_outcomes()

# --- (1) REGRESSION TABLE: focal predictor across leaving definitions -------
focal       <- "I(funding_imp_crse >= 4)"
tab_outcomes <- c(reached_finish     = "Reached finish",
                  left_before_finish = "Left before finish",
                  left_2y_plus_early = "Left 2y+ early",
                  left_after_year1   = "Dropped after yr1",
                  one_wave_only      = "Claimed once only",
                  considered_first   = "Considered leaving")
tidy_custom.glm <- function(x, ...) {            # fast Wald CIs
  s <- coef(summary(x))
  data.frame(term = rownames(s), estimate = s[,"Estimate"],
             conf.low = s[,"Estimate"] - 1.96*s[,"Std. Error"],
             conf.high = s[,"Estimate"] + 1.96*s[,"Std. Error"], p.value = s[,"Pr(>|z|)"])
}
progress("fitting focal-predictor models ...")
tab_models <- imap(tab_outcomes, \(lab, oc) {
  progress("  ", oc)
  glm(as.formula(paste0(oc, " ~ ", focal, " + factor(course) + factor(entry_year)")),
      family = binomial, data = sample)
}) |> set_names(tab_outcomes)

modelsummary(tab_models, exponentiate = TRUE, statistic = "conf.int",
  stars = c("*"=.05,"**"=.01,"***"=.001),
  coef_map = c("I(funding_imp_crse >= 4)TRUE" = "Funding critical to course (4-5 of 5)"),
  gof_map = "nobs",
  title = "Funding critical to course choice, across definitions of leaving (course + cohort fixed effects)",
  notes = "Odds ratios, 95% CI (Wald) in brackets. Associational, not causal.",
  output = file.path(outputs_dir(), "lsf_focal_table.html"))

# --- (2) FOREST PLOT across every predictor x definition --------------------
progress("drawing forest plot ...")
grid <- read_csv(file.path(outputs_dir(), "lsf_robustness_grid_long.csv"), show_col_types = FALSE)

lab_out <- c(reached_finish="Reached finish (completed)", left_before_finish="Left before finish",
             left_final_year_only="Stopped in final year only", left_2y_plus_early="Left 2y+ early",
             left_after_year1="Dropped after year 1", one_wave_only="Claimed once, never again",
             considered_first="Considered leaving (first wave)", considered_ever="Considered leaving (ever)")
lab_pred <- c(fund_availability="Aware of grant before applying", grant_influence="Grant influenced enrolment",
              "I(funding_imp_crse >= 4)"="Funding critical to WHAT to study",
              "I(funding_imp_uni >= 4)"="Funding critical to WHERE to study",
              grant_helps_stay="Grant helps me stay")

plot_df <- grid |>
  filter(outcome %in% names(lab_out), predictor %in% names(lab_pred)) |>
  mutate(outcome   = factor(outcome,   levels = rev(names(lab_out)),  labels = rev(lab_out)),
         predictor = factor(predictor, levels = names(lab_pred),      labels = lab_pred),
         dir = if_else(OR >= 1, "More likely to leave", "Less likely (protective)"))

p <- ggplot(plot_df, aes(OR, outcome, colour = dir)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .22, linewidth = .5) +
  geom_point(size = 2.2) +
  scale_x_log10() +
  scale_colour_manual(values = c("More likely to leave" = "#D4351C",
                                 "Less likely (protective)" = "#01A188")) +
  facet_wrap(~predictor, ncol = 1) +
  labs(title = "Funding dependence and leaving, across every definition of leaving",
       subtitle = "Odds ratios (log scale) with 95% CI. Course and cohort fixed effects. Associational, not causal.",
       x = "Odds ratio  (right of 1 = more likely to leave)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", hjust = 0))

ggsave(file.path(outputs_dir(), "lsf_forest.png"), p, width = 8.5, height = 13, dpi = 200)
progress("done -> ", outputs_dir())