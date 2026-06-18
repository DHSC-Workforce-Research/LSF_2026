# ===========================================================================
# 10_communicate.r  -  presentation outputs: (1) exponentiated regression table
#   of the focal predictor across leaving definitions, (2) DHSC-styled forest
#   plot across every predictor x definition. 
# ===========================================================================
# install.packages("ggplot2")  # if needed
library(dplyr); library(readr); library(stringr); library(purrr)
library(broom); library(modelsummary); library(ggplot2)
purrr::walk(list.files("functions", full.names = TRUE), source)

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
  ci_method = "wald",                       # closed-form CIs, skips the slow profiling
  stars = c("*"=.05,"**"=.01,"***"=.001),
  coef_map = c("I(funding_imp_crse >= 4)TRUE" = "Funding critical to course (4-5 of 5)"),
  gof_map = "nobs",
  title = "Funding critical to course choice, across definitions of leaving (course + cohort fixed effects)",
  notes = "Odds ratios, 95% CI (Wald) in brackets. Associational, not causal.",
  output = file.path(outputs_dir(), "lsf_focal_table.html"))

# --- (2) FOREST PLOTS: headline + full grid, coloured by significance -------
progress("drawing forest plots ...")
grid <- read_csv(file.path(outputs_dir(), "lsf_robustness_grid_long.csv"), show_col_types = FALSE)

headline_outcome <- "left_before_finish"   # leading definition (change title below if you change this)

# explicit OR breaks so the >1 side actually gets labelled on the log scale
or_breaks <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5, 2.0)

# shared text
spec_note <- "Logistic regression: leaving ~ predictor + course fixed effects + cohort fixed effects. Odds ratios, 95% Wald CI."
def_text  <- "Defined as leaving the course before reaching its expected final wave (did not complete)."
src       <- "Source: NHS Learning Support Fund longitudinal panel 2020-2026. DHSC analysis."

sev_order <- c("considered_ever","considered_first","one_wave_only",
               "left_after_year1","left_2y_plus_early","left_final_year_only","left_before_finish")
lab_out <- c(left_before_finish="Left before finish (any)", left_final_year_only="Stopped in final year",
             left_2y_plus_early="Left 2+ years early", left_after_year1="Dropped after year 1",
             one_wave_only="Claimed once, never again", considered_first="Considered leaving (first wave)",
             considered_ever="Considered leaving (ever)")
lab_pred <- c(fund_availability="Aware of grant before applying", grant_influence="Grant influenced enrolment",
              "I(funding_imp_crse >= 4)"="Funding critical to WHAT to study",
              "I(funding_imp_uni >= 4)"="Funding critical to WHERE to study",
              grant_helps_stay="Grant helps me stay")

sig_levels <- c("More likely to leave", "Less likely (protective)", "Not significant")
sig_cols <- c("More likely to leave"      = unname(dhsc_cols[["risk"]]),
              "Less likely (protective)"  = unname(dhsc_cols[["good"]]),
              "Not significant"           = unname(dhsc_cols[["midgrey"]]))
flag_sig <- function(df) df |>
  filter(is.finite(OR), is.finite(lo), is.finite(hi)) |>
  mutate(flag = factor(case_when(!(lo > 1 | hi < 1) ~ "Not significant",
                                 OR >= 1            ~ "More likely to leave",
                                 TRUE               ~ "Less likely (protective)"),
                       levels = sig_levels))

# (2a) HEADLINE ---------------------------------------------------------------
head_df <- grid |>
  filter(outcome == headline_outcome, predictor %in% names(lab_pred)) |>
  mutate(predictor = factor(predictor, levels = rev(names(lab_pred)), labels = rev(lab_pred))) |>
  flag_sig()

p_head <- ggplot(head_df, aes(OR, predictor, colour = flag)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dhsc_cols[["midgrey"]]) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .6) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", OR)), colour = dhsc_cols[["ink"]],
            vjust = -1, size = 3, show.legend = FALSE) +
  scale_x_log10(breaks = or_breaks, expand = expansion(mult = c(0.05, 0.10))) +
  scale_colour_manual(values = sig_cols, drop = FALSE) +
  labs(title    = "What predicts student non-completion?",
       subtitle = def_text,
       x = "Odds ratio  (right of 1 = more likely to leave)", y = NULL, colour = NULL,
       caption = paste(spec_note, src, sep = "\n")) +
  theme_dhsc()
save_dhsc(p_head, file.path(outputs_dir(), "lsf_forest_headline.png"), width = 9, height = 5)

# (2b) FULL GRID --------------------------------------------------------------
grid_df <- grid |>
  filter(outcome %in% sev_order, predictor %in% names(lab_pred)) |>
  mutate(outcome   = factor(outcome,   levels = sev_order,            labels = lab_out[sev_order]),
         predictor = factor(predictor, levels = rev(names(lab_pred)), labels = rev(lab_pred))) |>
  flag_sig()

xr <- range(c(grid_df$lo, grid_df$hi), na.rm = TRUE)

p_grid <- ggplot(grid_df, aes(OR, predictor, colour = flag)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dhsc_cols[["midgrey"]]) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25, linewidth = .5) +
  geom_point(size = 2.2) +
  scale_x_log10(limits = xr, breaks = or_breaks) +
  scale_colour_manual(values = sig_cols, drop = FALSE) +
  facet_wrap(~outcome, ncol = 4, scales = "free_x") +
  labs(title    = "How funding-dependence factors relate to each definition of leaving",
       subtitle = paste("Each panel is a different definition of leaving.", spec_note),
       x = "Odds ratio  (right of 1 = more likely to leave)", y = NULL, colour = NULL,
       caption = src) +
  theme_dhsc() +
  theme(panel.spacing = grid::unit(1, "lines"))
save_dhsc(p_grid, file.path(outputs_dir(), "lsf_forest_grid.png"), width = 13, height = 7.5)
progress("done -> ", outputs_dir())