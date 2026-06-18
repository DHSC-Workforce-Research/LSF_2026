# ===========================================================================
# 07_confidence_eda.r  -  FINANCIAL CONFIDENCE ("How confident are you about
#   being able to cover living expenses in the next year?", 1-5):
#   A. coverage by year   B. distribution   C. does it predict leaving
#   D. how it relates to funding awareness/dependence. Sets up the control in 08.
# ===========================================================================
library(dplyr); library(readr); library(tidyr); library(ggplot2); library(purrr)
library(broom); library(modelsummary)
purrr::walk(list.files("functions", full.names = TRUE), source)   # codebook + theme_dhsc

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
sample <- build_funding_leaving_sample(long, traj)

# join first continuing-wave confidence onto the entry-based sample (interim)
conf_cw <- long |>
  filter(first_year == FALSE, !is.na(confidence)) |>
  arrange(UniqueID, year) |> group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |>
  select(UniqueID, confidence)
sample <- left_join(sample, conf_cw, by = "UniqueID")

# recodes (used everywhere)
band <- function(x) factor(case_when(x <= 2 ~ "Not confident (1-2)",
                                     x == 3 ~ "Neutral (3)",
                                     x >= 4 ~ "Confident (4-5)"),
                           levels = c("Not confident (1-2)", "Neutral (3)", "Confident (4-5)"))
long   <- long   |> mutate(conf_band = band(confidence), not_confident = confidence <= 2)
sample <- sample |> mutate(conf_band = band(confidence), not_confident = confidence <= 2)

# --- A. COVERAGE: which years is it asked, how filled ----------------------
progress("A. coverage by year ...")
coverage <- long |>
  summarise(rows       = n(),
            continuing = sum(!first_year, na.rm = TRUE),
            answered   = sum(!is.na(confidence)),
            fill_all   = round(mean(!is.na(confidence)), 3),
            fill_cont  = round(sum(!is.na(confidence)) / sum(!first_year, na.rm = TRUE), 3),
            .by = year) |>
  arrange(year)
print(coverage)

# --- B. DISTRIBUTION overall + by year -------------------------------------
progress("B. distribution ...")
mean_year <- long |> filter(!is.na(confidence)) |>
  summarise(mean_conf = round(mean(confidence), 2),
            pct_not_conf = round(mean(confidence <= 2), 3), .by = year) |> arrange(year)
print(mean_year)

dist_year <- long |> filter(!is.na(confidence)) |>
  count(year, conf_band) |> group_by(year) |> mutate(pct = n / sum(n)) |> ungroup()

p_dist <- dist_year |>
  ggplot(aes(factor(year), pct, fill = conf_band)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Not confident (1-2)" = dhsc_cols[["risk"]],
                               "Neutral (3)"          = dhsc_cols[["midgrey"]],
                               "Confident (4-5)"      = dhsc_cols[["good"]])) +
  labs(title = "Financial confidence of continuing LSF students, by year",
       subtitle = "\"How confident are you about being able to cover living expenses in the next year?\" (1-5)",
       x = NULL, y = NULL, fill = NULL,
       caption = "Source: NHS Learning Support Fund longitudinal panel. DHSC analysis.") +
  theme_dhsc()
save_dhsc(p_dist, file.path(outputs_dir(), "lsf_confidence_by_year.png"), width = 9, height = 5)

# --- C. DOES CONFIDENCE PREDICT LEAVING? -----------------------------------
# both leave_course and confidence are continuing-wave, so model directly on long
progress("C. confidence -> considered leaving ...")
cont <- long |> filter(first_year == FALSE, !is.na(confidence), !is.na(leave_course))
c_raw <- glm(leave_course ~ confidence, family = binomial, data = cont)
c_fe  <- glm(leave_course ~ confidence + factor(course) + factor(year), family = binomial, data = cont)
c_bin <- glm(leave_course ~ not_confident + factor(course) + factor(year), family = binomial, data = cont)
modelsummary(
  list("Considered ~ confidence (raw)" = c_raw,
       "+ course & year FE"            = c_fe,
       "Not-confident (1-2) + FE"      = c_bin),
  exponentiate = TRUE, statistic = "conf.int", ci_method = "wald",
  coef_map = c("confidence"         = "Confidence (per +1 point)",
               "not_confidentTRUE"  = "Not confident (1-2 vs 3-5)"),
  gof_map = "nobs",
  title = "Does financial confidence predict considering leaving?",
  output = file.path(outputs_dir(), "lsf_confidence_predicts_leaving.html"))

# --- D. ARE LESS-CONFIDENT STUDENTS MORE AWARE / MORE FUNDING-DEPENDENT? ----
progress("D. confidence vs awareness/dependence ...")
dep_by_conf <- sample |>
  filter(!is.na(conf_band)) |>
  summarise(n              = n(),
            aware_pct      = round(mean(fund_availability, na.rm = TRUE), 3),  # awareness
            grant_infl_pct = round(mean(grant_influence,  na.rm = TRUE), 3),
            crit_what_pct  = round(mean(funding_imp_crse >= 4, na.rm = TRUE), 3),
            crit_where_pct = round(mean(funding_imp_uni  >= 4, na.rm = TRUE), 3),
            .by = conf_band) |>
  arrange(conf_band)
print(dep_by_conf)
write_csv(dep_by_conf, file.path(outputs_dir(), "lsf_dependence_by_confidence.csv"))
progress("done.")