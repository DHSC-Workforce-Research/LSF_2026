# =====================================================================
# 03_visualise.R
# Tidy result tables (from 02)  ->  DHSC widescreen slides (PNG).
# Reads tbl_*.csv from outputs, writes slide_*.png back. Pure presentation.
# Title = plain-English finding; subtitle = technical definition of the metric.
# Run:  source("scripts/03_visualise.R")
# =====================================================================
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr); library(readr); library(ggplot2); library(tidyr); library(stringr)
out <- outputs_dir()
rd  <- function(f) read_csv(file.path(out, f), show_col_types = FALSE)

risk <- dcol("risk", "#D4351C"); teal <- dcol("af_teal", "#28A197")
grey <- dcol("midgrey", "#6F777B"); ink <- dcol("ink", "#0B0C0C")
src  <- "Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis."
pred_levels <- rev(c("Aware of grant before applying", "Grant influenced enrolment",
                     "Grant helps me stay", "Funding critical to WHERE to study",
                     "Funding critical to WHAT to study"))

# --- slide 1: AUC ----------------------------------------------------
dec <- rd("tbl_auc_decile.csv")
sm  <- rd("tbl_auc_summary.csv") |> filter(outcome == "left_before_finish")
base <- sm$base_rate * 100; a <- sm$auc_survey
dec <- dec |> mutate(top = decile == max(decile))
lab_auc <- sprintf(paste0("What is AUC? %.2f here.\n",
  "Pick one student who left and one who didn't, at random.\n",
  "The model rates the leaver as higher-risk just %.0f%% of the\n",
  "time. A coin toss is 50%%; a useful test scores 70%% or more."), a, 100 * a)
p1 <- ggplot(dec, aes(decile, leave_rate)) +
  geom_hline(yintercept = base, linetype = "dashed", colour = grey, linewidth = .6) +
  geom_col(aes(fill = top), width = .78, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", leave_rate)), vjust = -0.6, size = 4.2, colour = ink) +
  annotate("label", x = 0.55, y = 58, hjust = 0, vjust = 1, label = lab_auc,
           fill = dcol("gridgrey", "#E6E6E6"), colour = ink, label.size = 0, size = 4.1, lineheight = 1.03) +
  annotate("text", x = 0.6, y = base + 2.2, hjust = 0, label = sprintf("Average, %.0f%%", base), colour = grey, size = 4.2) +
  scale_fill_manual(values = c(`FALSE` = teal, `TRUE` = dcol("af_orange", "#F46A25"))) +
  scale_x_continuous(breaks = 1:10, labels = c("Lowest\npredicted\nrisk", 2:9, "Highest\npredicted\nrisk"), expand = expansion(add = .6)) +
  scale_y_continuous(limits = c(0, 62), breaks = seq(0, 50, 10), labels = \(z) paste0(z, "%")) +
  labs(title = "We cannot predict which individual students will leave",
       subtitle = "Students split into ten equal groups by a model's predicted risk of leaving, using all five entry funding answers. Bars show\nthe share in each group who actually left before finishing (n = 128,109).",
       x = "Students ranked by the model's predicted risk of leaving (lowest to highest)",
       y = "Share who left before finishing",
       caption = paste("Logistic model of the five entry funding items.", src)) +
  theme_dhsc_slide(base = 15) + theme(panel.grid.major.x = element_blank(), plot.caption = element_text(lineheight = 1.15), plot.margin = margin(16, 22, 12, 12))
save_slide(p1, file.path(out, "slide_auc.png"))

# --- slide 2: factors ranked ----------------------------------------
fac <- rd("tbl_factors.csv") |> mutate(direction = ifelse(OR >= 1, "More likely to leave", "Less likely to leave")) |> arrange(OR)
fac$factor <- factor(fac$factor, levels = fac$factor)
p2 <- ggplot(fac, aes(OR, factor, colour = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = grey) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25, linewidth = .5) +
  geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1.1, size = 3.6, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0.8, 1.3, 0.1)) +
  scale_colour_manual(values = c("More likely to leave" = risk, "Less likely to leave" = teal)) +
  labs(title = "No single factor strongly predicts who leaves",
       subtitle = "Adjusted odds of leaving before finishing for students with each characteristic versus those without, holding course and\ncohort equal. 1.0 = no difference; bars are 95% confidence intervals (narrow because the sample is very large).",
       x = "Odds of leaving before finishing (1.0 = no difference)", y = NULL, colour = NULL,
       caption = paste("Single-factor logistic models, course and cohort fixed effects.", src)) +
  theme_dhsc_slide(base = 15) + theme(legend.position = "top")
save_slide(p2, file.path(out, "slide_factors.png"))

# --- slide 3: worry vs behaviour ------------------------------------
grid <- rd("tbl_or_grid.csv") |>
  filter(outcome %in% c("Considered leaving", "Left before finishing")) |>
  mutate(direction = ifelse(OR >= 1, "More likely", "Less likely"),
         outcome   = factor(outcome, levels = c("Considered leaving", "Left before finishing")),
         predictor = factor(predictor, levels = pred_levels))
p3 <- ggplot(grid, aes(OR, predictor, colour = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = grey) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .5) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1, size = 3.3, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0.8, 1.8, 0.2)) +
  scale_colour_manual(values = c("More likely" = risk, "Less likely" = teal)) +
  facet_wrap(~outcome) +
  labs(title = "Relying on the grant predicts worry, not leaving",
       subtitle = "Odds of considering leaving, and of actually leaving before finishing, for students giving each answer versus those who\ndid not. Same predictors, two outcomes.",
       x = "Odds ratio (1.0 = no difference)", y = NULL, colour = NULL,
       caption = paste("Single-predictor logistic models, course and cohort fixed effects.", src)) +
  theme_dhsc_slide(base = 15) + theme(legend.position = "top")
save_slide(p3, file.path(out, "slide_or_forest.png"))

# --- slide 4: survivorship ------------------------------------------
surv <- rd("tbl_survivorship.csv") |>
  mutate(spec = factor(spec, levels = c("All students", "Reached year 2", "Year 2 + financial confidence")),
         predictor = factor(predictor, levels = pred_levels))
p4 <- ggplot(surv, aes(OR, predictor, colour = spec)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = grey) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .45, position = position_dodge(width = .6)) +
  geom_point(size = 2.8, position = position_dodge(width = .6)) +
  scale_x_continuous(breaks = seq(0.8, 1.5, 0.1)) +
  scale_colour_manual(values = c("All students" = grey, "Reached year 2" = dcol("af_orange", "#F46A25"), "Year 2 + financial confidence" = dcol("dhsc_blue", "#0063BE"))) +
  labs(title = "Knowing about the grant beforehand only helps in the first year",
       subtitle = "Odds of leaving before finishing: all students, then only those who reach year 2, then adjusted for financial confidence.\nAwareness (top) is protective overall but null once students reach year 2.",
       x = "Odds ratio (1.0 = no difference)", y = NULL, colour = NULL,
       caption = paste("Logistic models, course and cohort fixed effects.", src)) +
  theme_dhsc_slide(base = 15) + theme(legend.position = "top")
save_slide(p4, file.path(out, "slide_survivorship.png"))

# --- slide 5: group comparisons -------------------------------------
gr <- rd("tbl_group_rates.csv")
overall <- gr |> filter(comparison == "Overall") |> pull(leave_rate)
gr2 <- gr |> filter(comparison != "Overall") |>
  mutate(group = recode(group, "TRUE" = "Yes", "FALSE" = "No"),
         group = factor(group, levels = c("Yes", "No", "Low (1-2)", "Mid (3)", "High (4-5)")),
         comparison = factor(comparison, levels = c("Aware of grant beforehand", "Has children", "Funding critical to course", "Financial confidence")))
p5 <- ggplot(gr2, aes(leave_rate, group, fill = comparison)) +
  geom_vline(xintercept = overall, linetype = "dashed", colour = grey) +
  geom_col(width = .62, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", leave_rate)), hjust = -0.25, size = 4, colour = ink) +
  facet_grid(comparison ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 46), breaks = seq(0, 40, 10), labels = \(z) paste0(z, "%")) +
  scale_fill_manual(values = c(teal, dcol("af_orange", "#F46A25"), dcol("af_blue", "#12436D"), dcol("af_purple", "#A285D1"))) +
  labs(title = "Some groups leave more than others, but the gaps are small",
       subtitle = sprintf("Share of each group who left before finishing. Dashed line is the %.0f%% overall average.", overall),
       x = "Share who left before finishing", y = NULL,
       caption = src) +
  theme_dhsc_slide(base = 15) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"), panel.grid.major.y = element_blank())
save_slide(p5, file.path(out, "slide_groups.png"))

# --- slide 6: exit breakdown ----------------------------------------
ex <- rd("tbl_exits.csv") |>
  mutate(label = recode(outcome_cautious,
                        active = "Still in progress",
                        no_entry_observed = "First year never surveyed",
                        dropped_out = "Dropped out",
                        completed = "Completed",
                        censored = "Would finish beyond the data"),
         judgeable = ifelse(outcome_cautious %in% c("completed", "dropped_out"), "Can classify", "Cannot classify")) |>
  arrange(share)
ex$label <- factor(ex$label, levels = ex$label)
y_ne <- which(levels(ex$label) == "First year never surveyed")   # bar to annotate
p6 <- ggplot(ex, aes(share, label, fill = judgeable)) +
  geom_col(width = .68) +
  geom_text(aes(label = sprintf("%.0f%%", share)), hjust = -0.2, size = 4.2, colour = ink) +
  annotate("text", x = 51, y = y_ne, hjust = 1, vjust = 0.5, size = 3.5, colour = grey, lineheight = 0.95,
           label = "Mostly the 2020 intake: the survey\nwasn't at full scale yet, so their\nfirst year was never recorded") +
  annotate("curve", x = 33, xend = 24, y = y_ne, yend = y_ne, curvature = 0,
           colour = grey, linewidth = .5, arrow = grid::arrow(length = grid::unit(2.4, "mm"))) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(0, 50, 10), labels = \(z) paste0(z, "%")) +
  scale_fill_manual(values = c("Can classify" = teal, "Cannot classify" = grey)) +
  labs(title = "We can only see the full journey for about a third of students",
       subtitle = "Share of all 290,947 LSF claimants in each course-outcome category, 2020 to 2026.",
       x = "Share of all claimants", y = NULL, fill = NULL,
       caption = paste("Course-length-anchored classification. Leaving analysis (n = 222,174) uses only students whose first year was observed,",
                       "effectively the 2021-onwards intakes.", src)) +
  theme_dhsc_slide(base = 15) + theme(legend.position = "top", panel.grid.major.y = element_blank(), plot.caption = element_text(lineheight = 1.15))
save_slide(p6, file.path(out, "slide_exits.png"))

progress("done. slides written to ", out)