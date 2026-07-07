# =====================================================================
# 03_visualise.R  -  tidy tables (from 02) -> DHSC slides (PNG).
# Finding text (titles, subtitles, AUC box) is read from slide_labels.json
# in the secure data folder, NOT stored here. Missing file => no titles.
# Run after 01, 02.
# =====================================================================
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr); library(readr); library(ggplot2); library(tidyr); library(stringr)
out <- outputs_dir()
rd  <- function(f) read_csv(file.path(out, f), show_col_types = FALSE)
wrapcap <- function(x, w = 135) str_wrap(x, w)
risk <- dcol("risk", "#D4351C"); teal <- dcol("af_teal", "#28A197")
orange <- dcol("af_orange", "#F46A25"); grey <- dcol("midgrey", "#6F777B"); ink <- dcol("ink", "#0B0C0C")
src <- "Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis."
capt_theme <- theme(plot.caption = element_text(lineheight = 1.15))
pred_levels <- rev(c("Aware of grant before applying", "Grant influenced enrolment",
                     "Grant helps me stay", "Funding critical to WHERE to study", "Funding critical to WHAT to study"))

# labels live with the data, not the code; missing file -> blank titles
lbl <- local({
  path <- file.path(derived_dir(), "slide_labels.json")
  L <- if (requireNamespace("jsonlite", quietly = TRUE) && file.exists(path)) jsonlite::fromJSON(path) else list()
  if (!length(L)) message("NOTE: slide_labels.json not found or unreadable; slides render without titles.")
  function(slide, field, default = "") {
    v <- tryCatch(L[[slide]][[field]], error = function(e) NULL)
    if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) default else v
  }
})

# --- slide 0: when each question is asked (survey structure) ---------
blocks_lv <- c("Asked at entry (Year 1)", "Asked each continuing year (Year 2 on)", "Observed every year")
measures <- tibble::tribble(
  ~block,       ~measure,                                          ~from, ~to,
  blocks_lv[1], "Aware of the grant beforehand",                       1, 1,
  blocks_lv[1], "Grant influenced enrolment",                          1, 1,
  blocks_lv[1], "Funding critical to course or university",            1, 1,
  blocks_lv[1], "Grant components applied for (parental, etc.)",       1, 1,
  blocks_lv[2], "Financial confidence",                                2, 4,
  blocks_lv[2], "Considered leaving this year",                        2, 4,
  blocks_lv[2], "Expects to finish on time",                           2, 4,
  blocks_lv[3], "Still claiming (our leaving measure)",                1, 4)
mlev <- rev(measures$measure)
qz <- measures |> rowwise() |> mutate(year = list(seq(from, to))) |> tidyr::unnest(year) |> ungroup() |>
  mutate(measure = factor(measure, levels = mlev), block = factor(block, levels = blocks_lv))
p0 <- ggplot(qz, aes(year, measure, fill = block)) +
  geom_tile(width = .92, height = .72, colour = "white", linewidth = 1.2) +
  scale_x_continuous(breaks = 1:4, labels = paste("Year", 1:4), position = "top", limits = c(0.5, 4.5), expand = c(0, 0)) +
  scale_fill_manual(values = setNames(c(teal, orange, dcol("af_blue", "#12436D")), blocks_lv)) +
  labs(title = "Key measures start too late to catch first-year leavers",
       subtitle = "The questionnaire changes after Year 1. Anything measured only from Year 2 (financial confidence, considered leaving) can\nonly be analysed on students who reached Year 2, so any model using it drops everyone who left in the first year.",
       x = NULL, y = NULL, fill = NULL,
       caption = wrapcap("Year 4 applies to 4-year courses only, and final-year answers cannot be checked against a later drop. Source: LSF questionnaire structure.")) +
  theme_dhsc_slide(base = 15) + capt_theme +
  theme(legend.position = "top", panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 0), axis.ticks = element_blank())
save_slide(p0, file.path(out, "slide_question_timing.png"))

# --- slide 1: AUC ----------------------------------------------------
dec <- rd("tbl_auc_decile.csv"); sm <- rd("tbl_auc_summary.csv") |> filter(outcome == "left_before_finish")
base <- sm$base_rate * 100; a <- sm$auc_survey; dec <- dec |> mutate(top = decile == max(decile))
p1 <- ggplot(dec, aes(decile, leave_rate)) +
  geom_hline(yintercept = base, linetype = "dashed", colour = grey, linewidth = .6) +
  geom_col(aes(fill = top), width = .78, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", leave_rate)), vjust = -0.6, size = 4.2, colour = ink) +
  annotate("text", x = 0.6, y = base + 2.2, hjust = 0, label = sprintf("Overall average, %.0f%%", base), colour = grey, size = 4.2) +
  scale_fill_manual(values = c(`FALSE` = teal, `TRUE` = orange)) +
  scale_x_continuous(breaks = 1:10, labels = c("Lowest\npredicted\nrisk", 2:9, "Highest\npredicted\nrisk"), expand = expansion(add = .6)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = \(z) paste0(z, "%")) +
  labs(title = lbl("auc","title"), subtitle = lbl("auc","subtitle"),
       x = "Students ranked by the model's predicted risk of leaving (lowest to highest)", y = "Share who left before finishing",
       caption = wrapcap(paste("Logistic model of the five entry funding items.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme + theme(panel.grid.major.x = element_blank(), plot.margin = margin(16, 22, 12, 12))
box_txt <- lbl("auc","box")
box_txt <- gsub("{auc}",    sprintf("%.2f", a),       box_txt, fixed = TRUE)
box_txt <- gsub("{aucpct}", sprintf("%.0f", 100 * a), box_txt, fixed = TRUE)
if (nzchar(box_txt)) p1 <- p1 + annotate("label", x = 0.55, y = 96, hjust = 0, vjust = 1, label = box_txt,
                                         fill = dcol("gridgrey", "#E6E6E6"), colour = ink, label.size = 0, size = 4.1, lineheight = 1.03)
save_slide(p1, file.path(out, "slide_auc.png"))

# --- slide 1b: model performance as a clean confusion table --------
cf <- rd("tbl_confusion.csv")
Lv <- round(100 * cf$base_rate)                       # leave per 100 = flagged per 100 (prevalence matched)
TP <- round(cf$sensitivity * Lv); FN <- Lv - TP; FP <- FN; TN <- 100 - TP - FN - FP
sens <- TP / Lv; spec <- TN / (TN + FP); prec <- TP / (TP + FP); acc <- (TP + TN) / 100
naive <- max(Lv, 100 - Lv)
head_fill <- dcol("dhsc_teal", "#01A188"); tint_ok <- "#D9ECE9"; tint_bad <- "#F7DAD4"; tint_tot <- "#EFEFEF"

# confusion matrix (rows = actual, columns = predicted), laid out as a branded table
cx <- c(1.9, 4.5, 6.4, 8.3); cw <- c(2.9, 1.85, 1.85, 1.85); ry <- 8.4 - (0:3) * 1.28
mkcell <- function(ci, rj, txt, fill, tcol, face, sz)
  tibble(x = cx[ci], y = ry[rj], w = cw[ci], txt = txt, fill = fill, tcol = tcol, face = face, sz = sz)
cm <- dplyr::bind_rows(
  mkcell(1,1,"", head_fill,"white","bold",4.4), mkcell(2,1,"Predicted\nto leave", head_fill,"white","bold",4.2),
  mkcell(3,1,"Predicted\nto stay", head_fill,"white","bold",4.2), mkcell(4,1,"Total", head_fill,"white","bold",4.2),
  mkcell(1,2,"Actually left", head_fill,"white","bold",4.0), mkcell(1,3,"Actually stayed", head_fill,"white","bold",4.0),
  mkcell(1,4,"Total", head_fill,"white","bold",4.0),
  mkcell(2,2,sprintf("%d",TP), tint_ok, ink,"bold",6.4), mkcell(3,2,sprintf("%d",FN), tint_bad, ink,"bold",6.4),
  mkcell(4,2,sprintf("%d",Lv), tint_tot, ink,"plain",5.2),
  mkcell(2,3,sprintf("%d",FP), tint_bad, ink,"bold",6.4), mkcell(3,3,sprintf("%d",TN), tint_ok, ink,"bold",6.4),
  mkcell(4,3,sprintf("%d",100-Lv), tint_tot, ink,"plain",5.2),
  mkcell(2,4,sprintf("%d",Lv), tint_tot, ink,"plain",5.2), mkcell(3,4,sprintf("%d",100-Lv), tint_tot, ink,"plain",5.2),
  mkcell(4,4,"100", tint_tot, ink,"bold",5.2))

# metrics panel (right)
mx_l <- 10.4; mx_r <- 18.8
mrows <- tibble::tribble(
  ~lab, ~val,
  "Sensitivity (leavers caught)",         sprintf("%.0f%%", 100 * sens),
  "Precision (flags that were correct)",  sprintf("%.0f%%", 100 * prec),
  "Specificity (stayers cleared)",        sprintf("%.0f%%", 100 * spec),
  "Model accuracy",                       sprintf("%.0f%%", 100 * acc),
  "Accuracy of naive guess ('no-one leaves')",      sprintf("%.0f%%", naive),
  "Type I errors (false alarms)",         sprintf("%d", FP),
  "Type II errors (missed leavers)",      sprintf("%d", FN),
  "AUC (0.50 = chance, 1.00 = perfect)",  sprintf("%.2f", cf$auc)) |>
  mutate(y = seq(7.0, by = -0.82, length.out = 8))
zeb <- mrows |> mutate(i = dplyr::row_number()) |> filter(i %% 2 == 1)

p1b <- ggplot(cm, aes(x, y)) +
  geom_tile(aes(width = w, height = 1.18, fill = fill), colour = "white", linewidth = 1.8) +
  scale_fill_identity() +
  geom_text(aes(label = txt, colour = tcol, fontface = face, size = sz), lineheight = .92) +
  scale_colour_identity() + scale_size_identity() +
  annotate("rect", xmin = mx_l - 0.4, xmax = mx_r + 0.4, ymin = 7.9, ymax = 8.65, fill = head_fill) +
  annotate("text", x = mx_l - 0.1, y = 8.27, hjust = 0, colour = "white", fontface = "bold", size = 4.6,
           label = "Model performance, per 100 students") +
  geom_rect(data = zeb, aes(xmin = mx_l - 0.4, xmax = mx_r + 0.4, ymin = y - 0.41, ymax = y + 0.41),
            fill = "#F4F4F4", inherit.aes = FALSE) +
  geom_text(data = mrows, aes(x = mx_l - 0.1, y = y, label = lab), hjust = 0, size = 4.2, colour = ink, inherit.aes = FALSE) +
  geom_text(data = mrows, aes(x = mx_r + 0.1, y = y, label = val), hjust = 1, size = 4.5, fontface = "bold", colour = ink, inherit.aes = FALSE) +
  annotate("text", x = 0.5, y = 3.35, hjust = 0, vjust = 1, size = 3.6, colour = grey, lineheight = 1.2,
           label = sprintf("Based on all %s students, of whom about %s actually left.\nShown per 100 for readability; the model flags as many as leave.",
                           format(cf$n, big.mark = ","), format(round(cf$n * cf$base_rate), big.mark = ","))) +
  scale_x_continuous(limits = c(0.3, 19.3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.6, 9.2), expand = c(0, 0)) +
  labs(title = lbl("confusion", "title", "We can't accurately predict who leaves based on their survey responses."),
       subtitle = lbl("confusion", "subtitle", "The model scores every student's risk of leaving, then flags the highest-risk, as many as the number who actually left. Cells are scaled\nto 100 students."),
       caption = wrapcap(paste("Logistic model of the five entry funding items plus cohort, prevalence-matched threshold. Type I and Type II errors are equal because we flag as many students as leave.", src))) +
  theme_void(base_size = 15) +
  theme(plot.title = element_text(face = "bold", size = 22, colour = ink, margin = margin(b = 4)),
        plot.subtitle = element_text(size = 15, colour = "grey30", margin = margin(b = 12)),
        plot.caption = element_text(size = 11, colour = "grey45", hjust = 0),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.margin = margin(16, 20, 12, 18))
save_slide(p1b, file.path(out, "slide_confusion.png"))

# --- slide 2: factors ----------------------------------------------
fac <- rd("tbl_factors.csv") |> mutate(direction = ifelse(OR >= 1, "More likely to leave", "Less likely to leave")) |> arrange(OR)
fac$factor <- factor(fac$factor, levels = fac$factor)
p2 <- ggplot(fac, aes(OR, factor, colour = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = grey) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25, linewidth = .5) + geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1.1, size = 3.6, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0.8, 1.3, 0.1)) +
  scale_colour_manual(values = c("More likely to leave" = risk, "Less likely to leave" = teal)) +
  labs(title = lbl("factors","title"), subtitle = lbl("factors","subtitle"),
       x = "Odds of leaving before finishing (1.0 = no difference)", y = NULL, colour = NULL,
       caption = wrapcap(paste("Single-factor logistic models, course and cohort fixed effects.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme + theme(legend.position = "top")
save_slide(p2, file.path(out, "slide_factors.png"))

# --- slide 3: worry vs behaviour -----------------------------------
grid <- rd("tbl_or_grid.csv") |> filter(outcome %in% c("Considered leaving", "Left before finishing")) |>
  mutate(direction = ifelse(OR >= 1, "More likely", "Less likely"),
         outcome = factor(outcome, levels = c("Considered leaving", "Left before finishing")), predictor = factor(predictor, levels = pred_levels))
p3 <- ggplot(grid, aes(OR, predictor, colour = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = grey) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .5) + geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1, size = 3.3, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0.8, 1.8, 0.2)) + scale_colour_manual(values = c("More likely" = risk, "Less likely" = teal)) + facet_wrap(~outcome) +
  labs(title = lbl("or_forest","title"), subtitle = lbl("or_forest","subtitle"),
       x = "Odds ratio (1.0 = no difference)", y = NULL, colour = NULL,
       caption = wrapcap(paste("Single-predictor logistic models, course and cohort fixed effects.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme +
  theme(legend.position = "top",
        panel.spacing    = grid::unit(2.5, "lines"),
        strip.background = element_rect(fill = "grey90", colour = NA),
        strip.text       = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.border     = element_rect(fill = NA, colour = "grey85"))
save_slide(p3, file.path(out, "slide_or_forest.png"))

# --- slide 4: survivorship -----------------------------------------
surv <- rd("tbl_survivorship.csv") |>
  mutate(spec = factor(spec, levels = c("All students", "Reached year 2", "Year 2 + financial confidence")), predictor = factor(predictor, levels = pred_levels))
p4 <- ggplot(surv, aes(OR, predictor, colour = spec)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = grey) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .45, position = position_dodge(width = .6)) +
  geom_point(size = 2.8, position = position_dodge(width = .6)) + scale_x_continuous(breaks = seq(0.8, 1.5, 0.1)) +
  scale_colour_manual(values = c("All students" = grey, "Reached year 2" = orange, "Year 2 + financial confidence" = dcol("dhsc_blue", "#0063BE"))) +
  labs(title = lbl("survivorship","title"), subtitle = lbl("survivorship","subtitle"),
       x = "Odds ratio (1.0 = no difference)", y = NULL, colour = NULL,
       caption = wrapcap(paste("Logistic models, course and cohort fixed effects.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme + theme(legend.position = "top")
save_slide(p4, file.path(out, "slide_survivorship.png"))

# --- slide 5: group comparisons ------------------------------------
gr <- rd("tbl_group_rates.csv"); overall <- gr |> filter(comparison == "Overall") |> pull(leave_rate)
comp_lv <- c("Aware of grant beforehand", "Has children", "Funding critical to course", "Financial confidence")
grp_lv  <- c("Yes", "No", "Low (1-2)", "Mid (3)", "High (4-5)")
gr2 <- gr |> filter(comparison != "Overall") |>
  mutate(group = recode(group, "TRUE" = "Yes", "FALSE" = "No"),
         group = factor(group, levels = grp_lv), comparison = factor(comparison, levels = comp_lv))
lab_line  <- tibble(comparison = factor(comp_lv[1], levels = comp_lv), group = factor("No", levels = grp_lv))
conf_note <- tibble(comparison = factor("Financial confidence", levels = comp_lv), group = factor("Mid (3)", levels = grp_lv))
p5 <- ggplot(gr2, aes(leave_rate, group, fill = comparison)) +
  geom_vline(xintercept = overall, linetype = "dashed", colour = grey) + geom_col(width = .62, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", leave_rate)), hjust = -0.25, size = 4, colour = ink) +
  geom_text(data = lab_line, aes(x = overall + 0.6, y = group), label = sprintf("Overall average, %.0f%%", overall), hjust = 0, vjust = -1.6, size = 3.6, colour = grey, inherit.aes = FALSE) +
  geom_label(data = conf_note, aes(x = 27, y = group), inherit.aes = FALSE, hjust = 0, vjust = 0.5,
             label = "Asked only of continuing (year 2+) students,\na smaller, lower-risk group, so not\ncomparable to the 33% line above",
             fill = dcol("gridgrey", "#E6E6E6"), colour = grey, label.size = 0, size = 3.0, lineheight = 0.95) +
  facet_grid(comparison ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 46), breaks = seq(0, 40, 10), labels = \(z) paste0(z, "%")) +
  scale_fill_manual(values = c(teal, orange, dcol("af_blue", "#12436D"), dcol("af_purple", "#A285D1"))) +
  labs(title = lbl("groups","title"), subtitle = lbl("groups","subtitle"),
       x = "Share who left before finishing", y = NULL, caption = wrapcap(src)) +
  theme_dhsc_slide(base = 15) + capt_theme +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"), panel.grid.major.y = element_blank())
save_slide(p5, file.path(out, "slide_groups.png"))

# --- slide 6: exit breakdown ---------------------------------------
ex <- rd("tbl_exits.csv") |> filter(outcome_cautious != "censored") |> # Censored bucket - noise, remove
  mutate(label = recode(outcome_cautious, active = "Still in progress", no_entry_observed = "First year never surveyed",
                        dropped_out = "Dropped out", completed = "Completed", censored = "Would finish beyond the data"),
         judgeable = ifelse(outcome_cautious %in% c("completed", "dropped_out"), "Can classify", "Cannot classify")) |> arrange(share)
ex$label <- factor(ex$label, levels = ex$label); y_ne <- which(levels(ex$label) == "First year never surveyed")
p6 <- ggplot(ex, aes(share, label, fill = judgeable)) +
  geom_col(width = .68) + geom_text(aes(label = sprintf("%.0f%%", share)), hjust = -0.2, size = 4.2, colour = ink) +
  annotate("text", x = 50, y = y_ne, hjust = 1, vjust = 0.5, size = 3.4, colour = grey, lineheight = 0.95,
           label = "Mostly the 2020 intake: the survey\nwasn't at full scale yet, so their\nfirst year was never recorded") +
  annotate("curve", x = 24, xend = 33, y = y_ne, yend = y_ne, curvature = -0.25, colour = grey, linewidth = .45, arrow = grid::arrow(length = grid::unit(2.2, "mm"), ends = "first")) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(0, 50, 10), labels = \(z) paste0(z, "%")) +
  scale_fill_manual(values = c("Can classify" = teal, "Cannot classify" = grey)) +
  labs(title = lbl("exits","title"), subtitle = lbl("exits","subtitle"), x = "Share of all claimants", y = NULL, fill = NULL,
       caption = wrapcap(paste("Course-length-anchored classification. Leaving analysis (n = 222,174) uses only students whose first year was observed, effectively the 2021-onwards intakes.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme + theme(legend.position = "top", panel.grid.major.y = element_blank())
save_slide(p6, file.path(out, "slide_exits.png"))

# --- slide 7: intention --------------------------------------------
intent <- rd("tbl_dynamics_intention.csv") |>
  mutate(grp = ifelse(considered_leaving, "Said they might leave", "Did not"), grp = factor(grp, levels = c("Did not", "Said they might leave")))
p7 <- ggplot(intent, aes(left_next_pct, grp, fill = considered_leaving)) +
  geom_col(width = .55, show.legend = FALSE) + geom_text(aes(label = sprintf("%.0f%%", left_next_pct)), hjust = -0.25, size = 5.5, colour = ink) +
  scale_fill_manual(values = c(`FALSE` = teal, `TRUE` = orange)) + scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 10), labels = \(z) paste0(z, "%")) +
  labs(title = lbl("intention","title"), subtitle = lbl("intention","subtitle"),
       x = "Share no longer claiming the following year", y = NULL,
       caption = wrapcap(paste("Counted only where the student had course left and a full next year of data existed.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme + theme(panel.grid.major.y = element_blank())
save_slide(p7, file.path(out, "slide_intention.png"))

# --- slide 8: retention funnel -------------------------------------
ret <- rd("tbl_retention_funnel.csv") |> mutate(grp = paste0(length_years, "-year courses"))
p8 <- ggplot(ret, aes(study_year, survival_pct, colour = grp, group = grp)) +
  geom_line(linewidth = 1.1) + geom_point(size = 3.4) +
  geom_text(data = filter(ret, length_years == 3), aes(label = sprintf("%.0f%%", survival_pct)), vjust = -1.2, size = 4.2, show.legend = FALSE) +
  geom_text(data = filter(ret, length_years == 4), aes(label = sprintf("%.0f%%", survival_pct)), vjust = 2.0, size = 4.2, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:4, labels = paste("Year", 1:4), expand = expansion(add = c(.15, .35))) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20), labels = \(z) paste0(z, "%")) +
  scale_colour_manual(values = c("3-year courses" = teal, "4-year courses" = orange)) +
  labs(title = lbl("retention","title"), subtitle = lbl("retention","subtitle"),
       x = "Year of study", y = "Share of starters still enrolled", colour = NULL,
       caption = wrapcap(paste("Course length derived from the data. Only cohorts old enough to be observed to their final year are included (3-year: 2021-2023 starts; 4-year: 2021-2022). 2020 pilot excluded.", src))) +
  theme_dhsc_slide(base = 15) + capt_theme + theme(legend.position = "top", panel.grid.major.x = element_blank())
save_slide(p8, file.path(out, "slide_retention.png"))

# --- slide 9: retention by largest courses -------------------------
rc <- rd("tbl_retention_by_course.csv")
ord <- rc |> distinct(course, starters) |> arrange(desc(starters)) |> pull(course)
rc <- rc |> mutate(course = factor(course, levels = ord))
p9 <- ggplot(rc, aes(study_year, survival_pct)) +
  geom_line(colour = teal, linewidth = 1) + geom_point(colour = teal, size = 2.4) +
  geom_text(aes(label = sprintf("%.0f%%", survival_pct)), vjust = -0.8, size = 3, colour = ink) +
  facet_wrap(~course, ncol = 4) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Y", 1:4)) +
  scale_y_continuous(limits = c(0, 115), breaks = c(0, 50, 100), labels = \(z) paste0(z, "%")) +
  labs(title = lbl("retention_courses","title"), subtitle = lbl("retention_courses","subtitle"),
       x = "Year of study", y = "Share of starters still enrolled",
       caption = wrapcap(paste("Observed proportions, not modelled. Only cohorts observed to their final year are included. 2020 pilot excluded.", src))) +
  theme_dhsc_slide(base = 13) + capt_theme + theme(strip.text = element_text(size = 10), panel.grid.major.x = element_blank())
save_slide(p9, file.path(out, "slide_retention_courses.png"))

progress("done. slides written to ", out)