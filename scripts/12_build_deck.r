# ===========================================================================
# 12_build_deck.r  -  BUILD THE LSF SLIDE DECK ASSETS
#   Produces a numbered, slide-ready set of 16:9 PNGs (charts + branded tables)
#   plus deck_outline.md (titles, exec summary, bullets, speaker notes, and the
#   asset -> slide mapping) into outputs/deck/. You drop the PNGs into the DHSC
#   PowerPoint template; the outline carries every word.
#
#   Consumes the analysis already produced upstream:
#     - lsf_grid_3spec_long.csv      (from 11_communicate_controlled.r)
#     - lsf_3spec_sample.rds         (cached entry-linked sample; built if absent)
#     - lsf_panel_long / trajectories CSVs (for descriptive stats)
#   Descriptive summaries are cached (lsf_deck_descriptives.rds) so reruns skip
#   the 2.15GB read. Set REBUILD_DESCRIPTIVES <- TRUE to refresh them.
# ===========================================================================
library(dplyr); library(readr); library(tidyr); library(purrr); library(ggplot2)
library(tibble); library(stringr)
purrr::walk(list.files("functions", full.names = TRUE), source)

progress("===================== 12_build_deck.r =====================")
deck_dir <- file.path(outputs_dir(), "deck")
if (!dir.exists(deck_dir)) dir.create(deck_dir, recursive = TRUE)
progress("building LSF deck assets -> ", deck_dir)

cap <- "Source: NHS Learning Support Fund longitudinal panel 2020-2026. DHSC analysis."

# --- 1. analysis results from script 11 ------------------------------------
grid_csv <- file.path(outputs_dir(), "lsf_grid_3spec_long.csv")
if (!file.exists(grid_csv))
  stop("lsf_grid_3spec_long.csv not found. Run 11_communicate_controlled.r first.", call. = FALSE)
g_res <- read_csv(grid_csv, show_col_types = FALSE)

# --- 2. entry-linked sample (load cache, or build it the same way as 11) ---
sample_cache <- file.path(derived_dir(), "lsf_3spec_sample.rds")
if (file.exists(sample_cache)) {
  sample <- readRDS(sample_cache)
  progress("loaded cached sample: ", nrow(sample), " students")
} else {
  progress("sample cache missing - building from CSVs ...")
  long0 <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
  traj0 <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
  sample <- build_funding_leaving_sample(long0, traj0) |> define_leaving_outcomes()
  conf_cw <- long0 |> filter(first_year == FALSE, !is.na(confidence)) |>
    arrange(UniqueID, year) |> group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |>
    select(UniqueID, confidence)
  sample <- left_join(sample, conf_cw, by = "UniqueID")
  saveRDS(sample, sample_cache)
}

# --- 3. descriptive summaries (cached) -------------------------------------
REBUILD_DESCRIPTIVES <- FALSE
desc_cache <- file.path(derived_dir(), "lsf_deck_descriptives.rds")

if (!REBUILD_DESCRIPTIVES && file.exists(desc_cache)) {
  desc <- readRDS(desc_cache)
  progress("loaded cached descriptives")
} else {
  progress("computing descriptives from the long panel (one-off read) ...")
  long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE) |>
    mutate(fy = as.logical(first_year), cl = as.logical(leave_course))

  coverage <- long |>
    summarise(responses = n(), first_year = sum(fy, na.rm = TRUE),
              continuing = sum(!fy, na.rm = TRUE), .by = year) |> arrange(year)

  considered <- long |> filter(!fy, !is.na(cl)) |>
    summarise(answered = n(), considered = sum(cl), pct = mean(cl), .by = year) |> arrange(year)

  factors <- long |> filter(!fy, cl, !is.na(leave_factors)) |>
    separate_rows(leave_factors, sep = ",") |>
    mutate(leave_factors = str_squish(leave_factors)) |>
    filter(leave_factors != "") |> count(leave_factors, sort = TRUE)

  prevalence <- sample |>
    summarise(`Aware of grant before applying`   = mean(fund_availability, na.rm = TRUE),
              `Grant influenced enrolment`        = mean(grant_influence,  na.rm = TRUE),
              `Funding critical to WHAT to study` = mean(funding_imp_crse >= 4, na.rm = TRUE),
              `Funding critical to WHERE to study`= mean(funding_imp_uni  >= 4, na.rm = TRUE),
              `Grant helps me stay`               = mean(grant_helps_stay, na.rm = TRUE)) |>
    pivot_longer(everything(), names_to = "Funding answer at entry", values_to = "p")

  n_panel    <- long |> distinct(UniqueID) |> nrow()
  overall_cl <- long |> filter(!fy, !is.na(cl)) |> summarise(p = mean(cl)) |> pull(p)

  desc <- list(coverage = coverage, considered = considered, factors = factors,
               prevalence = prevalence, n_panel = n_panel, overall_cl = overall_cl)
  saveRDS(desc, desc_cache)
}

n_all  <- nrow(sample)
n_surv <- sample |> filter(!is.na(confidence), !is.na(course), !is.na(entry_year)) |> nrow()

# --- 4. shared chart vocabulary --------------------------------------------
or_breaks   <- c(0.7, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5, 1.75)
pred_levels <- c("Aware of grant before applying", "Grant influenced enrolment",
                 "Funding critical to WHAT to study", "Funding critical to WHERE to study",
                 "Grant helps me stay")
out_levels  <- c("Considered leaving", "Dropped after year 1", "Left 2+ years early",
                 "Stopped in final year", "Left before finish")
spec_levels <- c("All sample, unadjusted", "Year-2 sample, unadjusted",
                 "Year-2 sample, + fin. confidence")
mcols <- c("All sample, unadjusted"           = dcol("midgrey",  "#7F7F7F"),
           "Year-2 sample, unadjusted"        = dcol("af_orange","#E66100"),
           "Year-2 sample, + fin. confidence" = dcol("dhsc_blue","#1F6FB2"))

# --- 5. TABLES (branded images) --------------------------------------------
progress("rendering tables ...")
t_key <- tibble(Metric = c("Students in panel", "Survey years", "Considered-leaving rate (continuing)",
                           "Modelled (entry-linked)", "Modelled (year-2 survivors)"),
                Value  = c(scales::comma(desc$n_panel), "2020-2026",
                           scales::percent(desc$overall_cl, accuracy = 0.1),
                           scales::comma(n_all), scales::comma(n_surv)))
save_slide(dhsc_table_plot(t_key, title = "LSF retention analysis - at a glance", caption = cap),
           file.path(deck_dir, "s02_key_numbers.png"), width = 11, height = 1.1 + 0.6 * (nrow(t_key) + 1))

t_cov <- desc$coverage |> transmute(Year = as.character(year),
            Responses = scales::comma(responses), `First-year` = scales::comma(first_year),
            Continuing = scales::comma(continuing))
save_slide(dhsc_table_plot(t_cov, title = "Survey responses by year", caption = cap),
           file.path(deck_dir, "s06_coverage.png"), width = 11, height = 1.0 + 0.55 * (nrow(t_cov) + 1))

t_prev <- desc$prevalence |> transmute(`Funding answer at entry`,
            `% of students` = scales::percent(p, accuracy = 0.1))
save_slide(dhsc_table_plot(t_prev, title = "How funding-dependent are entrants?", caption = cap),
           file.path(deck_dir, "s06_dependence_prevalence.png"), width = 11, height = 1.0 + 0.6 * (nrow(t_prev) + 1))

t_cl <- desc$considered |> transmute(Year = as.character(year), Answered = scales::comma(answered),
            `Considered leaving` = scales::comma(considered), Rate = scales::percent(pct, accuracy = 0.1))
save_slide(dhsc_table_plot(t_cl, title = "Considered leaving, by year", caption = cap),
           file.path(deck_dir, "s07_considered_table.png"), width = 11, height = 1.0 + 0.55 * (nrow(t_cl) + 1))

# --- 6. CHARTS -------------------------------------------------------------
progress("rendering charts ...")

# C-def  leaving is measured against each course's DERIVED expected finish ---
# Same student (last seen after 2 years), three course lengths, three verdicts.
# Course length is derived per course from the data; the finish line moves, so
# the same behaviour is classified differently depending on the course.
lanes <- tibble::tribble(
  ~lane,                                              ~len, ~verdict,
  "2-year course\n(top-up, direct entry)",            2,    "Completed (reached finish)",
  "3-year course\n(most nursing, midwifery, AHP)",    3,    "Left in final year (1 year early)",
  "4-year course\n(dual-field, integrated master's)", 4,    "Left 2+ years early")
last_seen <- 2L   # the same illustrative student in every lane

def_cells <- crossing(lane = lanes$lane, year = 1:4) |>
  left_join(lanes, by = "lane") |>
  mutate(state = case_when(year > len        ~ "Beyond course length",
                           year <= last_seen ~ "Enrolled / claiming",
                           TRUE              ~ "Gone (left early)"),
         lane = factor(lane, levels = rev(lanes$lane)))
fin <- lanes |> mutate(xf = len + 0.5, lane = factor(lane, levels = rev(lanes$lane)))

p_def <- ggplot(def_cells, aes(year, lane)) +
  geom_tile(aes(fill = state), width = 0.92, height = 0.6, colour = "white", linewidth = 1.4) +
  geom_segment(data = fin, inherit.aes = FALSE,
               aes(x = xf, xend = xf, y = as.integer(lane) - 0.42, yend = as.integer(lane) + 0.42),
               linetype = "dashed", colour = dcol("risk", "#C0392B"), linewidth = 0.8) +
  geom_text(data = fin, inherit.aes = FALSE,
            aes(x = xf, y = as.integer(lane) + 0.40, label = "expected finish"),
            colour = dcol("risk", "#C0392B"), size = 2.9, hjust = 0.5, vjust = 0) +
  geom_text(data = lanes |> mutate(lane = factor(lane, levels = rev(lanes$lane))),
            aes(x = 4.65, y = lane, label = verdict), inherit.aes = FALSE,
            hjust = 0, fontface = "bold", size = 4.2, colour = "#262626") +
  scale_fill_manual(values = c("Enrolled / claiming"  = "#01A188",
                               "Gone (left early)"     = dcol("af_orange", "#E66100"),
                               "Beyond course length"  = "grey90")) +
  scale_x_continuous(breaks = 1:4, labels = paste("Year", 1:4), limits = c(0.4, 7.8),
                     expand = c(0, 0), position = "top") +
  labs(title = "Leaving is measured against each course's expected finish, not a fixed number of years",
       subtitle = paste0("Course length is derived per course from the data (validated 94% within +-1 year). ",
                         "The same student - last seen after two years - is classified differently as the finish line moves."),
       x = NULL, y = NULL, fill = NULL, caption = cap) +
  theme_dhsc_slide(base = 15) +
  theme(panel.grid = element_blank(), legend.position = "top")
save_slide(p_def, file.path(deck_dir, "s05_leaving_definitions.png"), width = 13.33, height = 7.5)

# C1 considered-leaving rate by year
p_cl <- desc$considered |> mutate(year = factor(year)) |>
  ggplot(aes(year, pct)) +
  geom_col(fill = dcol("dhsc_blue", "#1F6FB2"), width = 0.7) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.4, size = 4.6) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Considered leaving, by survey year (continuing students)",
       subtitle = "Share answering 'yes' to ever feeling they may have to leave their course",
       x = NULL, y = NULL, caption = cap) + theme_dhsc_slide()
save_slide(p_cl, file.path(deck_dir, "s07_considered_year.png"), width = 12, height = 6.5)

# C2 top reasons for considering leaving
p_lf <- desc$factors |> slice_max(n, n = 10) |>
  mutate(leave_factors = reorder(leave_factors, n)) |>
  ggplot(aes(n, leave_factors)) +
  geom_col(fill = dcol("af_orange", "#E66100"), width = 0.7) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.15, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(title = "Top reasons given for considering leaving",
       subtitle = "Multi-select, among continuing students who considered leaving",
       x = "Students", y = NULL, caption = cap) + theme_dhsc_slide()
save_slide(p_lf, file.path(deck_dir, "s07_leave_factors.png"), width = 12, height = 6.8)

# C3 two opposite constructs (all-sample, left before finish)
g1 <- g_res |> filter(spec == "All sample, unadjusted", out == "left_before_finish") |>
  mutate(pred_label = factor(pred_label, levels = rev(pred_levels)),
         direction  = ifelse(OR < 1, "Protective (less likely to leave)",
                                      "Higher risk (more likely to leave)"))
p_tc <- ggplot(g1, aes(OR, pred_label, colour = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dcol("midgrey", "#7F7F7F")) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.6) +
  geom_point(size = 3.4) +
  geom_text(aes(x = hi, label = sprintf("%.2f", OR)), hjust = -0.3, size = 4, show.legend = FALSE) +
  scale_x_log10(breaks = or_breaks) +
  scale_colour_manual(values = c("Protective (less likely to leave)"  = dcol("good", "#2E7D32"),
                                 "Higher risk (more likely to leave)" = dcol("risk", "#C0392B"))) +
  labs(title = "Awareness protects; dependence flags risk",
       subtitle = "Odds of leaving before finishing, by funding answer at entry (all respondents)",
       x = "Odds ratio  (log scale, right of 1 = more likely to leave)", y = NULL,
       colour = NULL, caption = cap) +
  theme_dhsc_slide() + theme(legend.position = "top")
save_slide(p_tc, file.path(deck_dir, "s08_two_constructs.png"), width = 13.33, height = 7.5)

# C4 robust across definitions (year-2 survivors, faceted by outcome)
g2 <- g_res |> filter(spec == "Year-2 sample, unadjusted") |>
  mutate(pred_label = factor(pred_label, levels = rev(pred_levels)),
         out_label  = factor(out_label,  levels = out_levels))
p_rb <- ggplot(g2, aes(OR, pred_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dcol("midgrey", "#7F7F7F")) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.45, colour = dcol("dhsc_blue", "#1F6FB2")) +
  geom_point(size = 2.2, colour = dcol("dhsc_blue", "#1F6FB2")) +
  scale_x_log10(breaks = or_breaks) +
  facet_wrap(~out_label, ncol = 3) +
  labs(title = "The risk signal is robust across every definition of leaving",
       subtitle = "Odds ratios per funding answer (year-2 survivors), by how 'leaving' is defined",
       x = "Odds ratio  (log scale)", y = NULL, caption = cap) +
  theme_dhsc_slide(base = 13) + theme(panel.spacing = grid::unit(1, "lines"))
save_slide(p_rb, file.path(deck_dir, "s09_robust_definitions.png"), width = 13.33, height = 7.5)

# C5 three-spec headline (the widescreen one)
dodge <- position_dodge(width = 0.7)
g3 <- g_res |> filter(out == "left_before_finish") |>
  mutate(pred_label = factor(pred_label, levels = rev(pred_levels)),
         spec       = factor(spec, levels = spec_levels))
p_3 <- ggplot(g3, aes(OR, pred_label, colour = spec)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dcol("midgrey", "#7F7F7F")) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16, linewidth = 0.5, position = dodge) +
  geom_point(size = 3, position = dodge) +
  geom_text(aes(x = hi, label = sprintf("%.2f", OR)), position = dodge, hjust = -0.3, size = 3.4, show.legend = FALSE) +
  scale_x_log10(breaks = or_breaks) + scale_colour_manual(values = mcols) +
  labs(title = "Dependence survives the financial-confidence control; awareness was survivorship",
       subtitle = "Odds of leaving before finishing: all respondents -> year-2 survivors -> survivors + financial confidence",
       x = "Odds ratio  (log scale)", y = NULL, colour = NULL, caption = cap) +
  theme_dhsc_slide() + theme(legend.position = "top")
save_slide(p_3, file.path(deck_dir, "s10_3spec_headline.png"), width = 13.33, height = 7.5)

# C6 full grid (annex)
g4 <- g_res |>
  mutate(pred_label = factor(pred_label, levels = rev(pred_levels)),
         out_label  = factor(out_label,  levels = out_levels),
         spec       = factor(spec, levels = spec_levels))
p_full <- ggplot(g4, aes(OR, pred_label, colour = spec)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dcol("midgrey", "#7F7F7F")) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.4, position = dodge) +
  geom_point(size = 1.9, position = dodge) +
  scale_x_log10(breaks = or_breaks) + scale_colour_manual(values = mcols) +
  facet_wrap(~out_label, ncol = 3) +
  labs(title = "Full results: every funding answer x every leaving definition x three specs",
       subtitle = "All respondents (grey), year-2 survivors (orange), survivors + financial confidence (blue)",
       x = "Odds ratio  (log scale)", y = NULL, colour = NULL, caption = cap) +
  theme_dhsc_slide(base = 12) + theme(panel.spacing = grid::unit(1, "lines"), legend.position = "top")
save_slide(p_full, file.path(deck_dir, "s13_full_grid.png"), width = 13.33, height = 7.5)

# --- 7. OUTLINE (titles, exec summary, bullets, notes, asset map) ----------
progress("writing deck_outline.md ...")
md <- c(
"# NHS Learning Support Fund - retention analysis: deck outline",
"",
sprintf("Auto-generated by `12_build_deck.r`. Assets in this folder. Panel: %s students, 2020-2026.", scales::comma(desc$n_panel)),
"All slides 16:9. Brand: teal #01A188, Arial. Drop the matching `sNN_*.png` onto each slide.",
"",
"---",
"## Slide 1 - Title",
"**NHS Learning Support Fund: who is at risk of leaving, and what the funding answers tell us**",
"Subtitle: DHSC workforce analysis. Longitudinal LSF survey panel, 2020-2026.",
"_Notes:_ Scope in one line: the LSF claimant survey, a near-census of claimants; self-reported intentions, not administrative completion.",
"",
"## Slide 2 - Executive summary  [asset: s02_key_numbers.png]",
"- Funding **dependence** at entry reliably marks a higher retention-risk group (~1.2-1.4x), robust across every definition of leaving.",
"- That risk signal **survives controlling for financial confidence** - it is not just 'these students are broke'. The funding questions are a better at-risk screen than a confidence question.",
"- Grant **awareness** before applying looks protective overall, but that is **survivorship**: it guards against early exit only, and is neutral among continuing students.",
"- Everything is **associational, not causal**: the grant reaches precarious students (good targeting); we cannot say it improves retention without a control group.",
sprintf("- Evidence base: %s students modelled (entry-linked), %s year-2 survivors with financial-confidence data.", scales::comma(n_all), scales::comma(n_surv)),
"",
"## Slide 3 - What we did and the data",
sprintf("- LSF claimant survey, %s students, seven waves 2020-2026, stitched into one longitudinal panel (one row per student-year).", scales::comma(desc$n_panel)),
"- Compulsory-to-claim, so a near-census of claimants; instrument stable across years, so years pool.",
"- **Key caveat (state it up front):** these are self-reported intentions and concerns, not administrative dropout. 'Considered leaving' is a validated leading indicator, not a leaving record.",
"_Notes:_ Validated against next-wave disappearance: considering leaving raises 'gone next year' from 15.8% to 22.6%.",
"",
"## Slide 4 - Method in one slide",
"- Collapsed to trajectories (entry funding answers linked to later considered-leaving and to a derived completion proxy).",
"- 'Leaving' defined on a spectrum (next slide), measured against each course's own expected finish, so findings don't hinge on one definition.",
"- Course length derived from the data (median intended start year), validated 94% within +-1 year.",
"- Each funding answer modelled in its own logistic regression with course and cohort fixed effects.",
"",
"## Slide 5 - How we define 'leaving'  [asset: s05_leaving_definitions.png]",
"- Leaving is measured against each course's **expected finish**, which we derive per course from the data (not a fixed number of years).",
"- The chart shows one illustrative student, last seen after two years, on three course lengths: completer on a 2-year course, final-year leaver on a 3-year, 2+-years-early leaver on a 4-year.",
"- So the same rule ('reached finish' / 'N years early') gives different verdicts as the finish line moves - which is why we test a spectrum, including the assumption-free 'claimed once, never again'.",
"- Separately, the survey also asks students directly whether they considered leaving (a self-reported measure), tested on the findings slides.",
"_Notes:_ Most NMAH courses are 3 years; top-up/direct-entry 2; dual-field and integrated master's 4. Length validated 94% within +-1 year.",
"",
"## Slide 6 - Descriptive picture: coverage and dependence  [assets: s06_coverage.png, s06_dependence_prevalence.png]",
"- Left: survey responses by year (first-year vs continuing split).",
"- Right: how funding-dependent entrants are (share aware, influenced, funding critical to what/where, grant helps stay).",
"",
"## Slide 7 - Descriptive picture: considering leaving  [assets: s07_considered_year.png, s07_considered_table.png, s07_leave_factors.png]",
sprintf("- Considered-leaving rate sits around %s of continuing students.", scales::percent(desc$overall_cl, accuracy = 0.1)),
"- Financial worries are the top reason given - which motivates the financial-confidence control later.",
"",
"## Slide 8 - Finding 1: two opposite funding constructs  [asset: s08_two_constructs.png]",
"- Awareness sits left of 1 (protective); the four dependence measures sit right of 1 (risk).",
"- Awareness ~ preparedness; dependence ~ precarity. A useful, non-obvious split.",
"",
"## Slide 9 - Finding 2: the risk signal is robust  [asset: s09_robust_definitions.png]",
"- Dependence predicts leaving across every definition, including the assumption-free 'claimed once' measure and the self-reported 'considered leaving'.",
"- Effect sizes are modest on behaviour (1.1-1.25), larger on intention - the consistency is the story.",
"",
"## Slide 10 - Finding 3: it survives the confidence control  [asset: s10_3spec_headline.png]",
"- Controlling for financial confidence barely moves the dependence odds ratios (<=0.03).",
"- Awareness's protective effect collapses once we restrict to survivors - it was survivorship.",
"_Notes:_ Read left to right: grey (all) -> orange (survivors) -> blue (+ confidence).",
"",
"## Slide 11 - What we can and cannot say (the causal boundary)",
"- CAN say (p~0.85-0.9): dependence cleanly identifies the higher-risk group; the grant reaches precarious students; the link is not reducible to stated financial confidence.",
"- CANNOT say (structurally unidentified here): whether the grant improves retention - every respondent is a claimant, so there is no unfunded control group.",
"- Route to causal: a known LSF policy change in 2020-2026 (a discontinuity), or linkage to HESA/NMC completion records.",
"",
"## Slide 12 - Conclusions and recommendations",
"- Targeting works: funding-salience questions flag the at-risk population better than a direct confidence question - usable as a light-touch screen.",
"- Treat dependence-flagged students as a priority group for retention support.",
"- For an impact estimate, pursue admin-data linkage or exploit a policy discontinuity; the survey alone is descriptive.",
"",
"## Slide 13 - Annex: full results  [asset: s13_full_grid.png]",
"- Every funding answer x every leaving definition x all three specifications.",
"- Plus pointers: leaving-definition spectrum, survey data dictionary, methods note.",
"")
writeLines(md, file.path(deck_dir, "deck_outline.md"))

progress("done -> ", deck_dir)