# =====================================================================
# 02_analyse.R
# Analysis table (from 01)  ->  tidy result tables for visualisation.
# Reads lsf_analysis_sample.rds, writes aggregate tbl_*.csv to outputs.
# No findings stated here. Run:  source("scripts/02_analyse.R")
# =====================================================================
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(fixest)
set.seed(1)
samp <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
out  <- outputs_dir()
progress("loaded analysis sample: ", nrow(samp), " students")

# clean named binaries for the 1-5 importance scales (>=4 = critical). Named
# columns so model-coefficient lookup is reliable (inline I(...) was not).
samp <- samp |> mutate(crit_course = funding_imp_crse >= 4,
                       crit_uni    = funding_imp_uni  >= 4)

# --- helpers ---------------------------------------------------------
pull_or <- function(m, term) {
  na <- tibble(OR = NA_real_, lo = NA_real_, hi = NA_real_)
  if (is.null(m)) return(na)
  ct   <- as.data.frame(fixest::coeftable(m))
  cand <- c(paste0(term, "TRUE"), term)
  row  <- cand[cand %in% rownames(ct)][1]
  if (is.na(row)) { hit <- which(startsWith(rownames(ct), term)); if (length(hit)) row <- rownames(ct)[hit[1]] }
  if (is.na(row)) return(na)
  e <- ct[row, "Estimate"]; s <- ct[row, "Std. Error"]
  tibble(OR = exp(e), lo = exp(e - 1.96 * s), hi = exp(e + 1.96 * s))
}
fit_or <- function(term, outcome, fe = "course + entry_year") {
  f <- stats::as.formula(paste0(outcome, " ~ ", term, " | ", fe))
  m <- tryCatch(fixest::feglm(f, family = binomial, data = samp, warn = FALSE, notes = FALSE), error = function(e) NULL)
  pull_or(m, term)
}
auc <- function(score, y) {
  ok <- !is.na(score) & !is.na(y); score <- score[ok]; y <- as.integer(y[ok])
  r <- rank(score); n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
prof_of <- function(x) { x <- tolower(coalesce(x, ""))
  dplyr::case_when(
    str_detect(x, "midwif") ~ "Midwifery",
    str_detect(x, "mental health") ~ "Nursing - Mental Health",
    str_detect(x, "learning disab") ~ "Nursing - Learning Disability",
    str_detect(x, "child") ~ "Nursing - Children's",
    str_detect(x, "adult") & str_detect(x, "nurs") ~ "Nursing - Adult",
    str_detect(x, "nurs") ~ "Nursing - other/dual",
    str_detect(x, "physio") ~ "Physiotherapy",
    str_detect(x, "occupational") ~ "Occupational Therapy",
    str_detect(x, "paramedic") ~ "Paramedic",
    str_detect(x, "radiograph|radiother") ~ "Radiography",
    str_detect(x, "speech|language") ~ "Speech & Language Therapy",
    str_detect(x, "diet") ~ "Dietetics",
    str_detect(x, "podiat|chiropod") ~ "Podiatry",
    str_detect(x, "operating department|\\bodp\\b") ~ "ODP",
    TRUE ~ "Other AHP")
}

preds <- tibble::tribble(
  ~label,                              ~term,
  "Aware of grant before applying",    "fund_availability",
  "Grant influenced enrolment",        "grant_influence",
  "Funding critical to WHAT to study", "crit_course",
  "Funding critical to WHERE to study","crit_uni",
  "Grant helps me stay",               "grant_helps_stay")
outcomes <- tibble::tribble(
  ~label,                 ~var,
  "Left before finishing","left_before_finish",
  "Claimed once only",    "one_wave_only",
  "Considered leaving",   "considered_leaving",
  "Left 2+ years early",  "left_2y_plus_early")

# --- (1) odds-ratio grid --------------------------------------------
progress("(1) odds-ratio grid ...")
or_grid <- tidyr::crossing(p = seq_len(nrow(preds)), o = seq_len(nrow(outcomes))) |>
  mutate(res = purrr::map2(p, o, ~ fit_or(preds$term[.x], outcomes$var[.y]))) |>
  tidyr::unnest(res) |>
  transmute(predictor = preds$label[p], outcome = outcomes$label[o],
            OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3))
write_csv(or_grid, file.path(out, "tbl_or_grid.csv"))

# --- (2) survivorship (tidy long, with CIs) -------------------------
progress("(2) survivorship three-spec ...")
surv_sample <- samp |> filter(!is.na(confidence))
fit_spec <- function(term, data, adjust) {
  rhs <- if (adjust) paste0(term, " + i(confidence)") else term
  f <- stats::as.formula(paste0("left_before_finish ~ ", rhs, " | course + entry_year"))
  m <- tryCatch(fixest::feglm(f, family = binomial, data = data, warn = FALSE, notes = FALSE), error = function(e) NULL)
  pull_or(m, term)
}
survivorship <- preds |> rowwise() |>
  mutate(`All students`                 = list(fit_spec(term, samp,        FALSE)),
         `Reached year 2`                = list(fit_spec(term, surv_sample, FALSE)),
         `Year 2 + financial confidence` = list(fit_spec(term, surv_sample, TRUE))) |>
  ungroup() |>
  pivot_longer(c(`All students`, `Reached year 2`, `Year 2 + financial confidence`),
               names_to = "spec", values_to = "res") |>
  unnest(res) |>
  transmute(predictor = label, spec, OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3))
write_csv(survivorship, file.path(out, "tbl_survivorship.csv"))

# --- (3) discrimination (in-sample AUC + decile table) --------------
progress("(3) predictive discrimination ...")
terms_survey <- c("fund_availability", "grant_influence", "crit_course", "crit_uni", "grant_helps_stay")
auc_rows <- list(); decile_tbl <- NULL
for (oc in c("left_before_finish", "one_wave_only")) {
  d <- samp[, c(oc, "fund_availability", "grant_influence", "crit_course", "crit_uni", "grant_helps_stay", "entry_year")]
  d[[oc]] <- as.integer(as.logical(d[[oc]])); d <- d[stats::complete.cases(d), ]
  f_svy <- paste(oc, "~", paste(terms_survey, collapse = " + "))
  m_svy <- suppressWarnings(glm(as.formula(f_svy), data = d, family = binomial))
  m_yr  <- suppressWarnings(glm(as.formula(paste(f_svy, "+ factor(entry_year)")), data = d, family = binomial))
  p_svy <- predict(m_svy, type = "response"); p_yr <- predict(m_yr, type = "response")
  auc_rows[[oc]] <- tibble(outcome = oc, n = nrow(d), base_rate = round(mean(d[[oc]]), 3),
                           auc_survey = round(auc(p_svy, d[[oc]]), 3),
                           auc_plus_year = round(auc(p_yr, d[[oc]]), 3))
  if (oc == "left_before_finish")
    decile_tbl <- tibble(decile = dplyr::ntile(p_yr, 10), y = d[[oc]]) |>
      group_by(decile) |> summarise(n = n(), leave_rate = round(100 * mean(y), 1), .groups = "drop")
}
write_csv(bind_rows(auc_rows), file.path(out, "tbl_auc_summary.csv"))
write_csv(decile_tbl,          file.path(out, "tbl_auc_decile.csv"))

# --- (4) factors most associated with leaving (ranked, adjusted) ----
progress("(4) factors most associated with leaving ...")
factors_def <- tibble::tribble(
  ~label,                            ~term,
  "Has children (parental support)", "parental",
  "Funding critical to course",      "crit_course",
  "Funding critical to university",  "crit_uni",
  "Grant helps me stay",             "grant_helps_stay",
  "Grant influenced enrolment",      "grant_influence",
  "Shortage-subject payment",        "specialist",
  "Regional incentive",              "regional",
  "Aware of grant beforehand",       "fund_availability")
factors <- factors_def |> rowwise() |>
  mutate(res = list(fit_or(term, "left_before_finish"))) |> ungroup() |>
  unnest(res) |>
  transmute(factor = label, OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3)) |>
  arrange(desc(abs(log(OR))))
write_csv(factors, file.path(out, "tbl_factors.csv"))

# --- (5) basic group comparisons (descriptive leave rates) ----------
progress("(5) group comparisons ...")
samp <- samp |> mutate(
  conf_band = case_when(confidence <= 2 ~ "Low (1-2)", confidence == 3 ~ "Mid (3)",
                        confidence >= 4 ~ "High (4-5)", TRUE ~ NA_character_))
grp_rate <- function(var, label) samp |>
  filter(!is.na(left_before_finish), !is.na(.data[[var]])) |>
  group_by(group = as.character(.data[[var]])) |>
  summarise(n = n(), leave_rate = round(100 * mean(left_before_finish), 1), .groups = "drop") |>
  mutate(comparison = label) |> select(comparison, group, n, leave_rate)
group_rates <- bind_rows(
  samp |> filter(!is.na(left_before_finish)) |>
    summarise(comparison = "Overall", group = "All students",
              n = n(), leave_rate = round(100 * mean(left_before_finish), 1)),
  grp_rate("fund_availability", "Aware of grant beforehand"),
  grp_rate("parental",          "Has children"),
  grp_rate("crit_course",       "Funding critical to course"),
  grp_rate("conf_band",         "Financial confidence"))
write_csv(group_rates, file.path(out, "tbl_group_rates.csv"))

# --- (6) descriptive exits / continuations --------------------------
progress("(6) exit breakdown ...")
exits <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"),
                  show_col_types = FALSE) |>
  count(outcome_cautious, name = "n") |>
  mutate(share = round(100 * n / sum(n), 1)) |> arrange(desc(n))
write_csv(exits, file.path(out, "tbl_exits.csv"))

# --- (7) structure: profession / university / components / confidence -
progress("(7) profession / university / components / confidence ...")
samp$prof <- prof_of(samp$course)
profession <- samp |> group_by(prof) |>
  summarise(n = n(),
            left_before_finish = round(100 * mean(left_before_finish, na.rm = TRUE), 1),
            one_wave_only      = round(100 * mean(one_wave_only,      na.rm = TRUE), 1),
            considered_leaving = round(100 * mean(considered_leaving, na.rm = TRUE), 1),
            .groups = "drop") |> arrange(desc(left_before_finish))
write_csv(profession, file.path(out, "tbl_profession.csv"))

uni_spread <- function(v) {
  t <- samp |> filter(!is.na(.data[[v]]), !is.na(college)) |>
    group_by(college) |> summarise(n = n(), r = 100 * mean(.data[[v]]), .groups = "drop") |> filter(n >= 200)
  tibble(measure = v, n_providers = nrow(t),
         p10 = round(quantile(t$r, .10), 1), median = round(quantile(t$r, .50), 1),
         p90 = round(quantile(t$r, .90), 1), sd_between = round(sd(t$r), 1))
}
write_csv(bind_rows(uni_spread("left_before_finish"), uni_spread("one_wave_only")),
          file.path(out, "tbl_university_spread.csv"))

comp_or <- bind_rows(
  fit_or("parental",   "one_wave_only", "entry_year")          |> mutate(component = "Parental",   adjust = "cohort"),
  fit_or("specialist", "one_wave_only", "entry_year")          |> mutate(component = "Specialist", adjust = "cohort"),
  fit_or("regional",   "one_wave_only", "entry_year")          |> mutate(component = "Regional",   adjust = "cohort"),
  fit_or("parental",   "one_wave_only", "course + entry_year") |> mutate(component = "Parental",   adjust = "cohort+subject"),
  fit_or("specialist", "one_wave_only", "course + entry_year") |> mutate(component = "Specialist", adjust = "cohort+subject"),
  fit_or("regional",   "one_wave_only", "course + entry_year") |> mutate(component = "Regional",   adjust = "cohort+subject")) |>
  transmute(component, adjust, OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3))
write_csv(comp_or, file.path(out, "tbl_components.csv"))

confidence <- samp |> filter(!is.na(confidence), !is.na(left_before_finish)) |>
  group_by(confidence) |>
  summarise(n = n(), leave_rate = round(100 * mean(left_before_finish), 1), .groups = "drop") |>
  arrange(confidence)
write_csv(confidence, file.path(out, "tbl_confidence.csv"))

progress("done. tables written to ", out)