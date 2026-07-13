# ===========================================================================
# scripts/05_real_value_controlled.r
#
# After 04 showed that local real value of the LSF predicts leaving, this
# script asks the next questions:
#   1. Does real value still predict leaving once we control for the survey
#      funding factors already established in 02_analyse?
#   2. How does its effect size / predictive power compare to those factors?
#   3. Is the association robust across leaving definitions?
#   4. Does real value matter MORE for parental-support or specialist recipients
#      (separate interaction models)?
#
# ASSOCIATIONAL only. Same causal boundary as the rest of the project.
#
# Inputs:  lsf_analysis_sample.rds (from 01) + reference/*.csv
# Outputs: tbl_rv_*.csv in outputs_dir()
#
# Run:  source("scripts/05_real_value_controlled.r")
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(purrr); library(fixest); library(tibble)
})
set.seed(1)

# ---- CONFIG ----------------------------------------------------------------
REF_DIR   <- "reference"
PRIMARY   <- "real_value_rent_ttwa"     # headline: functional housing market
PRIMARY_LBL <- "Rent-adjusted (TTWA)"
FE        <- "course + entry_year"
OUTCOMES  <- tibble::tribble(
  ~label,                    ~var,
  "Left before finishing",   "left_before_finish",
  "Claimed once only",       "one_wave_only",
  "Left 2+ years early",     "left_2y_plus_early",
  "Considered leaving",      "considered_leaving"
)
MEASURES <- c(
  real_value_cpih      = "Inflation-only (CPIH)",
  real_value_rent      = "Rent-adjusted (LAD)",
  real_value_hp        = "House-price-adjusted (LAD)",
  real_value_rent_ttwa = "Rent-adjusted (TTWA)",
  real_value_hp_ttwa   = "House-price-adjusted (TTWA)"
)
SURVEY_RHS <- paste(
  "fund_availability", "grant_influence", "crit_course", "crit_uni", "grant_helps_stay",
  sep = " + "
)
COMP_RHS <- paste("parental", "specialist", "regional", sep = " + ")
# ---------------------------------------------------------------------------

out  <- outputs_dir()
progress("05: loading analysis sample ...")
SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))

ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

need <- c(RV_PROVIDER, RV_YEAR, "left_before_finish", "fund_availability",
          "grant_influence", "funding_imp_crse", "funding_imp_uni", "grant_helps_stay")
miss <- setdiff(need, names(SAMPLE))
if (length(miss))
  stop("SAMPLE is missing: ", paste(miss, collapse = ", "),
       "\nRe-run scripts/01_read_tidy.r first.")

progress("05: building real-value measures (parent flag = ", RV_PARENT, ") ...")
t0 <- Sys.time()
samp <- build_real_value(SAMPLE, ref, awards, cpih, base_year = 2020)
cat(sprintf("  done in %.1fs | coverage rent_ttwa %.1f%% | parents %s | specialists %s\n",
            as.numeric(Sys.time() - t0, units = "secs"),
            100 * mean(!is.na(samp[[PRIMARY]])),
            format(sum(samp$has_parent == 1L, na.rm = TRUE), big.mark = ","),
            format(sum(as.logical(samp$specialist), na.rm = TRUE), big.mark = ",")))

# survey dummies (same coding as 02) + scaled primary + £1k scale + components
samp <- samp |>
  mutate(
    crit_course = funding_imp_crse >= 4L,
    crit_uni    = funding_imp_uni  >= 4L,
    parental    = as.logical(coalesce(parental, has_parent == 1L)),
    specialist  = as.logical(coalesce(specialist, FALSE)),
    regional    = as.logical(coalesce(regional, FALSE)),
    fund_availability = as.logical(fund_availability),
    grant_influence   = as.logical(grant_influence),
    grant_helps_stay  = as.logical(grant_helps_stay),
    rv     = as.numeric(scale(.data[[PRIMARY]])),
    rv_k   = .data[[PRIMARY]] / 1000,                 # per £1,000 real (policy scale)
    has_parent = as.integer(parental)
  )

# ---------------------------------------------------------------------------
# (1) Spec ladder on PRIMARY measure: does rv survive survey controls?
# ---------------------------------------------------------------------------
progress("05: (1) spec ladder ...")

# S3 uses year-2 survivors (those with financial confidence observed)
surv_ok <- !is.na(samp$confidence)

spec_def <- tibble::tribble(
  ~spec, ~label,                                      ~rhs, ~survivors_only,
  "S0",  "Real value only",                           "rv", FALSE,
  "S1",  "Real value + survey funding",               paste("rv", SURVEY_RHS, sep = " + "), FALSE,
  "S2",  "S1 + grant components",                     paste("rv", SURVEY_RHS, COMP_RHS, sep = " + "), FALSE,
  "S3",  "S1 + financial confidence (year-2 only)", paste("rv", SURVEY_RHS, "i(confidence)", sep = " + "), TRUE,
  "S0k", "Real value only (per £1,000)",              "rv_k", FALSE,
  "S1k", "Real value + survey (per £1,000)",          paste("rv_k", SURVEY_RHS, sep = " + "), FALSE
)

ladder_one <- function(spec_row, outcome_var, outcome_label) {
  d <- samp
  if (isTRUE(spec_row$survivors_only)) d <- d |> filter(surv_ok)
  term <- if (spec_row$spec %in% c("S0k", "S1k")) "rv_k" else "rv"
  d <- d |> filter(!is.na(.data[[outcome_var]]), !is.na(.data[[term]]))
  m <- fit_feglm(d, outcome_var, spec_row$rhs, FE)
  pull_or(m, term) |>
    transmute(
      spec = spec_row$spec, spec_label = spec_row$label,
      outcome = outcome_label, outcome_var = outcome_var,
      measure = PRIMARY_LBL, scale = if (term == "rv_k") "per £1000" else "per 1 SD",
      term, OR, lo, hi, p, n = nrow(d)
    )
}

ladder <- tidyr::crossing(
  s = seq_len(nrow(spec_def)),
  o = seq_len(nrow(OUTCOMES))
) |>
  mutate(res = purrr::map2(s, o, ~ ladder_one(spec_def[.x, ], OUTCOMES$var[.y], OUTCOMES$label[.y]))) |>
  tidyr::unnest(res) |>
  select(-s, -o) |>
  mutate(across(c(OR, lo, hi), ~ round(.x, 3)),
         p = signif(p, 3))

write_csv(ladder, file.path(out, "tbl_rv_spec_ladder.csv"))
progress("  wrote tbl_rv_spec_ladder.csv (", nrow(ladder), " rows)")

# ---------------------------------------------------------------------------
# (2) Joint horse-race terms under S1 (primary outcome + primary measure)
# ---------------------------------------------------------------------------
progress("05: (2) joint terms (S1 horse race) ...")

joint_terms <- c("rv", "fund_availability", "grant_influence", "crit_course",
                 "crit_uni", "grant_helps_stay")
joint_labels <- c(
  rv = "Real value of LSF (per 1 SD, rent TTWA)",
  fund_availability = "Aware of grant before applying",
  grant_influence = "Grant influenced enrolment",
  crit_course = "Funding critical to WHAT to study",
  crit_uni = "Funding critical to WHERE to study",
  grant_helps_stay = "Grant helps me stay"
)

d_joint <- samp |>
  filter(!is.na(left_before_finish), !is.na(rv),
         !is.na(fund_availability), !is.na(grant_influence),
         !is.na(crit_course), !is.na(crit_uni), !is.na(grant_helps_stay))
m_joint <- fit_feglm(d_joint, "left_before_finish",
                     paste("rv", SURVEY_RHS, sep = " + "), FE)
joint <- purrr::map_dfr(joint_terms, ~ pull_or(m_joint, .x)) |>
  mutate(
    label = unname(joint_labels[term]),
    n = nrow(d_joint),
    across(c(OR, lo, hi), ~ round(.x, 3)),
    p = signif(p, 3)
  ) |>
  arrange(desc(abs(log(OR))))
write_csv(joint, file.path(out, "tbl_rv_joint_terms.csv"))
progress("  wrote tbl_rv_joint_terms.csv")

# ---------------------------------------------------------------------------
# (3) Interactions: does real value matter MORE for parents / specialists?
#     Separate models (individually), primary outcome + S1 survey controls.
# ---------------------------------------------------------------------------
progress("05: (3) parent + specialist interactions ...")

interact_specs <- tibble::tribble(
  ~group,        ~rhs_extra,              ~main_term,  ~int_term,
  "parental",    "parental + rv:parental", "parental",  "rv:parental",
  "specialist",  "specialist + rv:specialist", "specialist", "rv:specialist"
)

fit_interact <- function(group, rhs_extra, main_term, int_term, measure_col, measure_label) {
  d <- samp |>
    mutate(rv_m = as.numeric(scale(.data[[measure_col]]))) |>
    filter(!is.na(left_before_finish), !is.na(rv_m),
           !is.na(fund_availability), !is.na(grant_influence),
           !is.na(crit_course), !is.na(crit_uni), !is.na(grant_helps_stay),
           !is.na(.data[[group]]))
  # use rv_m in formula - assign column name rv for clean interaction names
  d$rv <- d$rv_m
  rhs <- paste("rv", SURVEY_RHS, rhs_extra, sep = " + ")
  m <- fit_feglm(d, "left_before_finish", rhs, FE)
  bind_rows(
    pull_or(m, "rv")        |> mutate(role = "real value (main, group=0)"),
    pull_or(m, main_term)   |> mutate(role = paste0(group, " main")),
    pull_or(m, int_term)    |> mutate(role = paste0("rv x ", group, " (does it matter MORE?)"))
  ) |>
    mutate(
      group = group, measure = measure_label, measure_col = measure_col,
      n = nrow(d), n_group = sum(as.logical(d[[group]]), na.rm = TRUE),
      across(c(OR, lo, hi), ~ round(.x, 3)), p = signif(p, 3)
    )
}

# full interactions on primary measure; also S0-style (no survey) for each group
interact_primary <- purrr::pmap_dfr(
  list(interact_specs$group, interact_specs$rhs_extra,
       interact_specs$main_term, interact_specs$int_term),
  ~ fit_interact(..1, ..2, ..3, ..4, PRIMARY, PRIMARY_LBL)
)

# measure sensitivity for interactions: S0-style rv * group only (no survey), all 5 measures
fit_interact_s0 <- function(group, measure_col, measure_label) {
  d <- samp |>
    mutate(rv = as.numeric(scale(.data[[measure_col]]))) |>
    filter(!is.na(left_before_finish), !is.na(rv), !is.na(.data[[group]]))
  rhs <- paste0("rv * ", group)
  m <- fit_feglm(d, "left_before_finish", rhs, FE)
  bind_rows(
    pull_or(m, "rv") |> mutate(role = "real value (main)"),
    pull_or(m, group) |> mutate(role = paste0(group, " main")),
    pull_or(m, paste0("rv:", group)) |> mutate(role = paste0("rv x ", group))
  ) |>
    mutate(
      group = group, measure = measure_label, measure_col = measure_col,
      controls = "none (S0 + interaction)",
      n = nrow(d), n_group = sum(as.logical(d[[group]]), na.rm = TRUE),
      across(c(OR, lo, hi), ~ round(.x, 3)), p = signif(p, 3)
    )
}

interact_s0 <- tidyr::crossing(
  group = c("parental", "specialist"),
  m = names(MEASURES)
) |>
  mutate(res = purrr::map2(group, m, ~ fit_interact_s0(.x, .y, MEASURES[[.y]]))) |>
  tidyr::unnest(res) |>
  select(-m)

interact_primary <- interact_primary |> mutate(controls = "S1 survey + interaction")
interact_all <- bind_rows(interact_primary, interact_s0)
write_csv(interact_all, file.path(out, "tbl_rv_interactions.csv"))
progress("  wrote tbl_rv_interactions.csv")

# ---------------------------------------------------------------------------
# (4) Measure sensitivity: S0 + S1 across all five real-value measures
# ---------------------------------------------------------------------------
progress("05: (4) measure sensitivity (S0/S1 x 5 measures) ...")

meas_one <- function(measure_col, measure_label, controls) {
  d <- samp |>
    mutate(rv = as.numeric(scale(.data[[measure_col]]))) |>
    filter(!is.na(left_before_finish), !is.na(rv))
  rhs <- if (controls == "S0") "rv" else paste("rv", SURVEY_RHS, sep = " + ")
  if (controls == "S1") {
    d <- d |> filter(!is.na(fund_availability), !is.na(grant_influence),
                     !is.na(crit_course), !is.na(crit_uni), !is.na(grant_helps_stay))
  }
  m <- fit_feglm(d, "left_before_finish", rhs, FE)
  pull_or(m, "rv") |>
    transmute(
      controls, measure = measure_label, measure_col,
      OR, lo, hi, p, n = nrow(d)
    )
}

meas_sens <- tidyr::crossing(
  m = names(MEASURES),
  controls = c("S0", "S1")
) |>
  mutate(res = purrr::map2(m, controls, ~ meas_one(.x, MEASURES[[.x]], .y))) |>
  tidyr::unnest(res) |>
  select(-m) |>
  mutate(across(c(OR, lo, hi), ~ round(.x, 3)), p = signif(p, 3))

write_csv(meas_sens, file.path(out, "tbl_rv_measure_sensitivity.csv"))
progress("  wrote tbl_rv_measure_sensitivity.csv")

# ---------------------------------------------------------------------------
# (5) Predictive discrimination: survey vs rv vs combined (AUC)
# ---------------------------------------------------------------------------
progress("05: (5) AUC horse race ...")

auc_for_outcome <- function(oc) {
  cols <- c(oc, "rv", "fund_availability", "grant_influence", "crit_course",
            "crit_uni", "grant_helps_stay", "entry_year")
  d <- samp[, cols]
  d[[oc]] <- as.integer(as.logical(d[[oc]]))
  d <- d[stats::complete.cases(d), ]
  if (nrow(d) < 100L) {
    return(tibble(outcome = oc, n = nrow(d), base_rate = NA_real_,
                  auc_survey = NA_real_, auc_rv = NA_real_, auc_combined = NA_real_))
  }
  f_svy <- paste(oc, "~", SURVEY_RHS, "+ factor(entry_year)")
  f_rv  <- paste(oc, "~ rv + factor(entry_year)")
  f_both <- paste(oc, "~ rv +", SURVEY_RHS, "+ factor(entry_year)")
  m_svy  <- suppressWarnings(stats::glm(stats::as.formula(f_svy),  data = d, family = binomial))
  m_rv   <- suppressWarnings(stats::glm(stats::as.formula(f_rv),   data = d, family = binomial))
  m_both <- suppressWarnings(stats::glm(stats::as.formula(f_both), data = d, family = binomial))
  tibble(
    outcome = oc, n = nrow(d), base_rate = round(mean(d[[oc]]), 3),
    auc_survey   = round(auc_score(stats::predict(m_svy,  type = "response"), d[[oc]]), 3),
    auc_rv       = round(auc_score(stats::predict(m_rv,   type = "response"), d[[oc]]), 3),
    auc_combined = round(auc_score(stats::predict(m_both, type = "response"), d[[oc]]), 3)
  )
}

auc_tbl <- purrr::map_dfr(c("left_before_finish", "one_wave_only"), auc_for_outcome)
write_csv(auc_tbl, file.path(out, "tbl_rv_auc.csv"))
progress("  wrote tbl_rv_auc.csv")

# ---------------------------------------------------------------------------
# Console headline (primary outcome, primary measure)
# ---------------------------------------------------------------------------
cat("\n=== Spec ladder: ", PRIMARY_LBL, " -> left_before_finish ===\n", sep = "")
print(as.data.frame(
  ladder |>
    filter(outcome_var == "left_before_finish", scale == "per 1 SD") |>
    select(spec, spec_label, OR, lo, hi, p, n)
), row.names = FALSE)

cat("\n=== Joint S1 terms (same model) ===\n")
print(as.data.frame(joint |> select(label, OR, lo, hi, p)), row.names = FALSE)

cat("\n=== Interactions (S1 controls, primary measure) ===\n")
print(as.data.frame(
  interact_primary |> select(group, role, OR, lo, hi, p, n_group)
), row.names = FALSE)

cat("\n=== AUC ===\n")
print(as.data.frame(auc_tbl), row.names = FALSE)

# keep enriched sample for 05b visuals / further work
saveRDS(samp, file.path(derived_dir(), "lsf_real_value_controlled_sample.rds"))

progress("05 done. tables -> ", out)
cat("Wrote: tbl_rv_spec_ladder.csv, tbl_rv_joint_terms.csv, tbl_rv_interactions.csv,\n",
    "       tbl_rv_measure_sensitivity.csv, tbl_rv_auc.csv\n", sep = "")
