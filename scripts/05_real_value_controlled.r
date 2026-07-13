# ===========================================================================
# scripts/05_real_value_controlled.r
#
# After 04 showed local real value of the LSF predicts leaving, this script:
#   1. Does real value still predict leaving controlling for survey funding items?
#   2. How does it compare (joint model + AUC)?
#   3. Robust across leaving definitions?
#   4. Does it matter MORE for parental / specialist recipients (interactions)?
#
# ASSOCIATIONAL only. Run after 01 (needs lsf_analysis_sample.rds).
#   source("scripts/05_real_value_controlled.r")
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(purrr); library(fixest); library(tibble)
})
set.seed(1)

# ---- CONFIG ----------------------------------------------------------------
REF_DIR     <- "reference"
PRIMARY     <- "real_value_rent_ttwa"
PRIMARY_LBL <- "Rent-adjusted (TTWA)"
FE          <- "course + entry_year"

OUTCOMES <- tibble::tibble(
  label = c("Left before finishing", "Claimed once only",
            "Left 2+ years early", "Considered leaving"),
  var   = c("left_before_finish", "one_wave_only",
            "left_2y_plus_early", "considered_leaving")
)
MEASURES <- c(
  real_value_cpih      = "Inflation-only (CPIH)",
  real_value_rent      = "Rent-adjusted (LAD)",
  real_value_hp        = "House-price-adjusted (LAD)",
  real_value_rent_ttwa = "Rent-adjusted (TTWA)",
  real_value_hp_ttwa   = "House-price-adjusted (TTWA)"
)
SURVEY_VARS <- c("fund_availability", "grant_influence", "crit_course",
                 "crit_uni", "grant_helps_stay")
SURVEY_RHS  <- paste(SURVEY_VARS, collapse = " + ")
COMP_VARS   <- c("parental", "specialist", "regional")
COMP_RHS    <- paste(COMP_VARS, collapse = " + ")

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

na_or_row <- function(term = "rv", ...) {
  tibble::tibble(term = term, OR = NA_real_, lo = NA_real_, hi = NA_real_,
                 p = NA_real_, ...)
}

# ---------------------------------------------------------------------------
out <- outputs_dir()
progress(paste0("05: loading analysis sample ..."))
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

progress(paste0("05: building real-value measures (parent flag = ", RV_PARENT, ") ..."))
t0 <- Sys.time()
samp <- build_real_value(SAMPLE, ref, awards, cpih, base_year = 2020)

# --- type coercion (this is what usually kills fixest with cryptic c() errors) -
# Prefer the analysis-sample parental/specialist flags; fall back to has_parent.
if (!"parental"   %in% names(samp)) samp$parental   <- samp$has_parent == 1L
if (!"specialist" %in% names(samp)) samp$specialist <- FALSE
if (!"regional"   %in% names(samp)) samp$regional   <- FALSE

samp <- samp |>
  mutate(
    crit_course = as.integer(suppressWarnings(as.integer(funding_imp_crse)) >= 4L),
    crit_uni    = as.integer(suppressWarnings(as.integer(funding_imp_uni))  >= 4L),
    parental    = to_01(parental),
    specialist  = to_01(specialist),
    regional    = to_01(regional),
    fund_availability = to_01(fund_availability),
    grant_influence   = to_01(grant_influence),
    grant_helps_stay  = to_01(grant_helps_stay),
    # outcomes to 0/1 integer
    left_before_finish  = to_01(left_before_finish),
    one_wave_only       = to_01(one_wave_only),
    left_2y_plus_early  = to_01(left_2y_plus_early),
    considered_leaving  = to_01(considered_leaving),
    # FE as character then factor later in fit
    course     = as.character(course),
    entry_year = as.integer(entry_year),
    confidence = suppressWarnings(as.integer(confidence)),
    rv   = as.numeric(scale(.data[[PRIMARY]])),
    rv_k = as.numeric(.data[[PRIMARY]]) / 1000,
    has_parent = as.integer(coalesce(parental, 0L))
  )

message(sprintf(
  "  done in %.1fs | coverage rent_ttwa %.1f%% | n=%s | parents=%s | specialists=%s",
  as.numeric(Sys.time() - t0, units = "secs"),
  100 * mean(!is.na(samp[[PRIMARY]])),
  format(nrow(samp), big.mark = ","),
  format(sum(samp$parental == 1L, na.rm = TRUE), big.mark = ","),
  format(sum(samp$specialist == 1L, na.rm = TRUE), big.mark = ",")
))

# ---------------------------------------------------------------------------
# (1) Spec ladder
# ---------------------------------------------------------------------------
progress("05: (1) spec ladder ...")

spec_def <- tibble::tibble(
  spec = c("S0", "S1", "S2", "S3", "S0k", "S1k"),
  label = c(
    "Real value only",
    "Real value + survey funding",
    "S1 + grant components",
    "S1 + financial confidence (year-2 only)",
    "Real value only (per £1,000)",
    "Real value + survey (per £1,000)"
  ),
  rhs = c(
    "rv",
    paste("rv", SURVEY_RHS, sep = " + "),
    paste("rv", SURVEY_RHS, COMP_RHS, sep = " + "),
    paste("rv", SURVEY_RHS, "factor(confidence)", sep = " + "),
    "rv_k",
    paste("rv_k", SURVEY_RHS, sep = " + ")
  ),
  survivors_only = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  term = c("rv", "rv", "rv", "rv", "rv_k", "rv_k")
)

# variables that must be non-missing for each spec (beyond outcome + term)
spec_need <- list(
  S0  = character(0),
  S1  = SURVEY_VARS,
  S2  = c(SURVEY_VARS, COMP_VARS),
  S3  = c(SURVEY_VARS, "confidence"),
  S0k = character(0),
  S1k = SURVEY_VARS
)

ladder_one <- function(i) {
  spec_row <- spec_def[i, ]
  out_row  <- OUTCOMES[((i - 1L) %% nrow(OUTCOMES)) + 1L, ]
  # rebuild index: we will call via expand grid ids instead
  NULL
}

# explicit expand grid so index in errors is human-readable
ladder_grid <- tidyr::expand_grid(
  spec_i = seq_len(nrow(spec_def)),
  out_i  = seq_len(nrow(OUTCOMES))
) |>
  mutate(
    spec = spec_def$spec[spec_i],
    outcome_var = OUTCOMES$var[out_i],
    outcome = OUTCOMES$label[out_i]
  )

fit_ladder_row <- function(spec, outcome_var, outcome) {
  sdef <- spec_def[spec_def$spec == spec, ]
  term <- sdef$term[[1]]
  rhs  <- sdef$rhs[[1]]
  need <- unique(c(outcome_var, term, "course", "entry_year", spec_need[[spec]]))

  d <- samp
  if (isTRUE(sdef$survivors_only[[1]])) d <- d[!is.na(d$confidence), , drop = FALSE]

  # complete cases on required cols only
  ok <- rep(TRUE, nrow(d))
  for (v in need) {
    if (!v %in% names(d)) {
      message("ladder missing column ", v, " for ", spec, " / ", outcome_var)
      return(na_or_row(term,
        spec = spec, spec_label = sdef$label[[1]],
        outcome = outcome, outcome_var = outcome_var,
        measure = PRIMARY_LBL,
        scale = if (term == "rv_k") "per £1000" else "per 1 SD",
        n = 0L
      ))
    }
    ok <- ok & !is.na(d[[v]])
  }
  d <- d[ok, , drop = FALSE]
  n <- nrow(d)

  if (n < 50L) {
    message("ladder skip (n=", n, "): ", spec, " / ", outcome_var)
    return(na_or_row(term,
      spec = spec, spec_label = sdef$label[[1]],
      outcome = outcome, outcome_var = outcome_var,
      measure = PRIMARY_LBL,
      scale = if (term == "rv_k") "per £1000" else "per 1 SD",
      n = n
    ))
  }

  message("  fitting ", spec, " ~ ", outcome_var, " (n=", format(n, big.mark = ","), ") ...")
  m <- fit_feglm(d, outcome_var, rhs, FE)
  pull_or(m, term) |>
    transmute(
      spec = spec,
      spec_label = sdef$label[[1]],
      outcome = outcome,
      outcome_var = outcome_var,
      measure = PRIMARY_LBL,
      scale = if (term == "rv_k") "per £1000" else "per 1 SD",
      term, OR, lo, hi, p,
      n = n
    )
}

ladder <- purrr::pmap_dfr(
  list(ladder_grid$spec, ladder_grid$outcome_var, ladder_grid$outcome),
  function(spec, outcome_var, outcome) {
    tryCatch(
      fit_ladder_row(spec, outcome_var, outcome),
      error = function(e) {
        message("LADDER ERROR [", spec, " / ", outcome_var, "]: ", conditionMessage(e))
        na_or_row("rv",
          spec = spec, spec_label = spec,
          outcome = outcome, outcome_var = outcome_var,
          measure = PRIMARY_LBL, scale = "per 1 SD", n = NA_integer_
        )
      }
    )
  }
) |>
  mutate(
    across(c(OR, lo, hi), ~ suppressWarnings(round(as.numeric(.x), 3))),
    p = suppressWarnings(signif(as.numeric(p), 3))
  )

write_csv(ladder, file.path(out, "tbl_rv_spec_ladder.csv"))
progress(paste0("  wrote tbl_rv_spec_ladder.csv (", nrow(ladder), " rows)"))

# ---------------------------------------------------------------------------
# (2) Joint horse-race (S1, primary outcome)
# ---------------------------------------------------------------------------
progress("05: (2) joint terms (S1 horse race) ...")

joint_terms  <- c("rv", SURVEY_VARS)
joint_labels <- c(
  rv = "Real value of LSF (per 1 SD, rent TTWA)",
  fund_availability = "Aware of grant before applying",
  grant_influence = "Grant influenced enrolment",
  crit_course = "Funding critical to WHAT to study",
  crit_uni = "Funding critical to WHERE to study",
  grant_helps_stay = "Grant helps me stay"
)

need_j <- c("left_before_finish", "rv", "course", "entry_year", SURVEY_VARS)
ok_j <- Reduce(`&`, lapply(need_j, function(v) !is.na(samp[[v]])))
d_joint <- samp[ok_j, , drop = FALSE]
message("  joint n=", format(nrow(d_joint), big.mark = ","))

m_joint <- fit_feglm(d_joint, "left_before_finish", paste("rv", SURVEY_RHS, sep = " + "), FE)
joint <- purrr::map_dfr(joint_terms, function(tm) {
  pull_or(m_joint, tm) |>
    mutate(
      label = unname(joint_labels[[tm]] %||% tm),
      n = nrow(d_joint)
    )
}) |>
  mutate(
    across(c(OR, lo, hi), ~ suppressWarnings(round(as.numeric(.x), 3))),
    p = suppressWarnings(signif(as.numeric(p), 3))
  ) |>
  arrange(desc(abs(log(pmax(OR, 1e-9)))))

write_csv(joint, file.path(out, "tbl_rv_joint_terms.csv"))
progress("  wrote tbl_rv_joint_terms.csv")

# ---------------------------------------------------------------------------
# (3) Interactions: parent and specialist separately
# ---------------------------------------------------------------------------
progress("05: (3) parent + specialist interactions ...")

fit_interact <- function(group, measure_col, measure_label, with_survey = TRUE) {
  d <- samp
  d$rv <- as.numeric(scale(d[[measure_col]]))
  need <- c("left_before_finish", "rv", "course", "entry_year", group)
  if (with_survey) need <- c(need, SURVEY_VARS)
  ok <- Reduce(`&`, lapply(need, function(v) !is.na(d[[v]])))
  d <- d[ok, , drop = FALSE]
  n <- nrow(d)
  n_group <- sum(d[[group]] == 1L, na.rm = TRUE)

  if (n < 50L || n_group < 30L) {
    message("interact skip ", group, " / ", measure_col, " n=", n, " n_group=", n_group)
    return(bind_rows(
      na_or_row("rv", role = "real value (main)", group = group,
                measure = measure_label, measure_col = measure_col,
                controls = if (with_survey) "S1 survey + interaction" else "none (S0 + interaction)",
                n = n, n_group = n_group),
      na_or_row(group, role = paste0(group, " main"), group = group,
                measure = measure_label, measure_col = measure_col,
                controls = if (with_survey) "S1 survey + interaction" else "none (S0 + interaction)",
                n = n, n_group = n_group),
      na_or_row(paste0("rv:", group), role = paste0("rv x ", group, " (does it matter MORE?)"),
                group = group, measure = measure_label, measure_col = measure_col,
                controls = if (with_survey) "S1 survey + interaction" else "none (S0 + interaction)",
                n = n, n_group = n_group)
    ))
  }

  # integer 0/1 group keeps fixest coef names clean: rv:parental
  rhs <- if (with_survey) {
    paste0("rv + ", SURVEY_RHS, " + ", group, " + rv:", group)
  } else {
    paste0("rv + ", group, " + rv:", group)
  }
  controls <- if (with_survey) "S1 survey + interaction" else "none (S0 + interaction)"
  message("  interact ", group, " / ", measure_label, " (", controls, ", n=", n, ")")
  m <- fit_feglm(d, "left_before_finish", rhs, FE)

  bind_rows(
    pull_or(m, "rv") |> mutate(role = "real value (main, group=0)"),
    pull_or(m, group) |> mutate(role = paste0(group, " main")),
    pull_or(m, paste0("rv:", group)) |>
      mutate(role = paste0("rv x ", group, " (does it matter MORE?)"))
  ) |>
    mutate(
      group = group, measure = measure_label, measure_col = measure_col,
      controls = controls, n = n, n_group = n_group,
      across(c(OR, lo, hi), ~ suppressWarnings(round(as.numeric(.x), 3))),
      p = suppressWarnings(signif(as.numeric(p), 3))
    )
}

interact_primary <- bind_rows(
  fit_interact("parental",   PRIMARY, PRIMARY_LBL, TRUE),
  fit_interact("specialist", PRIMARY, PRIMARY_LBL, TRUE)
)

interact_s0 <- tidyr::expand_grid(
  group = c("parental", "specialist"),
  measure_col = names(MEASURES)
) |>
  purrr::pmap_dfr(function(group, measure_col) {
    tryCatch(
      fit_interact(group, measure_col, MEASURES[[measure_col]], with_survey = FALSE),
      error = function(e) {
        message("INTERACT S0 ERROR [", group, " / ", measure_col, "]: ", conditionMessage(e))
        na_or_row("rv", role = "error", group = group, measure = MEASURES[[measure_col]],
                  measure_col = measure_col, controls = "none (S0 + interaction)",
                  n = NA_integer_, n_group = NA_integer_)
      }
    )
  })

interact_all <- bind_rows(interact_primary, interact_s0)
write_csv(interact_all, file.path(out, "tbl_rv_interactions.csv"))
progress("  wrote tbl_rv_interactions.csv")

# ---------------------------------------------------------------------------
# (4) Measure sensitivity S0 / S1
# ---------------------------------------------------------------------------
progress("05: (4) measure sensitivity (S0/S1 x 5 measures) ...")

meas_one <- function(measure_col, measure_label, controls) {
  d <- samp
  d$rv <- as.numeric(scale(d[[measure_col]]))
  need <- c("left_before_finish", "rv", "course", "entry_year")
  if (controls == "S1") need <- c(need, SURVEY_VARS)
  ok <- Reduce(`&`, lapply(need, function(v) !is.na(d[[v]])))
  d <- d[ok, , drop = FALSE]
  rhs <- if (controls == "S0") "rv" else paste("rv", SURVEY_RHS, sep = " + ")
  message("  measure ", controls, " / ", measure_label, " n=", nrow(d))
  m <- fit_feglm(d, "left_before_finish", rhs, FE)
  pull_or(m, "rv") |>
    transmute(
      controls, measure = measure_label, measure_col,
      OR, lo, hi, p, n = nrow(d)
    )
}

meas_sens <- tidyr::expand_grid(
  measure_col = names(MEASURES),
  controls = c("S0", "S1")
) |>
  purrr::pmap_dfr(function(measure_col, controls) {
    tryCatch(
      meas_one(measure_col, MEASURES[[measure_col]], controls),
      error = function(e) {
        message("MEASURE ERROR [", controls, " / ", measure_col, "]: ", conditionMessage(e))
        tibble(controls = controls, measure = MEASURES[[measure_col]],
               measure_col = measure_col, OR = NA_real_, lo = NA_real_,
               hi = NA_real_, p = NA_real_, n = NA_integer_)
      }
    )
  }) |>
  mutate(
    across(c(OR, lo, hi), ~ suppressWarnings(round(as.numeric(.x), 3))),
    p = suppressWarnings(signif(as.numeric(p), 3))
  )

write_csv(meas_sens, file.path(out, "tbl_rv_measure_sensitivity.csv"))
progress("  wrote tbl_rv_measure_sensitivity.csv")

# ---------------------------------------------------------------------------
# (5) AUC
# ---------------------------------------------------------------------------
progress("05: (5) AUC horse race ...")

auc_for_outcome <- function(oc) {
  cols <- c(oc, "rv", SURVEY_VARS, "entry_year")
  d <- samp[, cols]
  d[[oc]] <- to_01(d[[oc]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 100L) {
    return(tibble(outcome = oc, n = nrow(d), base_rate = NA_real_,
                  auc_survey = NA_real_, auc_rv = NA_real_, auc_combined = NA_real_))
  }
  # plain glm (no fixest) for scores — same as 02
  f_svy  <- paste(oc, "~", SURVEY_RHS, "+ factor(entry_year)")
  f_rv   <- paste(oc, "~ rv + factor(entry_year)")
  f_both <- paste(oc, "~ rv +", SURVEY_RHS, "+ factor(entry_year)")
  m_svy  <- suppressWarnings(stats::glm(stats::as.formula(f_svy),  data = d, family = binomial()))
  m_rv   <- suppressWarnings(stats::glm(stats::as.formula(f_rv),   data = d, family = binomial()))
  m_both <- suppressWarnings(stats::glm(stats::as.formula(f_both), data = d, family = binomial()))
  tibble(
    outcome = oc, n = nrow(d), base_rate = round(mean(d[[oc]]), 3),
    auc_survey   = round(auc_score(stats::predict(m_svy,  type = "response"), d[[oc]]), 3),
    auc_rv       = round(auc_score(stats::predict(m_rv,   type = "response"), d[[oc]]), 3),
    auc_combined = round(auc_score(stats::predict(m_both, type = "response"), d[[oc]]), 3)
  )
}

auc_tbl <- purrr::map_dfr(c("left_before_finish", "one_wave_only"), function(oc) {
  tryCatch(auc_for_outcome(oc), error = function(e) {
    message("AUC ERROR [", oc, "]: ", conditionMessage(e))
    tibble(outcome = oc, n = NA_integer_, base_rate = NA_real_,
           auc_survey = NA_real_, auc_rv = NA_real_, auc_combined = NA_real_)
  })
})
write_csv(auc_tbl, file.path(out, "tbl_rv_auc.csv"))
progress("  wrote tbl_rv_auc.csv")

# ---------------------------------------------------------------------------
# Console headline
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

saveRDS(samp, file.path(derived_dir(), "lsf_real_value_controlled_sample.rds"))
progress(paste0("05 done. tables -> ", out))
cat("Wrote: tbl_rv_spec_ladder.csv, tbl_rv_joint_terms.csv, tbl_rv_interactions.csv,\n",
    "       tbl_rv_measure_sensitivity.csv, tbl_rv_auc.csv\n", sep = "")
