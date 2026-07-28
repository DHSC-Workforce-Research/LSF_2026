# ===========================================================================
# functions/analysis_panel.r
#
# analysis_panel()  -  the longitudinal panel analysis, from the student-level
# sample to tidy result tables. Moved verbatim from scripts/02_analyse.r; the
# only change is two spaces of indentation and the function wrapper. It fits
# models and writes tbl_*.csv; it draws nothing.
#
# Writes to outputs_dir():
#   tbl_or_grid.csv          predictor x outcome odds-ratio grid
#   tbl_survivorship.csv     three-spec survivorship / selection decomposition
#   tbl_auc_summary.csv      predictive discrimination (the honest ceiling)
#   tbl_auc_decile.csv       leave rate by predicted-risk decile
#   tbl_confusion.csv        confusion matrix at a prevalence-matched threshold
#   tbl_factors.csv          what drives leaving
#   tbl_group_rates.csv      leave rates by group
#   tbl_exits.csv            exit-type breakdown
#   tbl_profession.csv       outcomes by profession
#   tbl_university_spread.csv  between-provider spread
#   tbl_components.csv       grant components vs one-wave-only
#   tbl_confidence.csv       leave rate by financial confidence
#   tbl_dynamics_intention.csv  considered-leaving -> left next year
#   tbl_hazard_by_cohort.csv    dropout hazard by cohort and study year
#   tbl_retention_funnel.csv    retention funnel by course length
#   tbl_retention_by_course.csv retention funnel for the largest courses
# ===========================================================================

analysis_panel <- function() {
  set.seed(1)
  samp <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  out  <- tables_dir()
  progress("loaded analysis sample: ", nrow(samp), " students")
  samp <- samp |> mutate(crit_course = funding_imp_crse >= 4, crit_uni = funding_imp_uni >= 4)

  pull_or <- function(m, term) {
    na <- tibble(OR = NA_real_, lo = NA_real_, hi = NA_real_)
    if (is.null(m)) return(na)
    ct <- as.data.frame(fixest::coeftable(m)); cand <- c(paste0(term, "TRUE"), term)
    row <- cand[cand %in% rownames(ct)][1]
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
  prof_of <- function(x) { x <- tolower(coalesce(x, ""))
    dplyr::case_when(
      str_detect(x, "midwif") ~ "Midwifery", str_detect(x, "mental health") ~ "Nursing - Mental Health",
      str_detect(x, "learning disab") ~ "Nursing - Learning Disability", str_detect(x, "child") ~ "Nursing - Children's",
      str_detect(x, "adult") & str_detect(x, "nurs") ~ "Nursing - Adult", str_detect(x, "nurs") ~ "Nursing - other/dual",
      str_detect(x, "physio") ~ "Physiotherapy", str_detect(x, "occupational") ~ "Occupational Therapy",
      str_detect(x, "paramedic") ~ "Paramedic", str_detect(x, "radiograph|radiother") ~ "Radiography",
      str_detect(x, "speech|language") ~ "Speech & Language Therapy", str_detect(x, "diet") ~ "Dietetics",
      str_detect(x, "podiat|chiropod") ~ "Podiatry", str_detect(x, "operating department|\\bodp\\b") ~ "ODP",
      TRUE ~ "Other AHP")
  }
  preds <- tibble::tribble(
    ~label, ~term,
    "Aware of grant before applying", "fund_availability", "Grant influenced enrolment", "grant_influence",
    "Funding critical to WHAT to study", "crit_course", "Funding critical to WHERE to study", "crit_uni",
    "Grant helps me stay", "grant_helps_stay")
  outcomes <- tibble::tribble(
    ~label, ~var,
    "Left before finishing", "left_before_finish", "Claimed once only", "one_wave_only",
    "Considered leaving", "considered_leaving", "Left 2+ years early", "left_2y_plus_early")

  progress("(1) odds-ratio grid ...")
  or_grid <- tidyr::crossing(p = seq_len(nrow(preds)), o = seq_len(nrow(outcomes))) |>
    mutate(res = purrr::map2(p, o, ~ fit_or(preds$term[.x], outcomes$var[.y]))) |> tidyr::unnest(res) |>
    transmute(predictor = preds$label[p], outcome = outcomes$label[o], OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3))
  write_csv(or_grid, file.path(out, "tbl_or_grid.csv"))

  progress("(2) survivorship three-spec ...")
  surv_sample <- samp |> filter(!is.na(confidence))
  fit_spec <- function(term, data, adjust) {
    rhs <- if (adjust) paste0(term, " + i(confidence)") else term
    f <- stats::as.formula(paste0("left_before_finish ~ ", rhs, " | course + entry_year"))
    m <- tryCatch(fixest::feglm(f, family = binomial, data = data, warn = FALSE, notes = FALSE), error = function(e) NULL)
    pull_or(m, term)
  }
  survivorship <- preds |> rowwise() |>
    mutate(`All students` = list(fit_spec(term, samp, FALSE)),
           `Reached year 2` = list(fit_spec(term, surv_sample, FALSE)),
           `Year 2 + financial confidence` = list(fit_spec(term, surv_sample, TRUE))) |> ungroup() |>
    pivot_longer(c(`All students`, `Reached year 2`, `Year 2 + financial confidence`), names_to = "spec", values_to = "res") |>
    unnest(res) |> transmute(predictor = label, spec, OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3))
  write_csv(survivorship, file.path(out, "tbl_survivorship.csv"))

  progress("(3) predictive discrimination ...")
  terms_survey <- c("fund_availability", "grant_influence", "crit_course", "crit_uni", "grant_helps_stay")
  auc <- function(score, y) {
    ok <- !is.na(score) & !is.na(y); score <- score[ok]; y <- as.integer(y[ok])
    r <- rank(score); n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
    if (n1 == 0 || n0 == 0) return(NA_real_); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  auc_rows <- list(); decile_tbl <- NULL; confusion_tbl <- NULL
  for (oc in c("left_before_finish", "one_wave_only")) {
    d <- samp[, c(oc, "fund_availability", "grant_influence", "crit_course", "crit_uni", "grant_helps_stay", "entry_year")]
    d[[oc]] <- as.integer(as.logical(d[[oc]])); d <- d[stats::complete.cases(d), ]
    f_svy <- paste(oc, "~", paste(terms_survey, collapse = " + "))
    m_svy <- suppressWarnings(glm(as.formula(f_svy), data = d, family = binomial))
    m_yr  <- suppressWarnings(glm(as.formula(paste(f_svy, "+ factor(entry_year)")), data = d, family = binomial))
    auc_rows[[oc]] <- tibble(outcome = oc, n = nrow(d), base_rate = round(mean(d[[oc]]), 3),
                             auc_survey = round(auc(predict(m_svy, type = "response"), d[[oc]]), 3),
                             auc_plus_year = round(auc(predict(m_yr, type = "response"), d[[oc]]), 3))
    if (oc == "left_before_finish") {
      p_hat <- predict(m_yr, type = "response"); y <- d[[oc]]
      decile_tbl <- tibble(decile = dplyr::ntile(p_hat, 10), y = y) |>
        group_by(decile) |> summarise(n = n(), leave_rate = round(100 * mean(y), 1), .groups = "drop")
      # confusion matrix at a prevalence-matched threshold: flag exactly as many
      # students as actually leave (the model's highest-scoring npos), so the count
      # predicted to leave equals the count who do. Type I = false alarm (flagged,
      # stayed); Type II = missed leaver (not flagged, left). Same scores as the AUC.
      npos <- sum(y == 1L); N <- length(y)
      flag <- rank(-p_hat, ties.method = "first") <= npos
      tp <- sum(flag & y == 1L); fp <- sum(flag & y == 0L)
      fn <- sum(!flag & y == 1L); tn <- sum(!flag & y == 0L)
      confusion_tbl <- tibble(
        outcome = oc, n = N, base_rate = round(mean(y), 3), n_flagged = npos,
        tp = tp, fp = fp, fn = fn, tn = tn,
        sensitivity = round(tp / (tp + fn), 3), specificity = round(tn / (tn + fp), 3),
        precision   = round(tp / (tp + fp), 3), accuracy = round((tp + tn) / N, 3),
        naive_accuracy = round(max(mean(y), 1 - mean(y)), 3),
        random_tp = round(npos * mean(y)), auc = round(auc(p_hat, y), 3))
    }
  }
  write_csv(bind_rows(auc_rows), file.path(out, "tbl_auc_summary.csv"))
  write_csv(decile_tbl, file.path(out, "tbl_auc_decile.csv"))
  write_csv(confusion_tbl, file.path(out, "tbl_confusion.csv"))

  progress("(4) factors ...")
  factors_def <- tibble::tribble(
    ~label, ~term,
    "Has children (parental support)", "parental", "Funding critical to course", "crit_course",
    "Funding critical to university", "crit_uni", "Grant helps me stay", "grant_helps_stay",
    "Grant influenced enrolment", "grant_influence", "Shortage-subject payment", "specialist",
    "Regional incentive", "regional", "Aware of grant beforehand", "fund_availability")
  factors <- factors_def |> rowwise() |> mutate(res = list(fit_or(term, "left_before_finish"))) |> ungroup() |>
    unnest(res) |> transmute(factor = label, OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3)) |>
    arrange(desc(abs(log(OR))))
  write_csv(factors, file.path(out, "tbl_factors.csv"))

  progress("(5) group comparisons ...")
  samp <- samp |> mutate(conf_band = case_when(confidence <= 2 ~ "Low (1-2)", confidence == 3 ~ "Mid (3)",
                                               confidence >= 4 ~ "High (4-5)", TRUE ~ NA_character_))
  grp_rate <- function(var, label) samp |>
    filter(!is.na(left_before_finish), !is.na(.data[[var]])) |>
    group_by(group = as.character(.data[[var]])) |>
    summarise(n = n(), leave_rate = round(100 * mean(left_before_finish), 1), .groups = "drop") |>
    mutate(comparison = label) |> select(comparison, group, n, leave_rate)
  group_rates <- bind_rows(
    samp |> filter(!is.na(left_before_finish)) |> summarise(comparison = "Overall", group = "All students", n = n(), leave_rate = round(100 * mean(left_before_finish), 1)),
    grp_rate("fund_availability", "Aware of grant beforehand"), grp_rate("parental", "Has children"),
    grp_rate("crit_course", "Funding critical to course"), grp_rate("conf_band", "Financial confidence"))
  write_csv(group_rates, file.path(out, "tbl_group_rates.csv"))

  progress("(6) exit breakdown ...")
  traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
  exits <- traj |> count(outcome_cautious, name = "n") |> mutate(share = round(100 * n / sum(n), 1)) |> arrange(desc(n))
  write_csv(exits, file.path(out, "tbl_exits.csv"))

  progress("(7) profession / university / components / confidence ...")
  samp$prof <- prof_of(samp$course)
  profession <- samp |> group_by(prof) |>
    summarise(n = n(), left_before_finish = round(100 * mean(left_before_finish, na.rm = TRUE), 1),
              one_wave_only = round(100 * mean(one_wave_only, na.rm = TRUE), 1),
              considered_leaving = round(100 * mean(considered_leaving, na.rm = TRUE), 1), .groups = "drop") |>
    arrange(desc(left_before_finish))
  write_csv(profession, file.path(out, "tbl_profession.csv"))
  uni_spread <- function(v) {
    t <- samp |> filter(!is.na(.data[[v]]), !is.na(college)) |> group_by(college) |>
      summarise(n = n(), r = 100 * mean(.data[[v]]), .groups = "drop") |> filter(n >= 200)
    tibble(measure = v, n_providers = nrow(t), p10 = round(quantile(t$r, .10), 1), median = round(quantile(t$r, .50), 1),
           p90 = round(quantile(t$r, .90), 1), sd_between = round(sd(t$r), 1))
  }
  write_csv(bind_rows(uni_spread("left_before_finish"), uni_spread("one_wave_only")), file.path(out, "tbl_university_spread.csv"))
  comp_or <- bind_rows(
    fit_or("parental", "one_wave_only", "entry_year") |> mutate(component = "Parental", adjust = "cohort"),
    fit_or("specialist", "one_wave_only", "entry_year") |> mutate(component = "Specialist", adjust = "cohort"),
    fit_or("regional", "one_wave_only", "entry_year") |> mutate(component = "Regional", adjust = "cohort"),
    fit_or("parental", "one_wave_only", "course + entry_year") |> mutate(component = "Parental", adjust = "cohort+subject"),
    fit_or("specialist", "one_wave_only", "course + entry_year") |> mutate(component = "Specialist", adjust = "cohort+subject"),
    fit_or("regional", "one_wave_only", "course + entry_year") |> mutate(component = "Regional", adjust = "cohort+subject")) |>
    transmute(component, adjust, OR = round(OR, 3), lo = round(lo, 3), hi = round(hi, 3))
  write_csv(comp_or, file.path(out, "tbl_components.csv"))
  confidence <- samp |> filter(!is.na(confidence), !is.na(left_before_finish)) |> group_by(confidence) |>
    summarise(n = n(), leave_rate = round(100 * mean(left_before_finish), 1), .groups = "drop") |> arrange(confidence)
  write_csv(confidence, file.path(out, "tbl_confidence.csv"))

  # --- (8) dynamics: intention transition + dropout hazard by cohort ---
  progress("(8) dynamics ...")
  anchor <- samp |> select(UniqueID, course_first_year_wave, expected_finish, last_wave)
  lw <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
                 col_select = c(UniqueID, year, first_year, leave_course), show_col_types = FALSE) |>
    left_join(anchor, by = "UniqueID") |>
    mutate(study_year = year - course_first_year_wave + 1L,
           at_risk    = !is.na(expected_finish) & year < expected_finish & year <= 2024L,
           left_next  = at_risk & year == last_wave)      # never seen again = dropped
  intention <- lw |> filter(at_risk, first_year == FALSE, !is.na(leave_course)) |>
    group_by(considered_leaving = leave_course) |>
    summarise(n = n(), left_next_pct = round(100 * mean(left_next), 1), .groups = "drop")
  write_csv(intention, file.path(out, "tbl_dynamics_intention.csv"))
  hazard <- lw |> filter(at_risk) |>
    group_by(cohort = course_first_year_wave, study_year) |>
    summarise(n = n(), hazard_pct = round(100 * mean(left_next), 1), .groups = "drop") |>
    filter(n >= 200) |> arrange(cohort, study_year)
  write_csv(hazard, file.path(out, "tbl_hazard_by_cohort.csv"))


  # --- (9) retention funnel: share of starters still enrolled each study year ---
  # By course length, using only cohorts old enough to be observed to their final
  # year within the data. "Still enrolled in year k" = last claim in year k or later.
  progress("(9) retention funnel ...")
  surv_base <- samp |>
    mutate(length = expected_finish - course_first_year_wave + 1L,
           last_sy = last_wave - course_first_year_wave + 1L,
           cohort  = course_first_year_wave)
  build_surv <- function(L) {
    d <- surv_base |> filter(length == L, cohort >= 2021L, cohort <= 2025L - (L - 1L))
    tibble(length_years = L, study_year = 1:L,
           survival_pct = round(100 * sapply(1:L, function(k) mean(d$last_sy >= k, na.rm = TRUE)), 1),
           starters = nrow(d))
  }
  retention <- bind_rows(build_surv(3L), build_surv(4L))
  write_csv(retention, file.path(out, "tbl_retention_funnel.csv"))
  progress("done (retention funnel added).")

  # --- (10) retention funnel by course (largest courses, observed) -----
  progress("(10) retention by course ...")
  course_len <- surv_base |> filter(!is.na(length), length %in% c(3L, 4L)) |>
    group_by(course) |> summarise(L = as.integer(median(length)), n = n(), .groups = "drop")
  top_courses <- course_len |> slice_max(n, n = 8, with_ties = FALSE) |> pull(course)
  build_course_surv <- function(crs) {
    L <- course_len$L[course_len$course == crs]
    d <- surv_base |> filter(course == crs, cohort >= 2021L, cohort <= 2025L - (L - 1L))
    tibble(course = crs, length_years = L, starters = nrow(d), study_year = 1:L,
           survival_pct = round(100 * sapply(1:L, function(k) mean(d$last_sy >= k, na.rm = TRUE)), 1))
  }
  retention_course <- bind_rows(lapply(top_courses, build_course_surv))
  write_csv(retention_course, file.path(out, "tbl_retention_by_course.csv"))
  progress("done (retention by course added).")

  progress("done. tables written to ", out)
  invisible(TRUE)
}
