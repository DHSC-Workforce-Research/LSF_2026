# ===========================================================================
# scripts/06_findings_pack.r
#
# ONE script for Lee -> produces a small findings pack of NUMBERS (no raw
# student data) so we can interpret results cleanly.
#
# What it answers
# ---------------
# A. STUDENT-LEVEL (current design, one row per student)
#    - Real LSF at ENTRY (college + entry_year) -> leaving definitions
#    - Scaled so you can say: "£1,000 less real LSF -> X% higher odds of leaving"
#    - Also: £500 / £1 SD for the same models
#
# B. STUDENT-YEAR PANEL (longitudinal)
#    - Real LSF attached to EACH survey wave (college + wave year)
#    - Which year's real LSF "counts" for exit?
#        * entry_wave real LSF  (start of training)
#        * last_wave real LSF   (when last seen)
#        * this_wave real LSF   (hazard: leave next year given this year)
#    - Does real LSF in year t predict:
#        * confidence in year t+1
#        * considered leaving in year t / t+1
#        * gone next year (left_next)
#
# Outputs (AGGREGATE only -> outputs_dir()/findings_pack_YYYYMMDD/)
#   00_README.txt
#   A_student_level_pound_effects.csv
#   A_student_level_spec_ladder.csv   (if 05 already ran, also copies)
#   B_panel_which_year_counts.csv
#   B_panel_lags.csv
#   B_panel_descriptives.csv
#   findings_numbers.txt             <- plain English numbers to paste/email
#
# Run from repo root (after 01 at least; 05 optional):
#   source("scripts/06_findings_pack.r")
#
# ASSOCIATIONAL only. No student-level data is written.
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(purrr); library(fixest); library(tibble)
})
set.seed(1)

# ---- CONFIG ----------------------------------------------------------------
REF_DIR     <- "reference"
PRIMARY     <- "real_value_rent_ttwa_cpih"   # headline: CPIH x local rent (TTWA)
PRIMARY_LBL <- "Weighted CoL rent+CPI (TTWA)"
FE_STUDENT  <- "course + entry_year"
FE_PANEL    <- "course + year"                # wave FE on panel models
POUND_STEPS <- c(500, 1000)                   # "£X less real LSF" scenarios
SURVEY_VARS <- c("fund_availability", "grant_influence", "crit_course",
                 "crit_uni", "grant_helps_stay")
# ---------------------------------------------------------------------------

stamp <- format(Sys.Date(), "%Y%m%d")
pack  <- file.path(outputs_dir(), paste0("findings_pack_", stamp))
dir.create(pack, showWarnings = FALSE, recursive = TRUE)

cat_both <- function(...) {
  msg <- paste0(...)
  message(msg)
  cat(msg, "\n", file = file.path(pack, "findings_numbers.txt"), append = TRUE)
}

writeLines(character(0), file.path(pack, "findings_numbers.txt"))  # reset

# ---- helpers ---------------------------------------------------------------
or_from_b <- function(b, se) {
  tibble(OR = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))
}

# fitest coef for a named term; return b, se, p, n
coef_row <- function(m, term) {
  if (is.null(m)) return(tibble(term = term, b = NA_real_, se = NA_real_, p = NA_real_))
  ct <- tryCatch(as.data.frame(fixest::coeftable(m)), error = function(e) NULL)
  if (is.null(ct) || !nrow(ct)) return(tibble(term = term, b = NA_real_, se = NA_real_, p = NA_real_))
  rn <- rownames(ct)
  cand <- c(term, paste0(term, "TRUE"), paste0("`", term, "`"))
  row <- cand[cand %in% rn][1]
  if (is.na(row)) {
    hit <- which(startsWith(rn, term))
    if (length(hit)) row <- rn[hit[1]] else
      return(tibble(term = term, b = NA_real_, se = NA_real_, p = NA_real_))
  }
  pcol <- intersect(c("Pr(>|z|)", "Pr(>|t|)"), names(ct))
  tibble(
    term = term,
    b  = ct[row, "Estimate"],
    se = ct[row, "Std. Error"],
    p  = if (length(pcol)) ct[row, pcol[1]] else NA_real_
  )
}

# Translate a log-odds slope on £-level real value into "£X less -> OR"
# If model is y ~ rv_pound (real value in pounds), then £X LESS real value
# multiplies odds by exp(-X * b). Report as percent higher odds if >1.
pound_effect <- function(b, se, pounds_less = 1000) {
  # effect of REDUCING real value by pounds_less
  bb <- -pounds_less * b
  ss <- pounds_less * se
  or <- exp(bb)
  tibble(
    pounds_less = pounds_less,
    OR_if_reduced = or,
    lo = exp(bb - 1.96 * ss),
    hi = exp(bb + 1.96 * ss),
    pct_higher_odds = 100 * (or - 1)   # positive = more leaving when grant thinner
  )
}

fit_bin <- function(data, y, rhs, fe) {
  data <- as.data.frame(data)
  for (v in c("course", "entry_year", "year")) {
    if (v %in% names(data)) data[[v]] <- factor(data[[v]])
  }
  f <- tryCatch(stats::as.formula(paste0(y, " ~ ", rhs, " | ", fe)), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  tryCatch(
    fixest::feglm(f, family = "binomial", data = data, warn = FALSE, notes = FALSE),
    error = function(e) { message("fit fail: ", y, " ~ ", rhs, " | ", fe, " - ", conditionMessage(e)); NULL }
  )
}

# linear FE for confidence (1-5)
fit_lin <- function(data, y, rhs, fe) {
  data <- as.data.frame(data)
  for (v in c("course", "entry_year", "year")) {
    if (v %in% names(data)) data[[v]] <- factor(data[[v]])
  }
  f <- tryCatch(stats::as.formula(paste0(y, " ~ ", rhs, " | ", fe)), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  tryCatch(
    fixest::feols(f, data = data, warn = FALSE, notes = FALSE),
    error = function(e) { message("fit fail lin: ", conditionMessage(e)); NULL }
  )
}

# ---- load -----------------------------------------------------------------
progress("06: load sample + reference ...")
SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

# ===========================================================================
# A. STUDENT-LEVEL: entry real LSF -> leaving, in £ units
# ===========================================================================
progress("06 A: student-level entry real LSF (£ effects) ...")

# force entry-year anchor (column map in real_value.r)
RV_YEAR_SAVE <- RV_YEAR
# already entry_year by default
stud <- build_real_value(SAMPLE, ref, awards, cpih, base_year = 2020)
if (!"parental" %in% names(stud)) stud$parental <- stud$has_parent == 1L
if (!"specialist" %in% names(stud)) stud$specialist <- FALSE

stud <- stud |>
  mutate(
    crit_course = as.integer(suppressWarnings(as.integer(funding_imp_crse)) >= 4L),
    crit_uni    = as.integer(suppressWarnings(as.integer(funding_imp_uni))  >= 4L),
    across(any_of(c("fund_availability", "grant_influence", "grant_helps_stay",
                    "parental", "specialist", "left_before_finish", "one_wave_only",
                    "left_2y_plus_early", "considered_leaving")), to_01),
    course = as.character(course),
    entry_year = as.integer(entry_year),
    # continuous £ real value (headline measure)
    rv_gbp = as.numeric(.data[[PRIMARY]]),
    rv_sd  = as.numeric(scale(rv_gbp)),
    rv_k   = rv_gbp / 1000
  )

sd_gbp <- stats::sd(stud$rv_gbp, na.rm = TRUE)
mean_gbp <- mean(stud$rv_gbp, na.rm = TRUE)
p10 <- stats::quantile(stud$rv_gbp, 0.10, na.rm = TRUE)
p90 <- stats::quantile(stud$rv_gbp, 0.90, na.rm = TRUE)

cat_both("=== A. STUDENT-LEVEL (entry real LSF, ", PRIMARY_LBL, ") ===")
cat_both("HEADLINE construct: nominal / [ w*(local_rent/nat_rent_2020) + (1-w)*(CPI/CPI_2020) ], w=0.5")
cat_both("(Budget-weighted cost-of-living deflator; housing counted once at weight w; CPI excludes owner-occupier housing.)")
cat_both(sprintf("Real LSF £distribution: mean=%.0f  SD=%.0f  p10=%.0f  p90=%.0f",
                 mean_gbp, sd_gbp, p10, p90))
cat_both(sprintf("So 1 SD ~ £%.0f of real grant value (weighted rent+CPI, TTWA).", sd_gbp))
if (all(c("real_value_rent_ttwa", "real_value_cpih") %in% names(stud))) {
  cat_both(sprintf(
    "Compare means: rent-only=%.0f | CPIH-only=%.0f | weighted rent+CPI (headline)=%.0f",
    mean(stud$real_value_rent_ttwa, na.rm = TRUE),
    mean(stud$real_value_cpih, na.rm = TRUE),
    mean_gbp
  ))
}
cat_both("")

outcomes_a <- tibble::tibble(
  outcome = c("left_before_finish", "one_wave_only", "left_2y_plus_early", "considered_leaving"),
  label   = c("Left before finishing", "Claimed once only", "Left 2+ years early", "Considered leaving")
)
specs_a <- tibble::tibble(
  spec = c("S0_gbp", "S1_gbp", "S0_sd", "S1_sd"),
  rhs  = c(
    "rv_gbp",
    paste("rv_gbp", paste(SURVEY_VARS, collapse = " + "), sep = " + "),
    "rv_sd",
    paste("rv_sd", paste(SURVEY_VARS, collapse = " + "), sep = " + ")
  ),
  scale = c("£ level", "£ level + survey", "per 1 SD", "per 1 SD + survey"),
  term  = c("rv_gbp", "rv_gbp", "rv_sd", "rv_sd")
)

rows_a <- list()
for (i in seq_len(nrow(outcomes_a))) {
  for (j in seq_len(nrow(specs_a))) {
    y <- outcomes_a$outcome[i]
    sp <- specs_a[j, ]
    need <- unique(c(y, sp$term, "course", "entry_year",
                     if (grepl("survey", sp$scale)) SURVEY_VARS))
    ok <- Reduce(`&`, lapply(need, function(v) !is.na(stud[[v]])))
    d <- stud[ok, , drop = FALSE]
    m <- fit_bin(d, y, sp$rhs, FE_STUDENT)
    cr <- coef_row(m, sp$term)
    base <- tibble(
      frame = "student_entry",
      outcome = y, outcome_label = outcomes_a$label[i],
      spec = sp$spec, scale = sp$scale, term = sp$term,
      n = nrow(d), b = cr$b, se = cr$se, p = cr$p
    )
    if (sp$term == "rv_gbp" && is.finite(cr$b)) {
      pe <- purrr::map_dfr(POUND_STEPS, ~ pound_effect(cr$b, cr$se, .x))
      pe <- pe |> mutate(
        frame = base$frame, outcome = y, outcome_label = outcomes_a$label[i],
        spec = sp$spec, scale = sp$scale, n = nrow(d), b = cr$b, se = cr$se, p = cr$p,
        sd_gbp = sd_gbp, mean_gbp = mean_gbp
      )
      rows_a[[length(rows_a) + 1L]] <- pe
      # plain English for primary outcome S1
      if (y == "left_before_finish" && sp$spec == "S1_gbp") {
        for (k in seq_len(nrow(pe))) {
          cat_both(sprintf(
            "LEFT BEFORE FINISH | S1 (survey controls) | £%s LESS real LSF -> odds of leaving x %.3f (%.3f-%.3f) i.e. about %+.1f%% on the odds | n=%s | p=%.3g",
            format(pe$pounds_less[k], big.mark = ","),
            pe$OR_if_reduced[k], pe$lo[k], pe$hi[k],
            pe$pct_higher_odds[k],
            format(nrow(d), big.mark = ","), cr$p
          ))
        }
      }
      if (y == "left_before_finish" && sp$spec == "S0_gbp") {
        for (k in seq_len(nrow(pe))) {
          cat_both(sprintf(
            "LEFT BEFORE FINISH | S0 (real value only) | £%s LESS real LSF -> odds x %.3f (%.3f-%.3f) ~ %+.1f%% on odds | n=%s | p=%.3g",
            format(pe$pounds_less[k], big.mark = ","),
            pe$OR_if_reduced[k], pe$lo[k], pe$hi[k],
            pe$pct_higher_odds[k],
            format(nrow(d), big.mark = ","), cr$p
          ))
        }
      }
    } else {
      o <- or_from_b(cr$b, cr$se)
      rows_a[[length(rows_a) + 1L]] <- base |>
        mutate(pounds_less = NA_real_, OR_if_reduced = o$OR, lo = o$lo, hi = o$hi,
               pct_higher_odds = 100 * (o$OR - 1),
               sd_gbp = sd_gbp, mean_gbp = mean_gbp)
      if (y == "left_before_finish" && sp$spec == "S1_sd") {
        cat_both(sprintf(
          "LEFT BEFORE FINISH | S1 | per +1 SD real LSF (~£%.0f more) -> odds of leaving x %.3f (%.3f-%.3f) | n=%s | p=%.3g",
          sd_gbp, o$OR, o$lo, o$hi, format(nrow(d), big.mark = ","), cr$p
        ))
        cat_both(sprintf(
          "  invert: per -1 SD real LSF (~£%.0f less) -> odds x %.3f",
          sd_gbp, 1 / o$OR
        ))
      }
    }
  }
}
A_tbl <- bind_rows(rows_a)
write_csv(A_tbl, file.path(pack, "A_student_level_pound_effects.csv"))

# copy 05 tables if present
for (f in c("tbl_rv_spec_ladder.csv", "tbl_rv_joint_terms.csv",
            "tbl_rv_interactions.csv", "tbl_rv_auc.csv",
            "tbl_rv_measure_sensitivity.csv")) {
  src <- file.path(outputs_dir(), f)
  if (file.exists(src)) file.copy(src, file.path(pack, f), overwrite = TRUE)
}

# ===========================================================================
# B. STUDENT-YEAR PANEL: which year's real LSF, and lags
# ===========================================================================
progress("06 B: build student-year panel with wave-specific real LSF ...")

# long panel (typed lightly)
long_path <- file.path(derived_dir(), "lsf_panel_long_2020_2026.csv")
if (!file.exists(long_path)) {
  cat_both("WARNING: long panel CSV missing - skip panel arm B. Run 01 first.")
} else {

  long <- read_csv(
    long_path,
    col_select = c(UniqueID, year, first_year, course, college, confidence,
                   leave_course, fund_availability, grant_influence,
                   funding_influence_uni, funding_influence_course, grant_difference),
    show_col_types = FALSE
  ) |>
    mutate(
      year = as.integer(year),
      confidence = suppressWarnings(as.integer(confidence)),
      leave_course = to_01(leave_course),
      first_year = as.logical(first_year)
    )

  # attach trajectory anchors for exit / left_next
  traj_path <- file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv")
  if (file.exists(traj_path)) {
    tr <- read_csv(traj_path, show_col_types = FALSE) |>
      select(UniqueID, any_of(c("last_wave", "course_first_year_wave", "expected_finish",
                                "n_waves", "first_wave")))
    long <- long |> left_join(tr, by = "UniqueID")
  } else {
    # derive from long
    tr <- long |>
      group_by(UniqueID) |>
      summarise(
        first_wave = min(year, na.rm = TRUE),
        last_wave  = max(year, na.rm = TRUE),
        n_waves    = n(),
        .groups = "drop"
      )
    long <- long |> left_join(tr, by = "UniqueID")
    long$course_first_year_wave <- long$first_wave
    long$expected_finish <- NA_integer_
  }

  # entry college (stable campus for real-value join; fall back to wave college)
  entry_col <- long |>
    filter(first_year %in% TRUE | year == course_first_year_wave) |>
    group_by(UniqueID) |>
    slice_min(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(UniqueID, college_entry = college, entry_year = year,
              course_entry = course)

  long <- long |>
    left_join(entry_col, by = "UniqueID") |>
    mutate(
      college_use = dplyr::coalesce(college, college_entry),
      # left next year = this is last wave and expected more course left
      at_risk = !is.na(expected_finish) & year < expected_finish & year <= 2024L,
      left_next = as.integer(at_risk & year == last_wave)
    )

  # --- wave-specific real LSF: match college_use + YEAR (not only entry_year)
  # temporarily point RV columns at wave fields via a thin wrapper sample
  wave_samp <- long |>
    transmute(
      UniqueID, year,
      college = college_use,
      entry_year = year,          # trick build_real_value year join
      course = dplyr::coalesce(course, course_entry),
      parental = 0L
    ) |>
    distinct()

  # build_real_value expects analysis-sample-like cols; parental optional
  wave_rv <- build_real_value(
    as.data.frame(wave_samp), ref, awards, cpih, base_year = 2020,
    provider_col = "college", year_col = "entry_year", parent_col = NA_character_
  ) |>
    transmute(
      UniqueID, year,
      rv_gbp_wave = as.numeric(.data[[PRIMARY]]),
      lad_code, region
    )

  panel <- long |>
    left_join(wave_rv, by = c("UniqueID", "year")) |>
    left_join(
      stud |> select(UniqueID, rv_gbp_entry = rv_gbp, left_before_finish,
                     one_wave_only, left_2y_plus_early, considered_leaving,
                     fund_availability, grant_influence, crit_course, crit_uni,
                     grant_helps_stay),
      by = "UniqueID"
    )

  # last-wave real LSF per student
  last_rv <- panel |>
    filter(!is.na(rv_gbp_wave)) |>
    group_by(UniqueID) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(UniqueID, rv_gbp_last = rv_gbp_wave, last_rv_year = year)

  panel <- panel |> left_join(last_rv, by = "UniqueID")

  # lags within student: real LSF and confidence previous wave
  panel <- panel |>
    arrange(UniqueID, year) |>
    group_by(UniqueID) |>
    mutate(
      rv_gbp_lag1        = dplyr::lag(rv_gbp_wave),
      confidence_lag1    = dplyr::lag(confidence),
      leave_course_lag1  = dplyr::lag(leave_course),
      confidence_lead1   = dplyr::lead(confidence),
      leave_course_lead1 = dplyr::lead(leave_course),
      year_lag1          = dplyr::lag(year)
    ) |>
    ungroup() |>
    # only lag from previous calendar survey year (not multi-year gaps as "t-1")
    mutate(
      lag_is_adjacent = !is.na(year_lag1) & (year - year_lag1 == 1L)
    )

  # descriptives
  desc <- tibble(
    n_student_years = nrow(panel),
    n_students = n_distinct(panel$UniqueID),
    pct_wave_rv = round(100 * mean(!is.na(panel$rv_gbp_wave)), 1),
    mean_rv_wave = mean(panel$rv_gbp_wave, na.rm = TRUE),
    sd_rv_wave = sd(panel$rv_gbp_wave, na.rm = TRUE),
    cor_entry_wave = suppressWarnings(cor(panel$rv_gbp_entry, panel$rv_gbp_wave, use = "complete.obs")),
    cor_entry_last = {
      x <- panel |> distinct(UniqueID, .keep_all = TRUE)
      suppressWarnings(cor(x$rv_gbp_entry, x$rv_gbp_last, use = "complete.obs"))
    }
  )
  write_csv(desc, file.path(pack, "B_panel_descriptives.csv"))
  cat_both("")
  cat_both("=== B. PANEL DESCRIPTIVES ===")
  cat_both(sprintf("Student-years=%s | students=%s | %% with wave real LSF=%.1f",
                   format(desc$n_student_years, big.mark = ","),
                   format(desc$n_students, big.mark = ","), desc$pct_wave_rv))
  cat_both(sprintf("Corr(entry real LSF, wave real LSF)=%.3f | Corr(entry, last-wave)=%.3f",
                   desc$cor_entry_wave, desc$cor_entry_last))
  cat_both("(If those correlations are ~1, year-specific real LSF barely moves - frozen grant + slow local prices.)")

  # ----- B1 which year's real LSF predicts student-level exit? -----
  progress("06 B1: which year counts for leaving ...")
  stud2 <- stud |>
    left_join(last_rv, by = "UniqueID") |>
    mutate(
      rv_entry = rv_gbp,
      rv_last  = rv_gbp_last
    )

  which_year <- list()
  for (y in c("left_before_finish", "one_wave_only")) {
    for (term in c("rv_entry", "rv_last")) {
      need <- c(y, term, "course", "entry_year")
      ok <- Reduce(`&`, lapply(need, function(v) !is.na(stud2[[v]])))
      d <- stud2[ok, , drop = FALSE]
      m <- fit_bin(d, y, term, FE_STUDENT)
      cr <- coef_row(m, term)
      pe <- pound_effect(cr$b, cr$se, 1000)
      which_year[[length(which_year) + 1L]] <- tibble(
        outcome = y, real_lsf_timing = term, n = nrow(d),
        b = cr$b, se = cr$se, p = cr$p,
        pounds_less = 1000,
        OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
        pct_higher_odds = pe$pct_higher_odds
      )
      cat_both(sprintf(
        "WHICH YEAR | %s | %s | £1,000 LESS -> odds x %.3f (%.3f-%.3f) ~ %+.1f%% | n=%s p=%.3g",
        y, term, pe$OR_if_reduced, pe$lo, pe$hi, pe$pct_higher_odds,
        format(nrow(d), big.mark = ","), cr$p
      ))
    }
  }
  # both in one model (horse race entry vs last) - collinear if highly correlated
  for (y in c("left_before_finish")) {
    need <- c(y, "rv_entry", "rv_last", "course", "entry_year")
    ok <- Reduce(`&`, lapply(need, function(v) !is.na(stud2[[v]])))
    d <- stud2[ok, , drop = FALSE]
    m <- fit_bin(d, y, "rv_entry + rv_last", FE_STUDENT)
    for (term in c("rv_entry", "rv_last")) {
      cr <- coef_row(m, term)
      pe <- pound_effect(cr$b, cr$se, 1000)
      which_year[[length(which_year) + 1L]] <- tibble(
        outcome = y, real_lsf_timing = paste0(term, "_joint"), n = nrow(d),
        b = cr$b, se = cr$se, p = cr$p, pounds_less = 1000,
        OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
        pct_higher_odds = pe$pct_higher_odds
      )
      cat_both(sprintf(
        "WHICH YEAR JOINT | %s | %s | £1,000 LESS -> odds x %.3f (%.3f-%.3f) | n=%s p=%.3g",
        y, term, pe$OR_if_reduced, pe$lo, pe$hi, format(nrow(d), big.mark = ","), cr$p
      ))
    }
  }
  write_csv(bind_rows(which_year), file.path(pack, "B_panel_which_year_counts.csv"))

  # ----- B2 lags: real LSF_t -> confidence_{t+1}, leave_course_t, left_next -----
  progress("06 B2: lag models (real LSF -> next confidence / leave) ...")

  lag_rows <- list()

  # (i) this wave real LSF -> considered leaving THIS wave
  d <- panel |>
    filter(!is.na(rv_gbp_wave), !is.na(leave_course), !is.na(course), !is.na(year),
           first_year %in% FALSE)   # leave_course is continuing-wave item
  m <- fit_bin(d, "leave_course", "rv_gbp_wave", FE_PANEL)
  cr <- coef_row(m, "rv_gbp_wave")
  pe <- pound_effect(cr$b, cr$se, 1000)
  lag_rows[[length(lag_rows) + 1L]] <- tibble(
    model = "realLSF_t -> considered_leaving_t",
    y = "leave_course", x = "rv_gbp_wave", n = nrow(d),
    b = cr$b, se = cr$se, p = cr$p, pounds_less = 1000,
    OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
    pct_higher_odds = pe$pct_higher_odds
  )
  cat_both(sprintf(
    "LAG | real LSF_t -> considered leaving_t | £1,000 LESS -> odds x %.3f (%.3f-%.3f) ~ %+.1f%% | n=%s p=%.3g",
    pe$OR_if_reduced, pe$lo, pe$hi, pe$pct_higher_odds, format(nrow(d), big.mark = ","), cr$p
  ))

  # (ii) this wave real LSF -> left next year (hazard)
  d <- panel |>
    filter(!is.na(rv_gbp_wave), !is.na(left_next), !is.na(course), !is.na(year),
           at_risk %in% TRUE)
  m <- fit_bin(d, "left_next", "rv_gbp_wave", FE_PANEL)
  cr <- coef_row(m, "rv_gbp_wave")
  pe <- pound_effect(cr$b, cr$se, 1000)
  lag_rows[[length(lag_rows) + 1L]] <- tibble(
    model = "realLSF_t -> left_next_year",
    y = "left_next", x = "rv_gbp_wave", n = nrow(d),
    b = cr$b, se = cr$se, p = cr$p, pounds_less = 1000,
    OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
    pct_higher_odds = pe$pct_higher_odds
  )
  cat_both(sprintf(
    "LAG | real LSF_t -> left next year | £1,000 LESS -> odds x %.3f (%.3f-%.3f) ~ %+.1f%% | n=%s p=%.3g",
    pe$OR_if_reduced, pe$lo, pe$hi, pe$pct_higher_odds, format(nrow(d), big.mark = ","), cr$p
  ))

  # (iii) real LSF_t -> confidence_{t+1}  (use lag: predict this confidence from lag RV)
  # row at t has confidence_t and rv_gbp_lag1 (= real LSF at t-1)
  d <- panel |>
    filter(lag_is_adjacent, !is.na(rv_gbp_lag1), !is.na(confidence),
           !is.na(course), !is.na(year))
  m <- fit_lin(d, "confidence", "rv_gbp_lag1", FE_PANEL)
  cr <- coef_row(m, "rv_gbp_lag1")
  # linear: £1,000 LESS real LSF -> change in confidence points = -1000 * b
  d_conf <- if (is.finite(cr$b)) -1000 * cr$b else NA_real_
  d_lo   <- if (is.finite(cr$b)) -1000 * (cr$b + 1.96 * cr$se) else NA_real_
  d_hi   <- if (is.finite(cr$b)) -1000 * (cr$b - 1.96 * cr$se) else NA_real_
  lag_rows[[length(lag_rows) + 1L]] <- tibble(
    model = "realLSF_{t-1} -> confidence_t (linear FE)",
    y = "confidence", x = "rv_gbp_lag1", n = nrow(d),
    b = cr$b, se = cr$se, p = cr$p, pounds_less = 1000,
    OR_if_reduced = NA_real_, lo = d_lo, hi = d_hi,
    pct_higher_odds = NA_real_,
    conf_points_if_reduced = d_conf
  )
  cat_both(sprintf(
    "LAG | real LSF_{t-1} -> confidence_t | £1,000 LESS real LSF -> confidence %+.3f points (%.3f to %.3f) on 1-5 scale | n=%s p=%.3g",
    d_conf, d_lo, d_hi, format(nrow(d), big.mark = ","), cr$p
  ))

  # (iv) also binary "low confidence" (1-2) next year
  d <- panel |>
    filter(lag_is_adjacent, !is.na(rv_gbp_lag1), !is.na(confidence),
           !is.na(course), !is.na(year)) |>
    mutate(low_conf = as.integer(confidence <= 2L))
  m <- fit_bin(d, "low_conf", "rv_gbp_lag1", FE_PANEL)
  cr <- coef_row(m, "rv_gbp_lag1")
  pe <- pound_effect(cr$b, cr$se, 1000)
  lag_rows[[length(lag_rows) + 1L]] <- tibble(
    model = "realLSF_{t-1} -> low_confidence_t (1-2)",
    y = "low_conf", x = "rv_gbp_lag1", n = nrow(d),
    b = cr$b, se = cr$se, p = cr$p, pounds_less = 1000,
    OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
    pct_higher_odds = pe$pct_higher_odds
  )
  cat_both(sprintf(
    "LAG | real LSF_{t-1} -> low confidence_t (1-2) | £1,000 LESS -> odds x %.3f (%.3f-%.3f) ~ %+.1f%% | n=%s p=%.3g",
    pe$OR_if_reduced, pe$lo, pe$hi, pe$pct_higher_odds, format(nrow(d), big.mark = ","), cr$p
  ))

  # (v) real LSF_t -> leave_course_{t+1}
  d <- panel |>
    filter(lag_is_adjacent, !is.na(rv_gbp_lag1), !is.na(leave_course),
           !is.na(course), !is.na(year), first_year %in% FALSE)
  m <- fit_bin(d, "leave_course", "rv_gbp_lag1", FE_PANEL)
  cr <- coef_row(m, "rv_gbp_lag1")
  pe <- pound_effect(cr$b, cr$se, 1000)
  lag_rows[[length(lag_rows) + 1L]] <- tibble(
    model = "realLSF_{t-1} -> considered_leaving_t",
    y = "leave_course", x = "rv_gbp_lag1", n = nrow(d),
    b = cr$b, se = cr$se, p = cr$p, pounds_less = 1000,
    OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
    pct_higher_odds = pe$pct_higher_odds
  )
  cat_both(sprintf(
    "LAG | real LSF_{t-1} -> considered leaving_t | £1,000 LESS -> odds x %.3f (%.3f-%.3f) ~ %+.1f%% | n=%s p=%.3g",
    pe$OR_if_reduced, pe$lo, pe$hi, pe$pct_higher_odds, format(nrow(d), big.mark = ","), cr$p
  ))

  write_csv(bind_rows(lag_rows), file.path(pack, "B_panel_lags.csv"))
}

# ---- README ----------------------------------------------------------------
readme <- c(
  "LSF real-value FINDINGS PACK (aggregate only - safe to email / paste)",
  paste("Generated:", Sys.time()),
  "",
  "DESIGN (two strands)",
  "--------------------",
  "1) Student-level: ONE real LSF score per student at ENTRY (college + entry year).",
  "   Outcome = trajectory leaving flags (left before finish, etc.).",
  "   This is what 04/05 mostly did. Good for 'does place-thinned bursary mark exit risk?'.",
  "",
  "2) Student-year panel: real LSF re-computed each WAVE (college + wave year).",
  "   Grant nominal is frozen but local rents/CPIH move, so score can change over time.",
  "   Use this for: which year counts; real LSF_t -> confidence_{t+1}; leave next year.",
  "",
  "If entry and last-wave real LSF are almost the same (high correlation), year choice",
  "barely matters and the panel lag story is weak (little within-person variation).",
  "",
  "INTERPRETING £ EFFECTS",
  "----------------------",
  "Models use real LSF in pounds. We report: if real LSF were £1,000 LOWER,",
  "odds of the bad outcome multiply by OR_if_reduced.",
  "  OR 1.08  => about +8% on the *odds* of leaving (not exactly +8pp probability).",
  "Confidence models are linear: £1,000 less -> change in points on the 1-5 scale.",
  "",
  "FILES",
  "-----",
  "findings_numbers.txt                 plain-English lines to paste back",
  "A_student_level_pound_effects.csv    entry real LSF £/SD effects",
  "B_panel_which_year_counts.csv        entry vs last-wave real LSF",
  "B_panel_lags.csv                     lag models",
  "B_panel_descriptives.csv             panel coverage + correlations",
  "tbl_rv_*.csv                         copied from 05 if present",
  "",
  "WHAT TO SEND BACK (no secure data)",
  "----------------------------------",
  "Just email findings_numbers.txt OR paste its contents into chat.",
  "Optionally attach the CSVs in this folder (still aggregate only)."
)
writeLines(readme, file.path(pack, "00_README.txt"))

cat_both("")
cat_both("Pack written to:")
cat_both(pack)
cat_both("")
cat_both("EMAIL / PASTE: send findings_numbers.txt only (or this whole findings_pack folder).")
cat_both("No student-level data is in this pack.")

message("\nDone. Open:\n  ", pack, "\n  especially findings_numbers.txt")
invisible(pack)
