# ===========================================================================
# functions/analysis_placement_interaction.r
#
# Does placement burden change how much real LSF matters? Hours x real-value
# interaction. Moved verbatim from scripts/p2_hours_interaction.r; the only
# changes are the function wrapper, indentation, and the constants REF_DIR /
# PRIMARY / PRIMARY_LBL / FE_STUDENT / FE_PANEL / POUNDS_LESS / MIN_BAND_N /
# MIN_SD_GBP now coming from scripts/00_config.r under the same names.
# ===========================================================================

analysis_placement_interaction <- function() {
  OUTCOMES <- tibble::tribble(
    ~var,                 ~label,
    "left_before_finish", "Left before finishing",
    "left_2y_plus_early", "Left 2+ years early",
    "one_wave_only",      "Claimed once only",
    "considered_leaving", "Considered leaving"
  )
  # ---------------------------------------------------------------------------

  stamp <- format(Sys.Date(), "%Y%m%d")
  pack  <- file.path(outputs_dir(), paste0("placement_hours_pack_", stamp))
  dir.create(pack, showWarnings = FALSE, recursive = TRUE)
  numbers_file <- file.path(pack, "placement_numbers.txt")
  cat_both <- function(...) {
    msg <- paste0(...)
    message(msg)
    cat(msg, "\n", file = numbers_file, append = TRUE)
  }

  # ---- load ------------------------------------------------------------------
  progress("p2: load sample, real value, placement hours ...")
  SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

  # real value first, hours second: attach_placement_hours() carries the match
  # report as an attribute and a later join would drop it.
  stud <- build_real_value(SAMPLE, ref, awards, cpih, base_year = 2020)
  stud <- attach_placement_hours(as.data.frame(stud), dir = REF_DIR)

  cat_both("")
  cat_both("=== ARM P1: real LSF x placement hours (U4) ===")
  placement_match_report(stud, emit = cat_both)
  cat_both("")

  stud <- stud |>
    mutate(across(any_of(c("fund_availability", "grant_influence", "grant_helps_stay",
                           "left_before_finish", "one_wave_only", "left_2y_plus_early",
                           "considered_leaving")), to_01),
           course     = as.character(course),
           entry_year = as.integer(entry_year),
           rv_gbp     = as.numeric(.data[[PRIMARY]]))

  # ===========================================================================
  # GATE: within-course variation in real value, by hours band.
  # This runs before any estimate and decides whether the arm has support.
  # ===========================================================================
  progress("p2: variation diagnostic ...")

  vd <- stud |>
    filter(!is.na(rv_gbp), !is.na(hours_band), !is.na(course)) |>
    group_by(hours_band, course) |>
    summarise(n_course = n(), sd_course = stats::sd(rv_gbp), .groups = "drop") |>
    filter(n_course >= 10) |>
    group_by(hours_band) |>
    summarise(n_courses      = n(),
              n_students     = sum(n_course),
              mean_within_sd = mean(sd_course, na.rm = TRUE),
              med_within_sd  = stats::median(sd_course, na.rm = TRUE),
              .groups = "drop") |>
    mutate(usable = mean_within_sd >= MIN_SD_GBP & n_students >= MIN_BAND_N)

  write_csv(vd, file.path(pack, "P2_variation_diagnostic.csv"))

  cat_both("--- GATE: within-course variation in real LSF, by hours band ---")
  cat_both("The interaction is identified off real value moving WITHIN a course,")
  cat_both("across places and years. If it does not move on high-hours courses,")
  cat_both("the interaction is noise however tight its interval looks.")
  for (i in seq_len(nrow(vd)))
    cat_both(sprintf("  %-8s courses=%-4d students=%-8s mean within-course SD = £%.0f  %s",
                     as.character(vd$hours_band[i]), vd$n_courses[i],
                     format(vd$n_students[i], big.mark = ","),
                     vd$mean_within_sd[i],
                     if (vd$usable[i]) "OK" else "THIN - treat estimates as unsupported"))
  if (!all(vd$usable)) {
    cat_both("")
    cat_both("WARNING: at least one band lacks usable within-course variation.")
    cat_both("Estimates below still print, but the arm does not support a claim about")
    cat_both("the band(s) marked THIN. Say that on the slide, not in a footnote.")
  }
  cat_both("")

  # ===========================================================================
  # The interaction models.
  # ===========================================================================
  progress("p2: fitting interaction models ...")

  # pound_effect() is defined inside 06_findings_pack.r rather than in functions/,
  # so it is not on the search path here. Copied verbatim rather than moved: this
  # arm must not change the behaviour of the existing findings pack. If it ever
  # moves to functions/, delete this copy.
  #   effect of REDUCING real value by `pounds_less`
  pound_effect <- function(b, se, pounds_less = 1000) {
    bb <- -pounds_less * b
    ss <- pounds_less * se
    or <- exp(bb)
    tibble(pounds_less = pounds_less,
           OR_if_reduced = or,
           lo = exp(bb - 1.96 * ss),
           hi = exp(bb + 1.96 * ss),
           pct_higher_odds = 100 * (or - 1))
  }

  # SE of a linear combination b_rv + h * b_int, from the model vcov. Needed
  # because the quantity anyone can read is "the £1,000 effect at h hours",
  # not the bare interaction coefficient.
  combo <- function(m, rv_term, int_term, h) {
    ct <- tryCatch(as.data.frame(fixest::coeftable(m)), error = function(e) NULL)
    if (is.null(ct)) return(NULL)
    rn <- rownames(ct)
    pick <- function(cands) { hit <- cands[cands %in% rn]; if (length(hit)) hit[1] else NA_character_ }
    r1 <- pick(c(rv_term, paste0("`", rv_term, "`")))
    r2 <- pick(c(int_term, paste0(strsplit(int_term, ":", fixed = TRUE)[[1]][2], ":",
                                  strsplit(int_term, ":", fixed = TRUE)[[1]][1]),
                 paste0("`", int_term, "`")))
    if (is.na(r1) || is.na(r2)) return(NULL)
    V <- tryCatch(stats::vcov(m), error = function(e) NULL)
    if (is.null(V) || !all(c(r1, r2) %in% rownames(V))) return(NULL)
    b  <- ct[r1, "Estimate"] + h * ct[r2, "Estimate"]
    va <- V[r1, r1] + h^2 * V[r2, r2] + 2 * h * V[r1, r2]
    if (!is.finite(va) || va < 0) return(NULL)
    list(b = b, se = sqrt(va))
  }

  # Hours values to report the £1,000 effect at: the observed band medians, so
  # "low-hours course" and "high-hours course" mean something concrete.
  band_h <- stud |>
    filter(!is.na(hours_band), !is.na(hours_per100)) |>
    group_by(hours_band) |>
    summarise(h = stats::median(hours_per100), n = n(), .groups = "drop")

  res_rows <- list(); marg_rows <- list()

  for (i in seq_len(nrow(OUTCOMES))) {
    y   <- OUTCOMES$var[i]
    lbl <- OUTCOMES$label[i]
    if (!y %in% names(stud)) { cat_both(sprintf("(outcome %s absent - skipped)", y)); next }

    d <- stud |>
      filter(!is.na(.data[[y]]), !is.na(rv_gbp), !is.na(hours_per100),
             !is.na(course), !is.na(entry_year))
    if (nrow(d) < MIN_BAND_N) {
      cat_both(sprintf("  %s: only %d usable students - insufficient support, not modelled.",
                       lbl, nrow(d)))
      next
    }

    m <- fit_feglm(d, y, "rv_gbp * hours_per100", fe = FE_STUDENT)
    if (is.null(m)) { cat_both(sprintf("  %s: model failed to fit.", lbl)); next }

    ct <- as.data.frame(fixest::coeftable(m))
    has_main <- any(rownames(ct) == "hours_per100")

    int <- pull_or(m, "rv_gbp:hours_per100")
    # rv_gbp is in single pounds, so the raw interaction OR is per (£1 x 100h) and
    # prints as 1.0000 whatever it is. Rescale to per (£1,000 x 100h) so the
    # number on the page has digits in it. Same estimate, readable units.
    b_int  <- log(int$OR)
    se_int <- (log(int$hi) - b_int) / 1.96
    or1k <- exp(1000 * b_int)
    lo1k <- exp(1000 * (b_int - 1.96 * se_int))
    hi1k <- exp(1000 * (b_int + 1.96 * se_int))

    res_rows[[length(res_rows) + 1L]] <- tibble(
      arm = "P1", outcome = y, outcome_label = lbl, n = nrow(d),
      n_courses = dplyr::n_distinct(d$course),
      term = "rv_gbp:hours_per100",
      OR = int$OR, lo = int$lo, hi = int$hi, p = int$p,
      OR_per_1000gbp_100h = or1k, lo_1k = lo1k, hi_1k = hi1k,
      hours_main_effect = if (has_main) "PRESENT (unexpected)" else "absorbed by course FE (by design)")

    cat_both(sprintf("--- %s ---", lbl))
    cat_both(sprintf("  n = %s across %d courses", format(nrow(d), big.mark = ","),
                     dplyr::n_distinct(d$course)))
    cat_both(if (has_main)
      "  hours main effect: PRESENT in the coefficient table - unexpected, investigate before using."
      else
      "  hours main effect: absorbed by course FE, by design. This is not a null result.")
    cat_both(sprintf("  interaction OR = %.4f per (£1,000 x 100 hours)  (95%% CI %.4f to %.4f, p = %.3f)",
                     or1k, lo1k, hi1k, int$p))

    # translate to the register 06 reports in: "£1,000 less real value ->
    # X% higher odds of leaving", quoted separately for a low- and a high-hours
    # course so the difference between them is the readable finding.
    for (k in seq_len(nrow(band_h))) {
      cc <- combo(m, "rv_gbp", "rv_gbp:hours_per100", band_h$h[k])
      if (is.null(cc)) next
      pe <- pound_effect(cc$b, cc$se, POUNDS_LESS)
      marg_rows[[length(marg_rows) + 1L]] <- tibble(
        arm = "P1", outcome = y, outcome_label = lbl,
        hours_band = as.character(band_h$hours_band[k]),
        hours_per_year = round(100 * band_h$h[k]),
        band_n = band_h$n[k], pounds_less = POUNDS_LESS,
        OR_if_reduced = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
        pct_higher_odds = pe$pct_higher_odds)
      cat_both(sprintf("    at %s hours (%s band): £%s less real LSF -> %+.1f%% odds (CI %.3f to %.3f)",
                       round(100 * band_h$h[k]), as.character(band_h$hours_band[k]),
                       format(POUNDS_LESS, big.mark = ","),
                       pe$pct_higher_odds, pe$lo, pe$hi))
    }
    cat_both("")
  }

  if (length(res_rows)) write_csv(bind_rows(res_rows), file.path(pack, "P2_interaction_results.csv"))
  if (length(marg_rows)) write_csv(bind_rows(marg_rows), file.path(pack, "P2_marginal_by_hours.csv"))

  # ===========================================================================
  # Hazard variant on the student-year panel, so this arm answers the same
  # question on the same frame Arm 2 of the real-value work answers it on.
  # Recipe follows scripts/06_findings_pack.r section B exactly.
  # ===========================================================================
  long_path <- file.path(derived_dir(), "lsf_panel_long_2020_2026.csv")
  if (!file.exists(long_path)) {
    cat_both("panel CSV missing - hazard variant skipped. Run 01 first.")
  } else {
    progress("p2: hazard variant on the student-year panel ...")

    long <- read_csv(long_path,
                     col_select = c(UniqueID, year, first_year, course, college),
                     show_col_types = FALSE) |>
      mutate(year = as.integer(year), first_year = as.logical(first_year))

    traj_path <- file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv")
    tr <- read_csv(traj_path, show_col_types = FALSE) |>
      select(UniqueID, any_of(c("last_wave", "course_first_year_wave", "expected_finish")))
    long <- left_join(long, tr, by = "UniqueID")

    entry_col <- long |>
      filter(first_year %in% TRUE | year == course_first_year_wave) |>
      group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
      transmute(UniqueID, college_entry = college, course_entry = course)

    long <- long |>
      left_join(entry_col, by = "UniqueID") |>
      mutate(college_use = coalesce(college, college_entry),
             at_risk   = !is.na(expected_finish) & year < expected_finish & year <= 2024L,
             left_next = as.integer(at_risk & year == last_wave))

    wave_samp <- long |>
      transmute(UniqueID, year, college = college_use, entry_year = year,
                course = coalesce(course, course_entry), parental = 0L) |>
      distinct()

    wave_rv <- build_real_value(as.data.frame(wave_samp), ref, awards, cpih,
                                base_year = 2020, provider_col = "college",
                                year_col = "entry_year", parent_col = NA_character_) |>
      transmute(UniqueID, year, rv_gbp_wave = as.numeric(.data[[PRIMARY]]))

    panel <- long |>
      left_join(wave_rv, by = c("UniqueID", "year")) |>
      left_join(stud |> distinct(UniqueID, hours_per100, hours_band, course_family),
                by = "UniqueID") |>
      filter(at_risk, !is.na(rv_gbp_wave), !is.na(hours_per100),
             !is.na(left_next), !is.na(course), !is.na(year))

    if (nrow(panel) < MIN_BAND_N) {
      cat_both("hazard variant: too few at-risk student-years - not modelled.")
    } else {
      mh <- fit_feglm(panel, "left_next", "rv_gbp_wave * hours_per100", fe = FE_PANEL)
      if (is.null(mh)) {
        cat_both("hazard variant: model failed to fit.")
      } else {
        inth <- pull_or(mh, "rv_gbp_wave:hours_per100")
        bh   <- log(inth$OR)
        seh  <- (log(inth$hi) - bh) / 1.96
        cat_both("--- hazard variant: gone next year, student-year panel ---")
        cat_both(sprintf("  n = %s student-years across %d courses",
                         format(nrow(panel), big.mark = ","),
                         dplyr::n_distinct(panel$course)))
        cat_both(sprintf("  interaction OR = %.4f per (£1,000 x 100 hours)  (95%% CI %.4f to %.4f, p = %.3f)",
                         exp(1000 * bh), exp(1000 * (bh - 1.96 * seh)),
                         exp(1000 * (bh + 1.96 * seh)), inth$p))
        haz <- tibble(arm = "P1_hazard", outcome = "left_next",
                      outcome_label = "Gone next year", n = nrow(panel),
                      n_courses = dplyr::n_distinct(panel$course),
                      term = "rv_gbp_wave:hours_per100",
                      OR = inth$OR, lo = inth$lo, hi = inth$hi, p = inth$p,
                      OR_per_1000gbp_100h = exp(1000 * bh),
                      lo_1k = exp(1000 * (bh - 1.96 * seh)),
                      hi_1k = exp(1000 * (bh + 1.96 * seh)),
                      hours_main_effect = "absorbed by course FE (by design)")
        write_csv(haz, file.path(pack, "P2_hazard_interaction.csv"))
        for (k in seq_len(nrow(band_h))) {
          cc <- combo(mh, "rv_gbp_wave", "rv_gbp_wave:hours_per100", band_h$h[k])
          if (is.null(cc)) next
          pe <- pound_effect(cc$b, cc$se, POUNDS_LESS)
          cat_both(sprintf("    at %s hours (%s band): £%s less -> %+.1f%% odds of going next year",
                           round(100 * band_h$h[k]), as.character(band_h$hours_band[k]),
                           format(POUNDS_LESS, big.mark = ","), pe$pct_higher_odds))
        }
        cat_both("")
      }
    }
  }

  cat_both("Arm P1 caveats: associational; hours are a single FY26/27 snapshot applied")
  cat_both("to 2020-2026 entrants; the hours main effect is not estimable under course FE.")
  cat_both("")
  progress("p2: done -> ", pack)

  invisible(TRUE)
}
