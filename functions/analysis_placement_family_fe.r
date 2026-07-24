# ===========================================================================
# functions/analysis_placement_family_fe.r
#
# Placement hours under course-family fixed effects: within a family of similar
# programmes, does a heavier placement load track worse outcomes? Moved verbatim
# from scripts/p3_hours_family_fe.r. Changes: the function wrapper, indentation,
# REF_DIR / FE_FAMILY / MIN_PROGRAMME / MIN_RANGE_H from scripts/00_config.r
# under the same names, and two aliases where p3 used a name config spells
# differently (MIN_N 200L is MIN_MODEL_N; CONTROLS is COMP_VARS, identical
# values). Aliases rather than a rename so the body stays byte-identical.
# ===========================================================================

analysis_placement_family_fe <- function() {
  MIN_N    <- MIN_MODEL_N   # students per model, from 00_config.r
  CONTROLS <- COMP_VARS     # identical to p3s c(parental, specialist, regional)

  OUTCOMES <- tibble::tribble(
    ~var,                 ~label,
    "left_before_finish", "Left before finishing",
    "left_2y_plus_early", "Left 2+ years early",
    "considered_leaving", "Considered leaving",
    "grant_helps_stay",   "Grant helps me stay"
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
  progress("p3: load sample and attach placement hours ...")
  stud <- placement_sample(dir = REF_DIR, quiet = TRUE)

  cat_both("")
  cat_both("=== ARM P2: hours under course-family FE (U5) - EXPLORATORY ===")
  cat_both("Course FE is dropped so hours can vary. Subject confounding is NOT")
  cat_both("controlled: this compares a midwife to an adult nurse, and placement")
  cat_both("hours are not the only difference between them.")
  cat_both("")

  have_controls <- intersect(CONTROLS, names(stud))
  if (!length(have_controls)) {
    cat_both("NOTE: no bursary component flags on the sample; models run unadjusted.")
  } else if (length(have_controls) < length(CONTROLS)) {
    cat_both(sprintf("NOTE: controls available: %s (missing: %s)",
                     paste(have_controls, collapse = ", "),
                     paste(setdiff(CONTROLS, have_controls), collapse = ", ")))
  }
  cat_both("NOTE: no demographic controls exist on lsf_analysis_sample.rds. Age,")
  cat_both("ethnicity and disability are on feat/demographics-slides and are not")
  cat_both("joined here. Treat every coefficient below as demographically unadjusted.")
  cat_both("")

  stud <- stud |>
    mutate(across(any_of(c("fund_availability", "grant_influence", "grant_helps_stay",
                           "left_before_finish", "one_wave_only", "left_2y_plus_early",
                           "considered_leaving", CONTROLS)), to_01),
           entry_year = as.integer(entry_year)) |>
    filter(!is.na(hours_per100), !is.na(course_family))

  if (!nrow(stud)) stop("no students carry both an hours value and a family", call. = FALSE)

  # ===========================================================================
  # Identifying variation, reported before any coefficient.
  # ===========================================================================
  progress("p3: family variation table ...")

  fam_var <- stud |>
    group_by(course_family) |>
    summarise(n_students   = n(),
              n_programmes = n_distinct(programme_code),
              hours_min    = 100 * min(hours_per100),
              hours_max    = 100 * max(hours_per100),
              .groups = "drop") |>
    mutate(hours_range = hours_max - hours_min,
           identifies  = n_programmes >= MIN_PROGRAMME &
                         hours_range  >= MIN_RANGE_H &
                         n_students   >= MIN_N,
           verdict = case_when(
             n_programmes <  MIN_PROGRAMME ~ "no within-family variation (single programme)",
             hours_range  <  MIN_RANGE_H   ~ sprintf("range too narrow (%.0f hours)", hours_range),
             n_students   <  MIN_N         ~ sprintf("too few students (%d)", n_students),
             TRUE                          ~ "estimated"))

  write_csv(fam_var, file.path(pack, "P3_family_variation.csv"))

  cat_both("--- IDENTIFYING VARIATION BY FAMILY ---")
  for (i in seq_len(nrow(fam_var)))
    cat_both(sprintf("  %-8s programmes=%-3d hours %4.0f-%4.0f (range %3.0f)  n=%-8s  %s",
                     as.character(fam_var$course_family[i]), fam_var$n_programmes[i],
                     fam_var$hours_min[i], fam_var$hours_max[i], fam_var$hours_range[i],
                     format(fam_var$n_students[i], big.mark = ","), fam_var$verdict[i]))
  cat_both("")

  # ===========================================================================
  # Models: pooled across families, then per family.
  # ===========================================================================
  progress("p3: fitting family-FE models ...")

  rhs <- paste(c("hours_per100", have_controls), collapse = " + ")
  res_rows <- list()

  fit_and_report <- function(d, y, lbl, scope, fe, note = "") {
    need <- c(y, "hours_per100", "entry_year")
    ok <- Reduce(`&`, lapply(need, function(v) !is.na(d[[v]])))
    d <- d[ok, , drop = FALSE]
    if (nrow(d) < MIN_N) {
      cat_both(sprintf("  %-8s %-24s insufficient support (n = %d)", scope, lbl, nrow(d)))
      return(tibble(arm = "P2", scope = scope, outcome = y, outcome_label = lbl,
                    n = nrow(d), n_programmes = n_distinct(d$programme_code),
                    OR = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_,
                    note = "insufficient support"))
    }
    m <- fit_feglm(d, y, rhs, fe = fe)
    o <- pull_or(m, "hours_per100")
    cat_both(sprintf("  %-8s %-24s n=%-8s programmes=%-3d OR=%s (95%% CI %s to %s)%s",
                     scope, lbl, format(nrow(d), big.mark = ","),
                     n_distinct(d$programme_code),
                     formatC(o$OR, format = "f", digits = 4),
                     formatC(o$lo, format = "f", digits = 4),
                     formatC(o$hi, format = "f", digits = 4),
                     if (nzchar(note)) paste0("  ", note) else ""))
    tibble(arm = "P2", scope = scope, outcome = y, outcome_label = lbl,
           n = nrow(d), n_programmes = n_distinct(d$programme_code),
           OR = o$OR, lo = o$lo, hi = o$hi, p = o$p, note = note)
  }

  cat_both("--- ARM P2 RESULTS (OR per 100 extra placement hours per year) ---")
  for (i in seq_len(nrow(OUTCOMES))) {
    y <- OUTCOMES$var[i]; lbl <- OUTCOMES$label[i]
    if (!y %in% names(stud)) { cat_both(sprintf("  (outcome %s absent - skipped)", y)); next }

    res_rows[[length(res_rows) + 1L]] <-
      fit_and_report(stud, y, lbl, "pooled", FE_FAMILY)

    for (f in levels(droplevels(stud$course_family))) {
      fv <- filter(fam_var, course_family == f)
      if (!nrow(fv) || !fv$identifies) {
        cat_both(sprintf("  %-8s %-24s NOT ESTIMATED - %s", f, lbl, fv$verdict))
        res_rows[[length(res_rows) + 1L]] <- tibble(
          arm = "P2", scope = f, outcome = y, outcome_label = lbl,
          n = if (nrow(fv)) fv$n_students else NA_integer_,
          n_programmes = if (nrow(fv)) fv$n_programmes else NA_integer_,
          OR = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_,
          note = fv$verdict)
        next
      }
      # Within one family the family FE is a constant, so entry_year carries the FE.
      note <- if (f == "dental")
        "CROSSWALK-AMBIGUOUS: the three dental programmes are hard to tell apart in free text" else ""
      res_rows[[length(res_rows) + 1L]] <-
        fit_and_report(filter(stud, course_family == f), y, lbl, f, "entry_year", note)
    }
    cat_both("")
  }

  results <- bind_rows(res_rows)
  write_csv(results, file.path(pack, "P3_family_fe_results.csv"))

  # ---- demeaning check -------------------------------------------------------
  # hours_demeaned under family FE must reproduce raw hours under family FE.
  # If it does not, the demeaning in attach_placement_hours() is wrong.
  progress("p3: demeaning equivalence check ...")
  chk_y <- OUTCOMES$var[OUTCOMES$var %in% names(stud)][1]
  if (!is.na(chk_y)) {
    d <- stud[!is.na(stud[[chk_y]]), , drop = FALSE]
    a <- pull_or(fit_feglm(d, chk_y, "hours_per100",  fe = FE_FAMILY), "hours_per100")
    b <- pull_or(fit_feglm(d, chk_y, "hours_demeaned", fe = FE_FAMILY), "hours_demeaned")
    same <- is.finite(a$OR) && is.finite(b$OR) && abs(a$OR - b$OR) < 1e-6
    cat_both(sprintf("demeaning check on %s: raw OR = %.6f, demeaned OR = %.6f -> %s",
                     chk_y, a$OR, b$OR,
                     if (same) "equivalent, as expected" else "MISMATCH - inspect attach_placement_hours()"))
  }

  cat_both("")
  cat_both("Arm P2 caveats: exploratory; subject confounding uncontrolled by construction;")
  cat_both("no demographic controls available on this branch; dental results carry")
  cat_both("crosswalk ambiguity; hours are a single FY26/27 snapshot.")
  cat_both("")
  progress("p3: done -> ", pack)

  invisible(TRUE)
}
