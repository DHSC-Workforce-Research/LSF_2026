# ===========================================================================
# functions/data_build.r
#
# Every data-preparation step in the pipeline, one function each. Called by
# scripts/01_data.r in order; nothing here fits a model or draws anything.
#
#   build_analysis_sample()      raw LSF workbooks -> lsf_analysis_sample.rds
#   build_real_value_sample()    + cost-of-living  -> lsf_real_value_sample.rds
#   parse_confidence_workbook()  BSA cross-tab     -> financial_confidence_long.csv
#   parse_funding_triangle()     BSA cross-tab     -> funding_triangle_long.csv
#
# Code moved verbatim from scripts/01_read_tidy.r, the data half of
# scripts/04_real_value.r, scripts/d1_parse_sample_demographics_data.r and
# scripts/d4_parse_funding_triangle.r. Constants now come from
# scripts/00_config.r. Everything is written to the secure derived folder,
# never to the repo.
# ===========================================================================


# ---------------------------------------------------------------------------
# build_analysis_sample()   (was scripts/01_read_tidy.r)
#
# Raw LSF workbooks -> one tidy student-level analysis table, in four stages:
#   A. ASSEMBLE   four shared workbook blocks -> one wide panel (+ QA)
#   B. LENGTHEN   wide panel -> one row per student per year answered
#   C. COLLAPSE   student-years -> one trajectory row per student, with a
#                 course-length-anchored reading of course exit attached
#   D. MODEL SET  student-level table: entry answers, leaving-outcome flags,
#                 financial confidence, university, bursary components
# ---------------------------------------------------------------------------
build_analysis_sample <- function() {
  deriv <- derived_dir()

  # --- A. assemble + clean + QA ---------------------------------------
  # The extract arrived as four positional blocks (block 2 lost its header).
  # stitch_panel() stacks them and recovers the header; clean_lsf() repairs the
  # GBP-symbol encoding, squishes whitespace, and turns blanks into NA. Values
  # stay character on purpose: typing is an analysis decision, taken in stage D.
  progress("A. assembling the four workbook blocks ...")
  wide <- stitch_panel(panel_files()) |> clean_lsf()
  print(check_panel(wide))            # hard QA: row / id / column counts must match

  # --- B. lengthen -----------------------------------------------------
  # Wide holds each student once, every question repeated behind a "YYYY-" prefix.
  # reshape_long() turns that into one row per student-year and drops years a
  # student did not answer. Persist it: stage D re-reads it with type guessing on.
  progress("B. reshaping to one row per student-year ...")
  long <- reshape_long(wide)
  write_csv(long, file.path(deriv, "lsf_panel_long_2020_2026.csv"))

  # --- C. collapse to trajectories + classify exit ---------------------
  # One row per student. build_trajectories() applies field-specific rollups
  # (latest value for current state, earliest observed first year for entry, an
  # ever-flag for considered-leaving). classify_outcome() anchors an exit reading
  # on course length, which build_course_lengths() estimates from the data itself.
  progress("C. building trajectories and classifying course exit ...")
  traj           <- build_trajectories(long)
  panel_max      <- max(traj$last_wave, na.rm = TRUE)
  course_lengths <- build_course_lengths(traj)
  traj <- classify_outcome(traj, panel_max_year = panel_max, len_overrides = course_lengths)
  write_csv(traj, file.path(deriv, "lsf_trajectories_classified_2020_2026.csv"))

  # --- D. student-level analysis table ---------------------------------
  # Re-read the long panel with type guessing on (TRUE/FALSE -> logical, 1-5
  # scales -> integer), taking only the columns the analysis needs. This types
  # the model inputs correctly without hand-coercion, and keeps the read light.
  progress("D. building the student-level analysis table ...")
  long_typed <- read_csv(
    file.path(deriv, "lsf_panel_long_2020_2026.csv"),
    col_select = c(UniqueID, year, first_year, course,
                   fund_availability, grant_influence,
                   funding_influence_uni, funding_influence_course,
                   grant_difference, leave_course,
                   confidence, college, grants_applied),
    show_col_types = FALSE)

  # entry answers + every leaving-outcome definition (the functions own the logic)
  sample <- build_funding_leaving_sample(long_typed, traj) |>
    define_leaving_outcomes()

  # financial confidence: first continuing-wave value per student (asked from year 2)
  confidence_cw <- long_typed |>
    filter(first_year == FALSE, !is.na(confidence)) |>
    arrange(UniqueID, year) |>
    group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |>
    select(UniqueID, confidence)

  # entry-wave university and bursary components (extras on top of the base grant)
  entry_extras <- long_typed |>
    filter(first_year == TRUE) |>
    group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
    transmute(
      UniqueID, college,
      parental   = str_detect(coalesce(grants_applied, ""), regex("parental",   ignore_case = TRUE)),
      specialist = str_detect(coalesce(grants_applied, ""), regex("specialist", ignore_case = TRUE)),
      regional   = str_detect(coalesce(grants_applied, ""), regex("regional",   ignore_case = TRUE))) |>
    mutate(n_extras = parental + specialist + regional)

  sample <- sample |>
    left_join(confidence_cw, by = "UniqueID") |>
    left_join(entry_extras,  by = "UniqueID")

  # --- save the single analysis table ----------------------------------
  saveRDS(sample, file.path(deriv, "lsf_analysis_sample.rds"))
  progress("done: ", nrow(sample), " students  ->  lsf_analysis_sample.rds")
  invisible(sample)
}


# ---------------------------------------------------------------------------
# build_real_value_sample()   (was the data half of scripts/04_real_value.r)
#
# Joins the committed cost-of-living reference CSVs onto the student-level
# sample and builds every real-value measure (logic in functions/real_value.r,
# headline = weighted rent+CPI index at HOUSING_WEIGHT). Model fitting that
# used to sit in 04 now lives in 02_analysis.r.
#
# Inputs:
#   * the three CSVs written by 90_build_reference.r, in REF_DIR
#   * the student-level sample, carrying at least RV_PROVIDER (college),
#     RV_YEAR (entry_year) and OUTCOME
# ---------------------------------------------------------------------------
build_real_value_sample <- function(sample = NULL) {
  if (is.null(sample))
    sample <- readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds"))
  SAMPLE <- as.data.frame(sample)

  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),            show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),            show_col_types = FALSE, progress = FALSE)

  need <- c(RV_PROVIDER, RV_YEAR, OUTCOME)
  miss <- setdiff(need, names(SAMPLE))
  if (length(miss))
    stop("SAMPLE is missing: ", paste(miss, collapse = ", "),
         "\nJoin `college` + `year` from the long panel onto the analysis sample by UniqueID.")

  cat("Building real-value measures...\n")
  t0 <- Sys.time()
  samp <- build_real_value(SAMPLE, ref, awards, cpih, base_year = BASE_YEAR)
  cat(sprintf("  done in %.1fs\n", as.numeric(Sys.time() - t0, units = "secs")))

  saveRDS(samp, file.path(derived_dir(), "lsf_real_value_sample.rds"))
  message("Real value coverage: ", round(100*mean(!is.na(samp$real_value_rent_ttwa)),1), "%")
  invisible(samp)
}


# ---------------------------------------------------------------------------
# parse_confidence_workbook()   (was scripts/d1_parse_sample_demographics_data.r)
#
# The BSA financial-confidence cross-tab: one sheet, fixed 7-column layout, a
# stack of demographic tables. Same parser as the funding triangle below.
# ---------------------------------------------------------------------------
parse_confidence_workbook <- function(max_row = 151, n_col = 7) {
  path <- find_in_data("Questionnaire Analysis.*\\.xlsx$")
  message("Reading: ", path)

  grid <- read_grid(path, max_row = max_row, n_col = n_col)
  long <- extract_stacked_tables(grid, n_col = n_col)
  reconcile_check(long)
  print(head(long, 12))

  out_csv <- file.path(derived_dir(), "financial_confidence_long.csv")
  write_csv(long, out_csv)
  message("Written: ", out_csv)
  invisible(long)
}


# ---------------------------------------------------------------------------
# parse_funding_triangle()   (was scripts/d4_parse_funding_triangle.r)
#
# Parse the demographic cross-tab workbook for the funding-decision triangle:
#   LEAVE_COURSE             - retention risk   (Yes/No)
#   FUNDING_INFLUENCE_COURSE - dependence, what (1-5)
#   FUNDING_INFLUENCE_HEI    - dependence, where (1-5)
# into ONE faithful long table. Response labels are read verbatim; no
# interpretation of codes happens here.
#
# Differences from the confidence workbook, all handled here:
#   - THREE sheets, one question each (sheet name -> question via a map).
#   - Sheets differ in WIDTH (yes/no = 4 cols; 1-5 scale = ~7). Width is
#     detected per sheet, not hardcoded.
#   - Each demographic block ends in a Grand Total ROW. That row is asserted
#     against the sum of its groups (catches a missing or double-counted
#     group) and then DROPPED so it never enters analysis.
#   - reconcile_check runs PER SHEET. Run on the combined table it merges any
#     demographic group that shares a denominator across two questions and
#     reports a false ~2x mismatch, so it stays inside parse_sheet.
# ---------------------------------------------------------------------------

# sheet -> question map. sheet_key is the sheet tab name, normalised
# (upper, non-alnum -> "_").
FUNDING_TRIANGLE_QUESTIONS <- function() {
  tribble(
    ~sheet_key,                 ~question,                                                                      ~slug,                      ~response_type,
    "LEAVE_COURSE",             "Over the last year, did you ever feel that you may have to leave your course?", "leave_course",             "binary",
    "FUNDING_INFLUENCE_COURSE", "How important was funding in your decision on what to study?",                 "funding_influence_course", "scale",
    "FUNDING_INFLUENCE_HEI",    "How important was funding to your decision on where to study?",                "funding_influence_hei",    "scale"
  )
}

norm_key <- function(x) {
  x |> str_to_upper() |> str_trim() |>
    str_replace_all("[^A-Z0-9]+", "_") |> str_replace_all("^_|_$", "")
}
is_total_row <- function(x) str_detect(str_to_lower(x), "grand *total|^total$")

# rightmost column carrying any content (so the Grand Total column is found
# whatever the sheet's width)
detect_ncol <- function(g) {
  hit <- vapply(seq_len(ncol(g)),
                function(j) any(!vapply(g[[j]], blank, logical(1))), logical(1))
  if (!any(hit)) NA_integer_ else max(which(hit))
}

parse_triangle_sheet <- function(path, sheet) {
  wide  <- read_grid(path, sheet = sheet, max_row = MAX_ROW, n_col = SCAN_COL)
  n_col <- detect_ncol(wide)
  if (is.na(n_col) || n_col < 3) {
    warning("Sheet '", sheet, "': <3 columns of content; skipped.", call. = FALSE)
    return(NULL)
  }
  grid <- wide[, seq_len(n_col), drop = FALSE]
  long <- extract_stacked_tables(grid, n_col = n_col)
  if (is.null(long) || !nrow(long)) {
    warning("Sheet '", sheet, "': no tables parsed; skipped.", call. = FALSE)
    return(NULL)
  }

  key  <- norm_key(sheet)
  meta <- filter(FUNDING_TRIANGLE_QUESTIONS(), sheet_key == key)
  if (!nrow(meta)) {
    warning("Sheet '", sheet, "' (key ", key, ") not in QUESTION_MAP; ",
            "kept with question = sheet name.", call. = FALSE)
    meta <- tibble(sheet_key = key, question = sheet,
                   slug = str_to_lower(key), response_type = NA_character_)
  }

  long <- long |>
    mutate(sheet         = sheet,
           question      = meta[["question"]][1],
           question_slug = meta[["slug"]][1],
           response_type = meta[["response_type"]][1],
           .before = 1)

  # ---- grand-total ROW: assert, then drop (the double-count trap) ----------
  # One n per group lives on option_index == 1. Sum the groups in each block
  # and compare to the block's Grand Total row.
  check <- long |>
    mutate(.tot = is_total_row(Group)) |>
    filter(option_index == 1) |>
    group_by(Demographic) |>
    summarise(total_row = sum(n[.tot], na.rm = TRUE),
              groups    = sum(n[!.tot], na.rm = TRUE),
              .groups   = "drop") |>
    mutate(gap = groups - total_row)
  bad <- filter(check, total_row > 0, abs(gap) > 0.5)
  if (nrow(bad)) {
    message("!! GRAND-TOTAL MISMATCH on '", sheet,
            "' (a group may be missing or double-counted):")
    walk(seq_len(nrow(bad)), function(i)
      message(sprintf("     %s: groups sum %s vs grand total %s",
                      bad[["Demographic"]][i], bad[["groups"]][i], bad[["total_row"]][i])))
  } else {
    message("Grand-total rows reconcile on '", sheet, "'.")
  }

  data_long <- filter(long, !is_total_row(Group))

  # ---- faithful column reconcile, PER SHEET --------------------------------
  # Each row's option counts should sum to its own total. Run here, not on the
  # combined table, so shared-denominator groups across questions do not
  # produce spurious ~2x mismatches.
  message("-- column reconcile for '", sheet, "':")
  reconcile_check(data_long)

  data_long
}

parse_funding_triangle <- function() {
  # Specific pattern: the folder also holds the confidence workbook, and a loose
  # "Questionnaire Analysis" pattern would match both. Keep this tight.
  path   <- find_in_data("DHSC Questionnaire Analysis_v1.*\\.xlsx$")
  sheets <- readxl::excel_sheets(path)
  message("Reading: ", path)
  message("Sheets:  ", paste(sheets, collapse = " | "))

  long_all <- map(sheets, function(s) parse_triangle_sheet(path, s)) |> compact() |> bind_rows()
  if (!nrow(long_all)) stop("No sheets parsed. Check sheet names and layout.", call. = FALSE)

  out_csv <- file.path(derived_dir(), "funding_triangle_long.csv")
  write_csv(long_all, out_csv)
  message(sprintf("Written: %s  (%d rows | %d questions | %d demographics)",
                  out_csv, nrow(long_all),
                  n_distinct(long_all[["question_slug"]]),
                  n_distinct(long_all[["Demographic"]])))
  print(head(long_all, 12))
  invisible(long_all)
}
