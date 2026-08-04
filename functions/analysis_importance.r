# ===========================================================================
# functions/analysis_importance.r
#
# Two relative-importance (Shapley / LMG) decompositions, sharing one engine.
#
# analysis_importance()         - ENTRY frame. Of the variation in
#   left_before_finish this model can explain, how much is attributable to
#   each of seven predictor blocks: course family, specific course, entry
#   cohort, place (region or travel-to-work area), real LSF value, the five
#   survey funding answers, and the grant-component flags?
#
# analysis_importance_hazard()  - HAZARD frame. Of the variation in gone-
#   next-year this model can explain, for a CONTINUING student mid-course,
#   how much is attributable to each of eight predictor blocks: specific
#   course, year of study, survey (calendar) year, place, this year's real
#   LSF value, the five entry funding answers, this year's financial
#   confidence, and this year's considered-leaving flag? Survivorship is the
#   FRAME here, not a bias to correct for: every row has, by construction,
#   reached a continuing wave.
#
# METHOD (shared, importance_shapley_engine() below). Fit every subset S of
# the k blocks with a plain logistic model (fixest::feglm, binomial): the
# term blocks in S go on the right-hand side, the FE blocks in S are absorbed
# after "|" (a subset with no FE blocks fits without a "|" at all, and the
# empty subset is outcome ~ 1). McFadden pseudo-R2(S) = 1 - deviance(S) /
# deviance(empty). The Shapley value of block j averages the marginal gain in
# R2 from adding j to S over every subset S that does not already contain j,
# weighted so every ORDER of adding the blocks counts equally:
#
#   shapley_j = sum over S not containing j of
#               w(|S|) * (R2(S + j) - R2(S)),   w(s) = s! (k-1-s)! / k!
#
# share_pct = 100 * shapley_j / R2(full). Both decompositions run this twice
# on the SAME fixed estimation sample: geography = "region" and geography =
# "ttwa" (the place block swaps from region to ttwa_code; nothing else
# changes), so the two geography runs share n and their R2 are directly
# comparable.
#
# ASSOCIATIONAL and DESCRIPTIVE throughout, in BOTH frames: these are shares
# of EXPLAINED VARIATION, not causal contributions. A block with a large
# share explains more of the variation this model can explain; it says
# nothing about whether changing that block would change the leaving rate.
#
# NESTING. In the entry frame, course_family is nested inside course: once
# course fixed effects are in, course_family is a strict function of course
# and fixest drops it as collinear. Ordinary "add one block at a time"
# importance would then have to choose an order and hand ALL of the shared
# explanatory power to whichever of family/course goes in first. Shapley
# instead averages over every order, so the two blocks split the variation
# they share. That splitting is the reason this method is used here, not a
# side effect to work around.
#
# HAZARD-FRAME-SPECIFIC CAVEATS, on top of the associational point above:
#   (a) descriptive shares of explained variation, not causal, as above;
#   (b) confidence and considered-leaving (leave_course) are partly SYMPTOMS
#       of impending exit, not independent predictors of it, so their shares
#       must not be read as causes of leaving;
#   (c) the same student contributes several student-years to this panel;
#       the deviance-based shares are point estimates on that panel and no
#       inference (a p-value, a confidence interval) is quoted anywhere in
#       this decomposition.
# The hazard frame drops two of the entry frame's blocks, on purpose:
#   - no course_family block: the entry decomposition already shows course
#     absorbs almost all of what course_family would explain (course fixed
#     effects make course_family collinear and it gets dropped), so it adds
#     nothing here;
#   - no components block: Arm 2's wave-specific real LSF (functions/
#     analysis_hazard_recruitment.r) is built on the FULL nominal package
#     (parental + specialist top-ups already folded into rv_gbp_wave before
#     scaling), so a separate grant-components block would just be re-asking
#     a question the real_value block already answers.
#
# EFFICIENCY. Every subset of the k blocks is fit for the PRIMARY geography
# (region): 2^k fits. For the SECONDARY geography (ttwa) only the 2^(k-1)
# subsets that CONTAIN the place block need refitting, because a subset
# without place does not depend on which geography variable place would have
# used; the rest are reused from the region cache. 3 * 2^(k-1) model fits in
# total (192 for the entry frame's k=7, 384 for the hazard frame's k=8),
# keyed on a canonical subset id so nothing is fit twice.
#
# REFACTOR NOTE (2026, addendum task). The subset-fitting + Shapley machinery
# used to live inline inside analysis_importance() only. It is now
# importance_shapley_engine(), an internal helper shared by both wrappers.
# analysis_importance() builds its sample and block definitions exactly as
# before and hands them to the engine; it is a verbatim refactor and its
# numbers are unchanged (observed on the work machine: region shares course
# 47.47, survey 29.35, region 10.71, components 5.29, cohort 2.95, rv 2.53,
# family 1.69; r2_mcfadden_full 0.0233; n 120,941).
#
# Writes to tables_dir():
#   tbl_importance_shapley.csv          geography, block, block_label,
#                                        shapley_r2, share_pct, rank (1 =
#                                        largest share, within geography)
#   tbl_importance_summary.csv          geography, n, k_blocks, n_fits,
#                                        dev_null, dev_full,
#                                        r2_mcfadden_full, r2_tjur_full,
#                                        auc_full
#   tbl_importance_hazard_shapley.csv   same schema, hazard frame
#   tbl_importance_hazard_summary.csv   same schema, hazard frame
# ===========================================================================

# Fallback course_family regex, used ONLY when reference/placement_hours.csv
# or reference/course_crosswalk.csv is missing (e.g. an annex-only checkout
# with no DHSC placement-hours data). Maps the free-text `course` string to
# the same four levels as placement_hours.csv (nursing / dental / ahp /
# other) by keyword, most-specific pattern first. Coarser than the crosswalk
# by design: it exists to keep this arm running when the crosswalk is
# absent, not to replace it.
importance_course_family_regex <- function(course) {
  x <- tolower(as.character(course))
  out <- rep("other", length(x))
  is_ahp <- grepl(paste(
    "radiograph", "dietit", "occupational therap", "operating department",
    "\\bodp\\b", "orthopt", "orthotist", "prosthet", "paramedic",
    "physiotherap", "podiatr", "speech.*language", "\\bslt\\b", "sonograph",
    sep = "|"), x)
  is_nurse  <- grepl("nurs|midwif", x)
  is_dental <- grepl("dental|dentist", x)
  out[is_ahp]    <- "ahp"
  out[is_nurse]  <- "nursing"
  out[is_dental] <- "dental"
  out
}

# ===========================================================================
# importance_shapley_engine()  -  shared subset-fitting + Shapley machinery.
#
# Fits every subset of a set of predictor BLOCKS with fixest::feglm(binomial)
# against `outcome` in data frame `d`, caches deviances on a canonical subset
# id, and returns the Shapley (LMG) decomposition for the PRIMARY geography
# (every subset) and the SECONDARY geography (place-containing subsets only,
# reusing the rest from the primary cache). Knows nothing about what the
# blocks MEAN; the caller supplies that via block_def. Used by both
# analysis_importance() and analysis_importance_hazard() so the subset
# enumeration, caching and Shapley maths are identical in both.
#
# block_def: a named list, one entry per block, IN THE ORDER blocks should be
#   reported. Each entry is list(type = "fe" | "term", var = <the FE variable
#   name for an "fe" block, or the term RHS string for a "term" block; NULL
#   for the "place" block, whose FE variable is resolved per geography via
#   place_var>, label = <a string, or - for a block whose label depends on
#   geography, i.e. "place" - a named list keyed by geography>). block_def
#   MUST contain a block named "place".
# place_var: named character vector, geography name -> FE variable name in
#   `d`, e.g. c(region = "region", ttwa = "ttwa_code"). Its FIRST name is the
#   PRIMARY geography (every subset fit); its SECOND is the SECONDARY
#   geography (place-containing subsets only, the sensitivity run).
# ===========================================================================
importance_shapley_engine <- function(d, outcome, block_def, place_var,
                                      progress_label = "importance",
                                      progress_every = 16L) {
  block_keys <- names(block_def)
  stopifnot("block_def must include a 'place' block" = "place" %in% block_keys)
  k <- length(block_keys)
  geography_primary   <- names(place_var)[1]
  geography_secondary <- names(place_var)[2]

  fe_blocks   <- block_keys[vapply(block_def, function(b) identical(b$type, "fe"),   logical(1))]
  term_blocks <- block_keys[vapply(block_def, function(b) identical(b$type, "term"), logical(1))]

  fe_var_of <- function(block, geography) {
    if (identical(block, "place")) return(unname(place_var[[geography]]))
    block_def[[block]]$var
  }
  term_rhs_of <- function(block) block_def[[block]]$var

  block_label_for <- function(block, geography) {
    lab <- block_def[[block]]$label
    if (is.list(lab)) lab[[geography]] else lab
  }

  fit_one <- function(included, geography) {
    fe_vars <- vapply(intersect(fe_blocks, included), fe_var_of, character(1), geography = geography)
    term_parts <- vapply(intersect(term_blocks, included), term_rhs_of, character(1))
    rhs <- if (length(term_parts)) paste(term_parts, collapse = " + ") else "1"
    fml <- if (length(fe_vars))
      stats::as.formula(paste0(outcome, " ~ ", rhs, " | ", paste(fe_vars, collapse = " + ")))
    else
      stats::as.formula(paste0(outcome, " ~ ", rhs))
    fixest::feglm(fml, family = binomial(), data = d, warn = FALSE, notes = FALSE)
  }

  subset_id <- function(included, geography) {
    if (!length(included)) return("EMPTY")
    base <- paste(sort(included), collapse = "_")
    if ("place" %in% included) paste0(base, "__", geography) else base
  }

  dev_cache   <- list()
  full_model  <- list()
  n_fits      <- 0L
  total_fits  <- as.integer(3L * 2L^(k - 1L))   # 2^k primary + 2^(k-1) secondary

  get_dev <- function(included, geography) {
    id <- subset_id(included, geography)
    cached <- dev_cache[[id]]
    if (!is.null(cached)) return(cached)
    m  <- fit_one(included, geography)
    dv <- stats::deviance(m)
    dev_cache[[id]] <<- dv
    if (length(included) == k) full_model[[geography]] <<- m
    n_fits <<- n_fits + 1L
    if (n_fits %% progress_every == 0L)
      progress(sprintf("%s: %d/%d fits done ...", progress_label, n_fits, total_fits))
    dv
  }

  r2_of <- function(included, geography) {
    1 - get_dev(included, geography) / get_dev(character(0), geography)
  }

  combos_all <- expand.grid(rep(list(c(FALSE, TRUE)), k))
  names(combos_all) <- block_keys

  progress(sprintf("%s: fitting %d %s-geography subsets ...",
                   progress_label, nrow(combos_all), geography_primary))
  for (i in seq_len(nrow(combos_all))) {
    included <- block_keys[as.logical(combos_all[i, ])]
    get_dev(included, geography_primary)
  }
  n_fits_primary <- n_fits

  combos_place <- combos_all[combos_all$place, , drop = FALSE]
  progress(sprintf("%s: fitting %d %s-geography subsets (place-containing only) ...",
                   progress_label, nrow(combos_place), geography_secondary))
  for (i in seq_len(nrow(combos_place))) {
    included <- block_keys[as.logical(combos_place[i, ])]
    get_dev(included, geography_secondary)
  }
  n_fits_secondary <- n_fits - n_fits_primary

  shapley_for_block <- function(j, geography) {
    others <- setdiff(block_keys, j)
    total <- 0
    for (s_size in 0:(k - 1L)) {
      w <- factorial(s_size) * factorial(k - 1L - s_size) / factorial(k)
      for (S in utils::combn(others, s_size, simplify = FALSE))
        total <- total + w * (r2_of(c(S, j), geography) - r2_of(S, geography))
    }
    total
  }

  shapley_tbl <- function(geography) {
    r2_full <- r2_of(block_keys, geography)
    purrr::map_dfr(block_keys, function(j) {
      sv <- shapley_for_block(j, geography)
      tibble::tibble(
        geography   = geography,
        block       = j,
        block_label = block_label_for(j, geography),
        shapley_r2  = round(sv, 6),
        share_pct   = round(100 * sv / r2_full, 2)
      )
    }) |>
      arrange(desc(share_pct)) |>
      mutate(rank = row_number())
  }

  progress(sprintf("%s: computing Shapley shares (%s) ...", progress_label, geography_primary))
  shapley_p <- shapley_tbl(geography_primary)
  progress(sprintf("%s: computing Shapley shares (%s) ...", progress_label, geography_secondary))
  shapley_s <- shapley_tbl(geography_secondary)
  shapley_all <- bind_rows(shapley_p, shapley_s)

  summary_row <- function(geography, n_fits_geo) {
    fm <- full_model[[geography]]
    y  <- as.integer(fm$y)
    p  <- as.numeric(fitted(fm))
    dv_null <- get_dev(character(0), geography)
    dv_full <- get_dev(block_keys, geography)
    tibble::tibble(
      geography         = geography,
      n                 = nrow(d),
      k_blocks          = k,
      n_fits            = n_fits_geo,
      dev_null          = round(dv_null, 2),
      dev_full          = round(dv_full, 2),
      r2_mcfadden_full  = round(1 - dv_full / dv_null, 4),
      r2_tjur_full      = round(mean(p[y == 1L]) - mean(p[y == 0L]), 4),
      auc_full          = round(auc_score(p, y), 4)
    )
  }

  summary_all <- bind_rows(
    summary_row(geography_primary,   n_fits_primary),
    summary_row(geography_secondary, n_fits_secondary)
  )

  list(
    shapley              = shapley_all,
    summary              = summary_all,
    n_fits               = n_fits,
    geography_primary    = geography_primary,
    geography_secondary  = geography_secondary
  )
}

# ===========================================================================
# analysis_importance()  -  ENTRY frame (k = 7). See file header.
# ===========================================================================
analysis_importance <- function() {
  out <- tables_dir()

  # ---- 1. entry sample, exactly as analysis_real_value() builds it ---------
  progress("importance: loading reference data and entry sample ...")

  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

  samp <- rv_entry_sample(ref, awards, cpih, .label = "importance (entry)")

  # --- type coercion, copied from analysis_real_value.r verbatim -------------
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

  # ---- 2. attach course_family and ttwa_code --------------------------------
  progress("importance: attaching course_family ...")

  ph_files_present <- file.exists(file.path(REF_DIR, "placement_hours.csv")) &&
    file.exists(file.path(REF_DIR, "course_crosswalk.csv"))

  fam_out <- NULL
  if (ph_files_present) {
    fam_out <- tryCatch(attach_placement_hours(samp, dir = REF_DIR), error = function(e) {
      message("importance: attach_placement_hours() failed (", conditionMessage(e),
              "); falling back to course-string regex for course_family.")
      NULL
    })
  }

  if (!is.null(fam_out)) {
    message("importance: course_family via the placement-hours crosswalk (attach_placement_hours()).")
    samp$course_family <- as.character(fam_out$course_family)
    # courses absent from the crosswalk would otherwise drop out of the fixed
    # sample as NA; fill them from the regex fallback instead of losing them
    na_fam <- is.na(samp$course_family)
    if (any(na_fam)) {
      samp$course_family[na_fam] <- importance_course_family_regex(samp$course[na_fam])
      message("importance: ", sum(na_fam),
              " rows had no crosswalk course_family; filled via the regex fallback.")
    }
  } else {
    message("importance: reference/placement_hours.csv or course_crosswalk.csv absent; ",
            "course_family via regex fallback on `course` (importance_course_family_regex()).")
    samp$course_family <- importance_course_family_regex(samp$course)
  }
  samp$course_family <- factor(samp$course_family, levels = PH_FAMILIES)

  progress("importance: attaching ttwa_code ...")
  ttwa_map <- dplyr::distinct(ref, lad_code, ttwa_code)
  stopifnot("ttwa_map is not unique by lad_code - the ttwa join would fan out" =
              !anyDuplicated(ttwa_map$lad_code))
  n_before_ttwa <- nrow(samp)
  samp <- dplyr::left_join(samp, ttwa_map, by = "lad_code")
  stopifnot("ttwa_code join changed row count - fan-out" = nrow(samp) == n_before_ttwa)

  # ---- 3 & 4. blocks and ONE fixed estimation sample ------------------------
  # FE blocks: family, course, cohort, place. Term blocks: real_value, survey,
  # components. "place" is region for the headline run and ttwa_code for the
  # sensitivity run; both variables have to be non-missing in the SAME sample
  # so the two geographies share n and their R2 are comparable.
  BLOCK_KEYS <- c("family", "course", "cohort", "place",
                  "real_value", "survey", "components")

  need_vars <- unique(c(
    "left_before_finish", "course_family", "course", "entry_year",
    "region", "ttwa_code", "rv", SURVEY_VARS, COMP_VARS
  ))
  miss <- setdiff(need_vars, names(samp))
  if (length(miss))
    stop("analysis_importance: sample is missing: ", paste(miss, collapse = ", "), call. = FALSE)

  ok <- stats::complete.cases(samp[need_vars])
  d  <- as.data.frame(samp[ok, , drop = FALSE])
  n  <- nrow(d)
  progress(sprintf("importance: fixed estimation sample n=%s (complete cases incl. region + ttwa_code) ...",
                   format(n, big.mark = ",")))
  if (n < 50L) stop("analysis_importance: fixed estimation sample too small (n=", n, ")", call. = FALSE)

  d$course        <- factor(d$course)
  d$entry_year    <- factor(d$entry_year)
  d$course_family <- factor(as.character(d$course_family), levels = PH_FAMILIES)
  d$region        <- as.character(d$region)
  d$ttwa_code     <- as.character(d$ttwa_code)

  # ---- 4b. separation purge: keep ONE sample across all fits -----------------
  # A binomial feglm silently DROPS observations in any fixed-effect level
  # whose outcome is constant (a course where every student left, say). Those
  # drops happen only in the subsets that contain that FE block, so deviances
  # would be computed on different rows in different subsets and the R2
  # differences the Shapley values are built from would not be comparable.
  # Purge such levels here, iterating until stable (removing a course can make
  # another level constant), so every subset fits on the identical sample.
  fe_all <- c("course_family", "course", "cohort_chr", "region", "ttwa_code")
  d$cohort_chr <- as.character(d$entry_year)
  repeat {
    drop <- rep(FALSE, nrow(d))
    for (v in fe_all) {
      mu <- tapply(d$left_before_finish, d[[v]], mean)
      bad <- names(mu)[!is.na(mu) & (mu == 0 | mu == 1)]
      if (length(bad)) drop <- drop | (as.character(d[[v]]) %in% bad)
    }
    if (!any(drop)) break
    d <- d[!drop, , drop = FALSE]
    d$course        <- droplevels(d$course)
    d$course_family <- droplevels(d$course_family)
    d$entry_year    <- droplevels(d$entry_year)
  }
  d$cohort_chr <- NULL
  if (nrow(d) < n) {
    message("importance: dropped ", n - nrow(d),
            " rows in outcome-constant FE levels (separation purge); n now ",
            format(nrow(d), big.mark = ","))
    n <- nrow(d)
  }
  if (n < 50L) stop("analysis_importance: sample too small after separation purge (n=", n, ")", call. = FALSE)

  # ---- 5. block definitions and the shared engine ----------------------------
  block_def <- list(
    family     = list(type = "fe",   var = "course_family", label = "Course family"),
    course     = list(type = "fe",   var = "course",         label = "Specific course"),
    cohort     = list(type = "fe",   var = "entry_year",     label = "Entry cohort"),
    place      = list(type = "fe",   var = NULL,
                      label = list(region = "Region", ttwa = "Travel-to-work area")),
    real_value = list(type = "term", var = "rv",             label = "Real LSF value"),
    survey     = list(type = "term", var = SURVEY_RHS,       label = "Survey funding answers"),
    components = list(type = "term", var = COMP_RHS,         label = "Grant components")
  )
  stopifnot(identical(names(block_def), BLOCK_KEYS))

  eng <- importance_shapley_engine(
    d, outcome = "left_before_finish", block_def = block_def,
    place_var = c(region = "region", ttwa = "ttwa_code"),
    progress_label = "importance", progress_every = 16L
  )

  write_csv(eng$shapley, file.path(out, "tbl_importance_shapley.csv"))
  progress(paste0("  wrote tbl_importance_shapley.csv (", nrow(eng$shapley), " rows)"))

  write_csv(eng$summary, file.path(out, "tbl_importance_summary.csv"))
  progress(paste0("  wrote tbl_importance_summary.csv (", nrow(eng$summary), " rows)"))

  cat("\n=== Relative importance (Shapley): left_before_finish ===\n")
  cat("--- region ---\n")
  print(as.data.frame(eng$shapley[eng$shapley$geography == "region", ] |>
                        select(rank, block_label, share_pct)), row.names = FALSE)
  cat("--- ttwa ---\n")
  print(as.data.frame(eng$shapley[eng$shapley$geography == "ttwa", ] |>
                        select(rank, block_label, share_pct)), row.names = FALSE)
  cat("\n")
  print(as.data.frame(eng$summary), row.names = FALSE)

  progress(paste0("importance: done (", eng$n_fits, " fits total) -> ", out))
  cat("Wrote: tbl_importance_shapley.csv, tbl_importance_summary.csv\n")

  invisible(TRUE)
}

# ===========================================================================
# analysis_importance_hazard()  -  HAZARD frame (k = 8). See file header for
# the question, the caveats and why course_family and components are dropped.
#
# Frame: rebuilds the student-year hazard panel the same way
# analysis_hazard_recruitment()'s Arm 2 does (functions/
# analysis_hazard_recruitment.r): the long panel + trajectory anchors give
# at_risk and left_next (gone next year), and the wave-specific real LSF is
# built on the FULL nominal package (parental + specialist top-ups), exactly
# as Arm 2's wave_rv is. Restricted here to CONTINUING waves where financial
# confidence was asked (confidence non-missing falls out of the complete-case
# restriction below, since confidence and leave_course are continuing-wave
# survey items - see analysis_findings_pack.r). year_of_study is derived as
# year - course_first_year_wave + 1.
# ===========================================================================
analysis_importance_hazard <- function() {
  out <- tables_dir()

  # ---- 1. hazard panel, built the same way analysis_hazard_recruitment()'s
  # Arm 2 builds it ------------------------------------------------------------
  progress("importance (hazard): loading reference data and hazard panel ...")

  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)
  SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))

  long_path <- file.path(derived_dir(), "lsf_panel_long_2020_2026.csv")
  if (!file.exists(long_path))
    stop("analysis_importance_hazard: need long panel; run 01_data.r first.", call. = FALSE)

  long <- read_csv(
    long_path,
    col_select = c(UniqueID, year, first_year, course, college, confidence, leave_course),
    show_col_types = FALSE
  ) |>
    mutate(
      year = as.integer(year),
      first_year = as.logical(first_year),
      confidence = suppressWarnings(as.integer(confidence)),
      leave_course = to_01(leave_course)
    )

  traj_path <- file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv")
  if (file.exists(traj_path)) {
    tr <- read_csv(traj_path, show_col_types = FALSE) |>
      select(UniqueID, any_of(c("last_wave", "course_first_year_wave", "expected_finish",
                                "n_waves", "first_wave")))
  } else {
    tr <- long |>
      group_by(UniqueID) |>
      summarise(
        first_wave = min(year, na.rm = TRUE),
        last_wave  = max(year, na.rm = TRUE),
        n_waves = n(), .groups = "drop"
      ) |>
      mutate(course_first_year_wave = first_wave, expected_finish = NA_integer_)
  }
  long <- long |> left_join(tr, by = "UniqueID")

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
      course_use  = dplyr::coalesce(course, course_entry),
      at_risk = !is.na(expected_finish) & year < expected_finish & year <= 2024L,
      left_next = as.integer(at_risk & year == last_wave),
      year_of_study = year - course_first_year_wave + 1L
    )

  # Wave-specific real LSF, FULL PACKAGE (parental + specialist top-ups) - the
  # same construct Arm 2 uses, NOT the core-only measure analysis_findings_
  # pack()'s lag models use (see the note in that file).
  if (!"parental" %in% names(SAMPLE)) SAMPLE$parental <- FALSE
  if (!"specialist" %in% names(SAMPLE)) SAMPLE$specialist <- FALSE
  flags <- SAMPLE |>
    transmute(UniqueID, parental = to_01(parental), specialist = to_01(specialist))

  wave_samp <- long |>
    left_join(flags, by = "UniqueID") |>
    transmute(
      UniqueID, year,
      college = college_use,
      entry_year = year,   # join year for costs (build_real_value's year column)
      course = course_use,
      parental = coalesce(parental, 0L),
      specialist = coalesce(specialist, 0L)
    ) |>
    distinct()

  wave_rv <- build_real_value(
    as.data.frame(wave_samp), ref, awards, cpih, base_year = 2020,
    provider_col = "college", year_col = "entry_year",
    .label = "importance (hazard wave)"
  ) |>
    transmute(
      UniqueID, year,
      rv_gbp_wave = as.numeric(.data[[PRIMARY]]),
      region = region,
      lad_code = lad_code
    )

  panel <- long |>
    left_join(wave_rv, by = c("UniqueID", "year")) |>
    filter(!is.na(rv_gbp_wave))

  progress("importance (hazard): attaching ttwa_code and entry funding answers ...")
  ttwa_map <- dplyr::distinct(ref, lad_code, ttwa_code)
  stopifnot("ttwa_map is not unique by lad_code - the ttwa join would fan out" =
              !anyDuplicated(ttwa_map$lad_code))
  n_before_ttwa <- nrow(panel)
  panel <- dplyr::left_join(panel, ttwa_map, by = "lad_code")
  stopifnot("ttwa_code join changed row count - fan-out" = nrow(panel) == n_before_ttwa)

  # ---- 2. entry funding answers (SURVEY_VARS), same coercions as
  # analysis_importance() -------------------------------------------------------
  entry <- rv_entry_sample(ref, awards, cpih, .label = "importance (hazard entry funding)") |>
    mutate(
      crit_course = as.integer(suppressWarnings(as.integer(funding_imp_crse)) >= 4L),
      crit_uni    = as.integer(suppressWarnings(as.integer(funding_imp_uni))  >= 4L),
      fund_availability = to_01(fund_availability),
      grant_influence   = to_01(grant_influence),
      grant_helps_stay  = to_01(grant_helps_stay)
    ) |>
    select(UniqueID, all_of(SURVEY_VARS))

  panel <- panel |>
    left_join(entry, by = "UniqueID") |>
    mutate(
      rv     = as.numeric(scale(rv_gbp_wave)),
      course = as.character(course_use)
    )

  # ---- 3. ONE fixed estimation sample: continuing waves, confidence asked ----
  need_vars <- unique(c(
    "left_next", "course", "year_of_study", "year", "region", "ttwa_code",
    "rv", SURVEY_VARS, "confidence", "leave_course"
  ))
  miss <- setdiff(need_vars, names(panel))
  if (length(miss))
    stop("analysis_importance_hazard: sample is missing: ", paste(miss, collapse = ", "), call. = FALSE)

  ok <- panel$at_risk %in% TRUE & stats::complete.cases(panel[need_vars])
  d  <- as.data.frame(panel[ok, , drop = FALSE])
  n  <- nrow(d)
  progress(sprintf("importance (hazard): fixed estimation sample n=%s (continuing waves, complete cases incl. region + ttwa_code) ...",
                   format(n, big.mark = ",")))
  if (n < 50L) stop("analysis_importance_hazard: fixed estimation sample too small (n=", n, ")", call. = FALSE)

  d$course        <- factor(d$course)
  d$year_of_study <- as.integer(d$year_of_study)
  d$region        <- as.character(d$region)
  d$ttwa_code     <- as.character(d$ttwa_code)

  # ---- 3b. separation purge, same rule as analysis_importance(), FE list
  # widened to include year_of_study and calendar year -------------------------
  fe_all <- c("course", "year_of_study_chr", "year_chr", "region", "ttwa_code")
  d$year_of_study_chr <- as.character(d$year_of_study)
  d$year_chr          <- as.character(d$year)
  repeat {
    drop <- rep(FALSE, nrow(d))
    for (v in fe_all) {
      mu <- tapply(d$left_next, d[[v]], mean)
      bad <- names(mu)[!is.na(mu) & (mu == 0 | mu == 1)]
      if (length(bad)) drop <- drop | (as.character(d[[v]]) %in% bad)
    }
    if (!any(drop)) break
    d <- d[!drop, , drop = FALSE]
    d$course <- droplevels(d$course)
  }
  d$year_of_study_chr <- NULL
  d$year_chr <- NULL
  if (nrow(d) < n) {
    message("importance (hazard): dropped ", n - nrow(d),
            " rows in outcome-constant FE levels (separation purge); n now ",
            format(nrow(d), big.mark = ","))
    n <- nrow(d)
  }
  if (n < 50L) stop("analysis_importance_hazard: sample too small after separation purge (n=", n, ")", call. = FALSE)

  d$year_of_study <- factor(d$year_of_study)
  d$year_f        <- factor(d$year)

  # ---- 4. block definitions (k = 8) and the shared engine --------------------
  BLOCK_KEYS_HZ <- c("course", "study_year", "survey_year", "place",
                     "real_value", "funding_entry", "confidence", "considered")
  block_def_hz <- list(
    course        = list(type = "fe",   var = "course",        label = "Specific course"),
    study_year    = list(type = "fe",   var = "year_of_study",  label = "Year of study"),
    survey_year   = list(type = "fe",   var = "year_f",         label = "Survey year"),
    place         = list(type = "fe",   var = NULL,
                         label = list(region = "Region", ttwa = "Travel-to-work area")),
    real_value    = list(type = "term", var = "rv",                    label = "Real LSF value (this year)"),
    funding_entry = list(type = "term", var = SURVEY_RHS,              label = "Entry funding answers"),
    confidence    = list(type = "term", var = "factor(confidence)",    label = "Financial confidence (this year)"),
    considered    = list(type = "term", var = "leave_course",          label = "Considered leaving (this year)")
  )
  stopifnot(identical(names(block_def_hz), BLOCK_KEYS_HZ))

  eng <- importance_shapley_engine(
    d, outcome = "left_next", block_def = block_def_hz,
    place_var = c(region = "region", ttwa = "ttwa_code"),
    progress_label = "importance (hazard)", progress_every = 32L
  )

  write_csv(eng$shapley, file.path(out, "tbl_importance_hazard_shapley.csv"))
  progress(paste0("  wrote tbl_importance_hazard_shapley.csv (", nrow(eng$shapley), " rows)"))

  write_csv(eng$summary, file.path(out, "tbl_importance_hazard_summary.csv"))
  progress(paste0("  wrote tbl_importance_hazard_summary.csv (", nrow(eng$summary), " rows)"))

  cat("\n=== Relative importance (Shapley), hazard frame: gone next year ===\n")
  cat("--- region ---\n")
  print(as.data.frame(eng$shapley[eng$shapley$geography == "region", ] |>
                        select(rank, block_label, share_pct)), row.names = FALSE)
  cat("--- ttwa ---\n")
  print(as.data.frame(eng$shapley[eng$shapley$geography == "ttwa", ] |>
                        select(rank, block_label, share_pct)), row.names = FALSE)
  cat("\n")
  print(as.data.frame(eng$summary), row.names = FALSE)

  progress(paste0("importance (hazard): done (", eng$n_fits, " fits total) -> ", out))
  cat("Wrote: tbl_importance_hazard_shapley.csv, tbl_importance_hazard_summary.csv\n")

  invisible(TRUE)
}
