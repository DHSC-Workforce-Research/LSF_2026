# ===========================================================================
# functions/analysis_importance.r
#
# analysis_importance()  -  relative-importance (Shapley / LMG) decomposition
# of left_before_finish across seven predictor blocks: course family, specific
# course, entry cohort, place (region or travel-to-work area), real LSF value,
# the five survey funding answers, and the grant-component flags.
#
# QUESTION. Of the variation in left_before_finish this model can explain, how
# much is attributable to each block? Order-independent, so a block does not
# get credited or blamed just because of where it sits in a spec ladder.
#
# METHOD. Fit every one of the 2^7 = 128 subsets S of the seven blocks with a
# plain logistic model (fixest::feglm, binomial): the term blocks in S go on
# the right-hand side, the FE blocks in S are absorbed after "|" (a subset
# with no FE blocks fits without a "|" at all, and the empty subset is
# left_before_finish ~ 1). McFadden pseudo-R2(S) = 1 - deviance(S) /
# deviance(empty). The Shapley value of block j averages the marginal gain in
# R2 from adding j to S over every subset S that does not already contain j,
# weighted so every ORDER of adding the seven blocks counts equally:
#
#   shapley_j = sum over S not containing j of
#               w(|S|) * (R2(S + j) - R2(S)),   w(s) = s! (k-1-s)! / k!
#
# share_pct = 100 * shapley_j / R2(full). This is run twice on the SAME fixed
# estimation sample: geography = "region" and geography = "ttwa" (the place
# block swaps from region to ttwa_code; nothing else changes), so the two
# geography runs share n and their R2 are directly comparable.
#
# ASSOCIATIONAL and DESCRIPTIVE throughout: these are shares of EXPLAINED
# VARIATION, not causal contributions. A block with a large share explains
# more of the variation in leaving that this model can explain; it says
# nothing about whether changing that block would change the leaving rate.
#
# NESTING. course_family is nested inside course: once course fixed effects
# are in, course_family is a strict function of course and fixest drops it as
# collinear. Ordinary "add one block at a time" importance would then have to
# choose an order and hand ALL of the shared explanatory power to whichever of
# family/course goes in first. Shapley instead averages over every order, so
# the two blocks split the variation they share. That splitting is the reason
# this method is used here, not a side effect to work around.
#
# EFFICIENCY. Every subset of the 7 blocks is fit for geography = "region"
# (128 fits). For geography = "ttwa" only the 64 subsets that CONTAIN the
# place block need refitting, because a subset without place does not depend
# on which geography variable place would have used; the other 64 are reused
# from the region cache. 192 model fits in total, keyed on a canonical subset
# id so nothing is fit twice.
#
# Writes to tables_dir():
#   tbl_importance_shapley.csv   geography, block, block_label, shapley_r2,
#                                 share_pct, rank (1 = largest share, within
#                                 geography)
#   tbl_importance_summary.csv   geography, n, k_blocks, n_fits, dev_null,
#                                 dev_full, r2_mcfadden_full, r2_tjur_full,
#                                 auc_full
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
  k <- length(BLOCK_KEYS)

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

  # ---- 4b. separation purge: keep ONE sample across all 192 fits -------------
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

  # ---- 5. fit every subset, cached on a canonical subset id -----------------
  fe_var_of <- function(block, place_var) {
    switch(block,
           family = "course_family", course = "course", cohort = "entry_year",
           place  = place_var)
  }
  term_rhs_of <- function(block) {
    switch(block, real_value = "rv", survey = SURVEY_RHS, components = COMP_RHS)
  }
  fe_blocks   <- c("family", "course", "cohort", "place")
  term_blocks <- c("real_value", "survey", "components")

  fit_one <- function(included, place_var) {
    fe_vars <- vapply(intersect(fe_blocks, included), fe_var_of, character(1), place_var = place_var)
    term_parts <- vapply(intersect(term_blocks, included), term_rhs_of, character(1))
    rhs <- if (length(term_parts)) paste(term_parts, collapse = " + ") else "1"
    fml <- if (length(fe_vars))
      stats::as.formula(paste0("left_before_finish ~ ", rhs, " | ", paste(fe_vars, collapse = " + ")))
    else
      stats::as.formula(paste0("left_before_finish ~ ", rhs))
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

  get_dev <- function(included, geography) {
    id <- subset_id(included, geography)
    cached <- dev_cache[[id]]
    if (!is.null(cached)) return(cached)
    place_var <- if (geography == "region") "region" else "ttwa_code"
    m  <- fit_one(included, place_var)
    dv <- stats::deviance(m)
    dev_cache[[id]] <<- dv
    if (length(included) == k) full_model[[geography]] <<- m
    n_fits <<- n_fits + 1L
    if (n_fits %% 16L == 0L)
      progress(sprintf("importance: %d/%d fits done ...", n_fits, 128L + 64L))
    dv
  }

  r2_of <- function(included, geography) {
    1 - get_dev(included, geography) / get_dev(character(0), geography)
  }

  combos_all <- expand.grid(rep(list(c(FALSE, TRUE)), k))
  names(combos_all) <- BLOCK_KEYS

  progress(sprintf("importance: fitting %d region-geography subsets ...", nrow(combos_all)))
  for (i in seq_len(nrow(combos_all))) {
    included <- BLOCK_KEYS[as.logical(combos_all[i, ])]
    get_dev(included, "region")
  }
  n_fits_region <- n_fits

  combos_place <- combos_all[combos_all$place, , drop = FALSE]
  progress(sprintf("importance: fitting %d ttwa-geography subsets (place-containing only) ...",
                   nrow(combos_place)))
  for (i in seq_len(nrow(combos_place))) {
    included <- BLOCK_KEYS[as.logical(combos_place[i, ])]
    get_dev(included, "ttwa")
  }
  n_fits_ttwa <- n_fits - n_fits_region

  # ---- 6. Shapley values per block, per geography ----------------------------
  shapley_for_block <- function(j, geography) {
    others <- setdiff(BLOCK_KEYS, j)
    total <- 0
    for (s_size in 0:(k - 1L)) {
      w <- factorial(s_size) * factorial(k - 1L - s_size) / factorial(k)
      for (S in utils::combn(others, s_size, simplify = FALSE))
        total <- total + w * (r2_of(c(S, j), geography) - r2_of(S, geography))
    }
    total
  }

  block_label_for <- function(block, geography) {
    if (block == "place") return(if (geography == "region") "Region" else "Travel-to-work area")
    c(family = "Course family", course = "Specific course", cohort = "Entry cohort",
      real_value = "Real LSF value", survey = "Survey funding answers",
      components = "Grant components")[[block]]
  }

  shapley_tbl <- function(geography) {
    r2_full <- r2_of(BLOCK_KEYS, geography)
    purrr::map_dfr(BLOCK_KEYS, function(j) {
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

  progress("importance: computing Shapley shares (region) ...")
  shapley_region <- shapley_tbl("region")
  progress("importance: computing Shapley shares (ttwa) ...")
  shapley_ttwa   <- shapley_tbl("ttwa")

  shapley_all <- bind_rows(shapley_region, shapley_ttwa)
  write_csv(shapley_all, file.path(out, "tbl_importance_shapley.csv"))
  progress(paste0("  wrote tbl_importance_shapley.csv (", nrow(shapley_all), " rows)"))

  # ---- 7. full-model extras: Tjur R2 and rank-based AUC ----------------------
  summary_row <- function(geography, n_fits_geo) {
    fm <- full_model[[geography]]
    y  <- as.integer(fm$y)
    p  <- as.numeric(fitted(fm))
    dv_null <- get_dev(character(0), geography)
    dv_full <- get_dev(BLOCK_KEYS, geography)
    tibble::tibble(
      geography         = geography,
      n                 = n,
      k_blocks          = k,
      n_fits            = n_fits_geo,
      dev_null          = round(dv_null, 2),
      dev_full          = round(dv_full, 2),
      r2_mcfadden_full  = round(1 - dv_full / dv_null, 4),
      r2_tjur_full      = round(mean(p[y == 1L]) - mean(p[y == 0L]), 4),
      auc_full          = round(auc_score(p, y), 4)
    )
  }

  # ---- 8. write summary table -------------------------------------------------
  summary_all <- bind_rows(
    summary_row("region", n_fits_region),
    summary_row("ttwa",   n_fits_ttwa)
  )
  write_csv(summary_all, file.path(out, "tbl_importance_summary.csv"))
  progress(paste0("  wrote tbl_importance_summary.csv (", nrow(summary_all), " rows)"))

  cat("\n=== Relative importance (Shapley): left_before_finish ===\n")
  cat("--- region ---\n")
  print(as.data.frame(shapley_region |> select(rank, block_label, share_pct)), row.names = FALSE)
  cat("--- ttwa ---\n")
  print(as.data.frame(shapley_ttwa |> select(rank, block_label, share_pct)), row.names = FALSE)
  cat("\n")
  print(as.data.frame(summary_all), row.names = FALSE)

  progress(paste0("importance: done (", n_fits, " fits total) -> ", out))
  cat("Wrote: tbl_importance_shapley.csv, tbl_importance_summary.csv\n")

  invisible(TRUE)
}
