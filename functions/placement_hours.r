# ---------------------------------------------------------------------------
# placement_hours.r  ->  attaches DHSC programme-level average placement hours
# per year (FY26/27) to the LSF analysis sample.
#
# Two reference CSVs:
#   reference/placement_hours.csv    programme_code -> hours_per_year, family
#   reference/course_crosswalk.csv   free-text `course` -> programme_code
#
# The crosswalk is HAND-AUTHORED. `course` in the survey is free text and there
# is no canonical list, so scripts/p0_audit_course_strings.r prints what
# students actually typed and the mapping is completed by eye. Matching here is
# exact on a squished, case-folded key and nothing else. No fuzzy matching: the
# three dental categories (231 / 375 / 747 hours) differ three-fold and a
# plausible-looking wrong match is worse than an honest NA.
#
# Hours are a PROGRAMME-LEVEL, TIME-INVARIANT attribute. Course fixed effects
# absorb them completely - fixest will silently drop the term. That is the
# central constraint of this arm, not a caveat. See PLACEMENT_HOURS_ARMS.md.
#
# Variables attached:
#   hours_per_year   raw, NA where unmatched or where DHSC supplied no value
#   hours_per100     hours_per_year / 100, the modelling unit (KTD3)
#   course_family    nursing / dental / ahp / other, factor
#   hours_band       low / medium / high, cut on STUDENT-weighted tertiles (KTD4)
#   hours_demeaned   hours_per100 minus its family mean, the P2 working variable
# ---------------------------------------------------------------------------

PH_REF_DIR    <- "reference"
PH_COURSE_COL <- "course"
PH_FAMILIES   <- c("nursing", "dental", "ahp", "other")

# normalise a course string for matching. Squish whitespace, case-fold, delete
# apostrophes so "Children's" == "Childrens". Deliberately conservative: it does
# NOT strip words, because "Dental Hygienists" and "Dental Hygiene Therapy" are
# different programmes and any aggressive normalisation collapses them.
ph_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("['\u2019`]", "", x)
  stringr::str_squish(x)
}

# Student-balanced breaks over a coarse, lumpy variable.
#
# Hours take ~23 distinct values across the whole sample and the nursing block
# holds a large share of students at ~700h, so quantile-based tertiles put two
# boundaries on the same value and lose a band. This walks every way of cutting
# the sorted distinct values into `n_bands` contiguous runs and keeps the one
# whose STUDENT counts are closest to equal, falling back to fewer bands when
# there are not enough distinct values to go round.
#
# Returns list(breaks, n_bands, counts) or breaks = NULL if not even 2 bands
# are possible. `breaks` are cut() edges, so the first is below the minimum.
ph_balanced_breaks <- function(v, n_bands = 3L) {
  v <- v[!is.na(v)]
  none <- list(breaks = NULL, n_bands = 0L, counts = integer(0))
  if (!length(v)) return(none)
  tab <- table(v)
  u   <- as.numeric(names(tab))     # sorted distinct values
  cnt <- as.integer(tab)
  k   <- length(u)

  for (nb in seq(from = as.integer(n_bands), to = 2L)) {
    if (k < nb) next
    target <- length(v) / nb
    best <- NULL; best_cost <- Inf
    # split positions: nb-1 indices in 1..(k-1), strictly increasing
    grid <- utils::combn(k - 1L, nb - 1L, simplify = FALSE)
    for (sp in grid) {
      edges  <- c(0L, sp, k)
      counts <- vapply(seq_len(nb), function(i)
        sum(cnt[(edges[i] + 1L):edges[i + 1L]]), integer(1))
      cost <- sum((counts - target)^2)
      if (cost < best_cost) { best_cost <- cost; best <- list(sp = sp, counts = counts) }
    }
    if (is.null(best)) next
    breaks <- c(u[1] - 1e-9, u[best$sp], u[k])
    if (anyDuplicated(breaks)) next
    return(list(breaks = breaks, n_bands = as.integer(nb), counts = best$counts))
  }
  none
}

# ---- reference loaders -----------------------------------------------------
load_placement_hours <- function(dir = PH_REF_DIR) {
  path <- file.path(dir, "placement_hours.csv")
  if (!file.exists(path)) stop("missing ", path, call. = FALSE)
  h <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                       col_types = readr::cols(
                         programme_code = readr::col_character(),
                         programme_name = readr::col_character(),
                         hours_per_year = readr::col_double(),
                         course_family  = readr::col_character()))
  if (anyDuplicated(h$programme_code))
    stop("placement_hours.csv: duplicate programme_code", call. = FALSE)
  bad <- setdiff(unique(h$course_family), PH_FAMILIES)
  if (length(bad))
    stop("placement_hours.csv: unknown course_family: ", paste(bad, collapse = ", "),
         call. = FALSE)
  h
}

load_course_crosswalk <- function(dir = PH_REF_DIR) {
  path <- file.path(dir, "course_crosswalk.csv")
  if (!file.exists(path))
    stop("missing ", path,
         "\n  Run scripts/p0_audit_course_strings.r first, then complete the",
         "\n  template it writes. The crosswalk cannot be generated.", call. = FALSE)
  x <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                       col_types = readr::cols(.default = readr::col_character()))
  if (!all(c("course_raw", "programme_code") %in% names(x)))
    stop("course_crosswalk.csv needs at least course_raw and programme_code", call. = FALSE)
  x <- dplyr::mutate(x, course_key = ph_key(course_raw))
  # A course string mapped to two different programmes is an authoring error,
  # not something to silently resolve.
  dup <- x |>
    dplyr::filter(!is.na(programme_code), nzchar(programme_code)) |>
    dplyr::distinct(course_key, programme_code) |>
    dplyr::count(course_key) |>
    dplyr::filter(n > 1)
  if (nrow(dup))
    stop("course_crosswalk.csv: same course string mapped to >1 programme_code:\n",
         paste(" -", dup$course_key, collapse = "\n"), call. = FALSE)
  x |>
    dplyr::filter(!is.na(programme_code), nzchar(programme_code)) |>
    dplyr::distinct(course_key, .keep_all = TRUE) |>
    dplyr::select(course_key, programme_code)
}

# ---- the attach ------------------------------------------------------------
# Returns the sample with hours variables attached and a `match_report`
# attribute. Row count is preserved: unmatched students keep their row and
# carry NA hours, they are never dropped (KTD5).
attach_placement_hours <- function(sample,
                                   dir        = PH_REF_DIR,
                                   course_col = PH_COURSE_COL,
                                   n_bands    = 3L) {
  if (!course_col %in% names(sample))
    stop("no `", course_col, "` column on the sample", call. = FALSE)
  n_in <- nrow(sample)

  hours <- load_placement_hours(dir)
  xwalk <- load_course_crosswalk(dir)

  out <- sample
  out$.course_key <- ph_key(out[[course_col]])
  out <- dplyr::left_join(out, xwalk, by = c(".course_key" = "course_key"))
  out <- dplyr::left_join(
    out,
    dplyr::select(hours, programme_code, programme_name, hours_per_year, course_family),
    by = "programme_code")

  out$hours_per100  <- out$hours_per_year / 100
  out$course_family <- factor(out$course_family, levels = PH_FAMILIES)

  # ---- bands: cut on student counts, not programme counts (KTD4) -----------
  # Cutting on the 23 programmes would put ~8 programmes in each band however
  # many students sit behind them, and nursing dominates the sample.
  #
  # Plain quantile() does NOT work here. Hours take only ~23 distinct values and
  # one of them (the nursing block, ~700h) holds a large share of students, so a
  # tertile boundary lands inside that mass point, two boundaries collapse onto
  # the same value, and a band comes out empty. ph_balanced_breaks() instead
  # searches every way of splitting the distinct values into n_bands runs and
  # takes the split whose student counts are closest to equal. With ~23 values
  # the search is exhaustive and instant.
  hv   <- out$hours_per100[!is.na(out$hours_per100)]
  brk  <- ph_balanced_breaks(hv, n_bands)
  cuts <- brk$breaks
  band_labels <- c("low", "medium", "high")

  if (is.null(cuts)) {
    out$hours_band <- factor(NA_character_, levels = band_labels, ordered = TRUE)
    message("attach_placement_hours: only ", length(unique(hv)),
            " distinct hours values; cannot cut ", n_bands,
            " bands. hours_band left NA.")
  } else {
    if (brk$n_bands < n_bands)
      message("attach_placement_hours: only ", length(unique(hv)),
              " distinct hours values; cut ", brk$n_bands, " bands, not ", n_bands, ".")
    labs <- if (brk$n_bands == 2L) c("low", "high") else band_labels[seq_len(brk$n_bands)]
    out$hours_band <- cut(out$hours_per100, breaks = cuts, labels = labs,
                          include.lowest = TRUE, ordered_result = TRUE)
    out$hours_band <- factor(as.character(out$hours_band),
                             levels = band_labels, ordered = TRUE)
  }
  # Students with no hours value (unmatched, or a programme DHSC supplied no
  # figure for) are NA on the band. They must not land in "low" - a missing
  # value is not a short placement.
  out$hours_band[is.na(out$hours_per100)] <- NA

  # ---- family-demeaned hours (the P2 working variable) ---------------------
  fam_mean <- out |>
    dplyr::filter(!is.na(hours_per100), !is.na(course_family)) |>
    dplyr::group_by(course_family) |>
    dplyr::summarise(.fam_mean = mean(hours_per100), .groups = "drop")
  out <- dplyr::left_join(out, fam_mean, by = "course_family")
  out$hours_demeaned <- out$hours_per100 - out$.fam_mean
  out$.fam_mean <- NULL

  # ---- match report --------------------------------------------------------
  matched   <- !is.na(out$programme_code)
  has_hours <- !is.na(out$hours_per100)
  unmatched_top <- out[!matched, , drop = FALSE] |>
    dplyr::count(.data[[course_col]], name = "n", sort = TRUE) |>
    dplyr::rename(course_raw = 1) |>
    head(20)

  report <- list(
    n_students        = n_in,
    n_matched         = sum(matched),
    n_unmatched       = sum(!matched),
    match_rate        = if (n_in > 0) sum(matched) / n_in else NA_real_,
    n_with_hours      = sum(has_hours),
    hours_rate        = if (n_in > 0) sum(has_hours) / n_in else NA_real_,
    n_matched_no_hours = sum(matched & !has_hours),
    band_cuts         = cuts,
    band_counts       = table(out$hours_band, useNA = "ifany"),
    family_counts     = table(out$course_family, useNA = "ifany"),
    unmatched_top     = unmatched_top
  )

  out$.course_key <- NULL
  stopifnot(nrow(out) == n_in)
  attr(out, "match_report") <- report
  out
}

# Print the match report. Call this BEFORE any estimate (R7). A crosswalk that
# quietly loses a third of the sample is worse than no crosswalk.
placement_match_report <- function(sample, emit = cat) {
  r <- attr(sample, "match_report")
  if (is.null(r)) stop("no match_report attribute - run attach_placement_hours() first",
                       call. = FALSE)
  emit("=== placement hours: match report ===")
  emit(sprintf("students                 : %s", format(r$n_students, big.mark = ",")))
  emit(sprintf("matched to a programme   : %s  (%.1f%%)",
               format(r$n_matched, big.mark = ","), 100 * r$match_rate))
  emit(sprintf("carrying an hours value  : %s  (%.1f%%)",
               format(r$n_with_hours, big.mark = ","), 100 * r$hours_rate))
  emit(sprintf("matched but no hours     : %s  (DHSC supplied no figure, e.g. Sonography)",
               format(r$n_matched_no_hours, big.mark = ",")))
  if (!is.null(r$band_cuts))
    emit(sprintf("band cut points (per 100h): %s",
                 paste(sprintf("%.2f", r$band_cuts), collapse = " | ")))
  # Index by position, not by name: table(useNA = "ifany") gives one entry an NA
  # name, and x[[NA]] is a subscript error rather than a lookup.
  emit_counts <- function(label, tab) {
    emit(label)
    nms <- names(tab)
    for (i in seq_along(tab))
      emit(sprintf("  %-10s %s",
                   if (is.na(nms[i]) || !nzchar(nms[i])) "(no value)" else nms[i],
                   format(as.integer(tab[i]), big.mark = ",")))
  }
  emit_counts("band counts:",   r$band_counts)
  emit_counts("family counts:", r$family_counts)
  if (nrow(r$unmatched_top)) {
    emit("top unmatched course strings:")
    for (i in seq_len(nrow(r$unmatched_top)))
      emit(sprintf("  %6s  %s", format(r$unmatched_top$n[i], big.mark = ","),
                   r$unmatched_top$course_raw[i]))
  }
  invisible(r)
}

# Convenience: load the sample, attach hours, print the report, return it.
placement_sample <- function(dir = PH_REF_DIR, quiet = FALSE) {
  s <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  s <- attach_placement_hours(s, dir = dir)
  if (!quiet) placement_match_report(s)
  s
}
