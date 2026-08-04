# ===========================================================================
# functions/deck_builders_importance.r
#
# build_importance_slide()  -  horizontal bar chart of the Shapley (LMG)
# decomposition of left_before_finish across the seven predictor blocks that
# analysis_importance() (functions/analysis_importance.r) fits: course family,
# specific course, entry cohort, region, real LSF value, survey funding
# answers, grant components. One bar per block, region geography, ranked by
# share_pct descending (largest share at the top of the chart).
#
# build_importance_hazard_slide()  -  the same ranked-bar design, reading
# tbl_importance_hazard_shapley.csv / tbl_importance_hazard_summary.csv, for
# the HAZARD-frame decomposition analysis_importance_hazard() fits: of what
# is known about a CONTINUING student mid-course, how much of who is gone
# next year is attributable to each of eight blocks. The footnote adds the
# symptom caveat for financial confidence and considered-leaving (see the
# header of functions/analysis_importance.r): those two blocks are partly
# SYMPTOMS of impending exit, not independent predictors of it.
#
# STANDALONE, DELIBERATELY NOT wired into functions/deck_manifest.r:
# feat/deck-text is renumbering the deck in parallel and neither slide has a
# fixed slot yet. Both are still sourced automatically, because
# scripts/00_config.r walks every file under functions/ with source(), so
# wrap_title() / wrap_sub() / wrapcap() / deck_palette() (functions/deck_
# builders.r), theme_dhsc_slide() (functions/deck_helpers.r) and dhsc_cols
# (functions/dhsc_theme.r) are all in scope by the time either builder is
# actually called. This file never saves a plot or picks a slide number, same
# contract as every builder in deck_builders.r; the caller does both.
#
# Locating the source tables: every *_shapley.csv / *_summary.csv pair is
# found with deck_table() (functions/deck_helpers.r), which recursively
# searches under outputs_dir() and derived_dir() for the newest file matching
# the name and does not assume a folder - the same rule tests/
# check_numbers.r's find_newest() uses. Run 02_analysis.r first so these
# tables exist somewhere under outputs_dir().
#
# Usage (after 02_analysis.r has run):
#
#   source("scripts/00_config.r")
#   suppressMessages({ library(dplyr); library(readr); library(stringr)
#                      library(ggplot2); library(tibble) })
#   p <- build_importance_slide()
#   save_slide(p, file.path(deck_dir(), "slide_XX_importance.png"))
#   p2 <- build_importance_hazard_slide()
#   save_slide(p2, file.path(deck_dir(), "slide_XX_importance_hazard.png"))
#
# ASCII only in this file except the pound sign; Windows source() truncates at
# the first exotic byte.
# ===========================================================================

build_importance_slide <- function() {
  p   <- deck_palette()
  shp <- deck_table("tbl_importance_shapley.csv")
  smy <- deck_table("tbl_importance_summary.csv")

  # ---- region-geography rows only, ranked descending by share_pct ---------
  shp <- shp[shp$geography == "region", ]
  if (!nrow(shp))
    stop("build_importance_slide: no geography == 'region' rows in ",
         "tbl_importance_shapley.csv.", call. = FALSE)
  shp <- shp[order(shp$share_pct), ]                       # ascending: geom_col
  shp$block_label <- factor(shp$block_label, levels = unique(shp$block_label))  # puts smallest bar at
                                                             # the bottom, largest at the top

  smy <- smy[smy$geography == "region", ]
  if (nrow(smy) != 1L)
    stop("build_importance_slide: expected exactly one geography == 'region' ",
         "row in tbl_importance_summary.csv, found ", nrow(smy), ".", call. = FALSE)
  r2_pct <- round(100 * smy$r2_mcfadden_full[1])
  n_val  <- format(smy$n[1], big.mark = ",", trim = TRUE)

  ggplot(shp, aes(share_pct, block_label)) +
    geom_col(fill = p$teal, width = 0.68) +
    geom_text(aes(label = sprintf("%.0f%%", share_pct)),
              hjust = -0.25, size = 5, colour = p$ink) +
    scale_x_continuous(limits = c(0, max(shp$share_pct) * 1.18),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = wrap_title("What explains leaving, and by how much"),
      subtitle = wrap_sub(sprintf(
        "The model explains %d%% of the variation in leaving before finishing (pseudo-R2), n = %s.",
        r2_pct, n_val)),
      x = "Share of explained variation (%)", y = NULL,
      caption = wrapcap(paste(
        "Shares are order-independent (Shapley decomposition over predictor blocks).",
        "Descriptive, not causal."))
    ) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank())
}

build_importance_hazard_slide <- function() {
  p   <- deck_palette()
  shp <- deck_table("tbl_importance_hazard_shapley.csv")
  smy <- deck_table("tbl_importance_hazard_summary.csv")

  # ---- region-geography rows only, ranked descending by share_pct ---------
  shp <- shp[shp$geography == "region", ]
  if (!nrow(shp))
    stop("build_importance_hazard_slide: no geography == 'region' rows in ",
         "tbl_importance_hazard_shapley.csv.", call. = FALSE)
  shp <- shp[order(shp$share_pct), ]                       # ascending: geom_col
  shp$block_label <- factor(shp$block_label, levels = unique(shp$block_label))  # puts smallest bar at
                                                             # the bottom, largest at the top

  smy <- smy[smy$geography == "region", ]
  if (nrow(smy) != 1L)
    stop("build_importance_hazard_slide: expected exactly one geography == 'region' ",
         "row in tbl_importance_hazard_summary.csv, found ", nrow(smy), ".", call. = FALSE)
  r2_pct <- round(100 * smy$r2_mcfadden_full[1])
  n_val  <- format(smy$n[1], big.mark = ",", trim = TRUE)

  ggplot(shp, aes(share_pct, block_label)) +
    geom_col(fill = p$teal, width = 0.68) +
    geom_text(aes(label = sprintf("%.0f%%", share_pct)),
              hjust = -0.25, size = 5, colour = p$ink) +
    scale_x_continuous(limits = c(0, max(shp$share_pct) * 1.18),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = wrap_title("What flags next-year leavers among continuing students"),
      subtitle = wrap_sub(sprintf(
        "The model explains %d%% of the variation in going next year (pseudo-R2), n = %s student-years.",
        r2_pct, n_val)),
      x = "Share of explained variation (%)", y = NULL,
      caption = wrapcap(paste(
        "Shares are order-independent (Shapley decomposition over predictor blocks).",
        "Descriptive, not causal.",
        "Financial confidence and considered-leaving are this year's values and are partly",
        "symptoms of impending exit, not independent predictors of it."))
    ) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank())
}

# ===========================================================================
# build_importance_y1hazard_slide()  -  same ranked-bar design, reading
# tbl_importance_y1hazard_shapley.csv / _summary.csv (first-year students,
# outcome = gone next year). This frame carries the reversal: the entry
# funding answers outrank specific course for first-year exit.
# ===========================================================================
build_importance_y1hazard_slide <- function() {
  p   <- deck_palette()
  shp <- deck_table("tbl_importance_y1hazard_shapley.csv")
  smy <- deck_table("tbl_importance_y1hazard_summary.csv")

  shp <- shp[shp$geography == "region", ]
  if (!nrow(shp))
    stop("build_importance_y1hazard_slide: no geography == 'region' rows in ",
         "tbl_importance_y1hazard_shapley.csv.", call. = FALSE)
  shp <- shp[order(shp$share_pct), ]
  shp$block_label <- factor(shp$block_label, levels = unique(shp$block_label))

  smy <- smy[smy$geography == "region", ]
  if (nrow(smy) != 1L)
    stop("build_importance_y1hazard_slide: expected exactly one geography == 'region' ",
         "row in tbl_importance_y1hazard_summary.csv, found ", nrow(smy), ".", call. = FALSE)
  r2_pct <- round(100 * smy$r2_mcfadden_full[1], 1)
  n_val  <- format(smy$n[1], big.mark = ",", trim = TRUE)

  ggplot(shp, aes(share_pct, block_label)) +
    geom_col(fill = p$teal, width = 0.68) +
    geom_text(aes(label = sprintf("%.0f%%", share_pct)),
              hjust = -0.25, size = 5, colour = p$ink) +
    scale_x_continuous(limits = c(0, max(shp$share_pct) * 1.18),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = wrap_title("In year one, funding dependence outranks course"),
      subtitle = wrap_sub(sprintf(
        paste("What students say at entry about depending on the grant is the biggest",
              "marker of first-year exit. The model explains %s%% of the variation",
              "(pseudo-R2), n = %s first-year students."), r2_pct, n_val)),
      x = "Share of explained variation (%)", y = NULL,
      caption = wrapcap(paste(
        "Shares are order-independent (Shapley decomposition over predictor blocks).",
        "Descriptive, not causal."))
    ) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank())
}

# ===========================================================================
# build_importance_triptych_slide()  -  the cross-frame comparison, one panel
# per frame, SAME block order in every panel so the eye can track a block
# across frames. This is the slide that shows the finding: the funding
# answers' share falls across the course (46 -> 29 -> 10) while specific
# course's share rises (37 -> 47 -> 61). Blocks are harmonised by their
# `block` key, not their per-frame label, so "survey" (entry frame) and
# "funding_entry" (the two hazard frames) become one row. An empty row in a
# panel means that block does not exist in that frame (in-year questions are
# not asked in year 1; components are folded into the wave real value).
# Region geography throughout.
# ===========================================================================
build_importance_triptych_slide <- function() {
  p <- deck_palette()

  frames <- list(
    list(tag = "y1",    shp = "tbl_importance_y1hazard_shapley.csv",
         smy = "tbl_importance_y1hazard_summary.csv",
         label = "Year 1: gone next year"),
    list(tag = "entry", shp = "tbl_importance_shapley.csv",
         smy = "tbl_importance_summary.csv",
         label = "At entry: ever leaves"),
    list(tag = "cont",  shp = "tbl_importance_hazard_shapley.csv",
         smy = "tbl_importance_hazard_summary.csv",
         label = "Years 2+: gone next year")
  )

  # harmonise block keys across frames to one display label each
  key_label <- c(
    course        = "Specific course",
    survey        = "Funding answers (entry)",
    funding_entry = "Funding answers (entry)",
    place         = "Region",
    considered    = "Considered leaving (in-year)",
    components    = "Grant components",
    family        = "Course family",
    cohort        = "Entry cohort",
    survey_year   = "Survey year",
    confidence    = "Financial confidence (in-year)",
    study_year    = "Year of study",
    real_value    = "Real LSF value"
  )

  rows <- list(); subs <- character(0)
  for (f in frames) {
    shp <- deck_table(f$shp); smy <- deck_table(f$smy)
    shp <- shp[shp$geography == "region", ]
    smy <- smy[smy$geography == "region", ]
    if (!nrow(shp) || nrow(smy) != 1L)
      stop("build_importance_triptych_slide: region rows missing in ", f$shp, call. = FALSE)
    miss <- setdiff(unique(shp$block), names(key_label))
    if (length(miss))
      stop("build_importance_triptych_slide: unmapped block key(s): ",
           paste(miss, collapse = ", "), call. = FALSE)
    shp$label_h <- unname(key_label[shp$block])
    shp$frame   <- f$label
    rows[[f$tag]] <- shp[, c("frame", "label_h", "share_pct")]
    subs <- c(subs, sprintf("%s explains %s%%",
                            f$label, round(100 * smy$r2_mcfadden_full[1], 1)))
  }
  d <- do.call(rbind, rows)

  # one canonical row order for every panel: by best share across frames
  ord <- tapply(d$share_pct, d$label_h, max)
  lv  <- names(sort(ord))                       # ascending: biggest ends up top
  d$label_h <- factor(d$label_h, levels = lv)
  d$frame   <- factor(d$frame, levels = vapply(frames, function(f) f$label, character(1)))

  ggplot(d, aes(share_pct, label_h)) +
    geom_col(fill = p$teal, width = 0.68) +
    geom_text(aes(label = sprintf("%.0f%%", share_pct)),
              hjust = -0.2, size = 3.6, colour = p$ink) +
    scale_x_continuous(limits = c(0, max(d$share_pct) * 1.22),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_discrete(drop = FALSE) +
    facet_wrap(~frame, nrow = 1) +
    labs(
      title = wrap_title("The funding signal lives in year one; course takes over later"),
      subtitle = wrap_sub(paste0(
        "Share of explained variation in leaving, by predictor block. ",
        paste(subs, collapse = "; "), " (pseudo-R2).")),
      x = "Share of explained variation (%)", y = NULL,
      caption = wrapcap(paste(
        "Shares are order-independent (Shapley decomposition over predictor blocks); region geography.",
        "An empty row means the block does not exist in that frame (in-year questions are not",
        "asked in year 1; grant components are folded into the wave real value).",
        "Descriptive, not causal."))
    ) +
    theme_dhsc_slide(13) +
    theme(panel.grid.major.y = element_blank())
}

# ===========================================================================
# PUBLICATION TABLES - the decomposition as three branded tables, rendered
# through dhsc_table_plot() (functions/deck_helpers.r), the repo's pure-
# ggplot table renderer. Same region-geography numbers as the bar slides.
#
#   build_importance_table_overall()  how much of leaving is explainable at
#                                     all, per frame (n, leave rate, pseudo-
#                                     R2, AUC)
#   build_importance_table_shares()   share of the explained variation taken
#                                     by each factor, three frame columns
#   build_importance_table_models()   test-set AUC, logit vs best flexible
#
# Each returns a ggplot; save with save_slide() like any other builder. Not
# registered in the manifest (feat/deck-text is renumbering the deck).
# ===========================================================================

# frame spec shared by the three tables: tag, csv pair, display label
importance_frames_spec <- function() list(
  list(tag = "y1",    shp = "tbl_importance_y1hazard_shapley.csv",
       smy = "tbl_importance_y1hazard_summary.csv",
       pred = "y1_next",    label = "First year -> gone next year"),
  list(tag = "entry", shp = "tbl_importance_shapley.csv",
       smy = "tbl_importance_summary.csv",
       pred = "entry_ever", label = "At entry -> leaves before finishing"),
  list(tag = "cont",  shp = "tbl_importance_hazard_shapley.csv",
       smy = "tbl_importance_hazard_summary.csv",
       pred = "cont_next",  label = "Years 2+ -> gone next year")
)

build_importance_table_overall <- function() {
  pred <- deck_table("tbl_leaver_prediction.csv")
  rows <- lapply(importance_frames_spec(), function(f) {
    smy <- deck_table(f$smy); smy <- smy[smy$geography == "region", ]
    br  <- pred$base_rate[pred$frame == f$pred & pred$model == "logit_main"]
    tibble::tibble(
      Frame              = f$label,
      n                  = format(smy$n[1], big.mark = ","),
      `Leave rate`       = if (length(br) == 1 && is.finite(br))
                             sprintf("%.0f%%", 100 * br) else "-",
      `Explained (pseudo-R2)` = sprintf("%.1f%%", 100 * smy$r2_mcfadden_full[1]),
      AUC                = sprintf("%.2f", smy$auc_full[1])
    )
  })
  dhsc_table_plot(
    dplyr::bind_rows(rows),
    title    = "How much of leaving can be explained at all",
    subtitle = "Course, place, cohort, the real grant value and the survey answers together",
    caption  = paste("Region geography. Leave rates from the matching prediction-benchmark",
                     "samples. Most of the variation in leaving is not explained by anything measured."))
}

build_importance_table_shares <- function() {
  key_label <- c(course = "Specific course",
                 survey = "Funding-dependence answers (at entry)",
                 funding_entry = "Funding-dependence answers (at entry)",
                 place = "Region", considered = "Considered leaving (in-year)",
                 family = "Course family", components = "Grant components",
                 survey_year = "Survey year", cohort = "Entry cohort",
                 real_value = "Real value of the grant",
                 confidence = "Financial confidence (in-year)",
                 study_year = "Year of study")

  frames <- importance_frames_spec()
  got <- lapply(frames, function(f) {
    shp <- deck_table(f$shp); shp <- shp[shp$geography == "region", ]
    stats::setNames(shp$share_pct, unname(key_label[shp$block]))
  })
  labels_all <- unique(unname(key_label))
  best <- vapply(labels_all, function(l)
    max(unlist(lapply(got, function(g) g[l])), na.rm = TRUE), numeric(1))
  labels_all <- labels_all[order(-best)]

  fmt <- function(g, l) {
    v <- g[l]
    if (is.na(v)) "-" else if (v < 1) sprintf("%.1f%%", v) else sprintf("%.0f%%", v)
  }
  df <- tibble::tibble(
    Factor = labels_all,
    `First year -> next yr` = vapply(labels_all, function(l) fmt(got[[1]], l), character(1)),
    `At entry -> ever`      = vapply(labels_all, function(l) fmt(got[[2]], l), character(1)),
    `Years 2+ -> next yr`   = vapply(labels_all, function(l) fmt(got[[3]], l), character(1))
  )
  dhsc_table_plot(
    df,
    title    = "Of what is explained, the share taken by each factor",
    subtitle = "The funding signal lives in year one; course takes over later",
    caption  = paste("Shapley decomposition, region geography; shares sum to 100% within each column.",
                     "A dash means the factor does not exist in that frame. Descriptive, not causal."))
}

build_importance_table_models <- function() {
  pred <- deck_table("tbl_leaver_prediction.csv")
  rows <- lapply(importance_frames_spec(), function(f) {
    sub <- pred[pred$frame == f$pred, ]
    lg  <- sub$auc[sub$model == "logit_main"]
    fx  <- suppressWarnings(max(sub$auc[sub$model %in% c("tree", "forest")], na.rm = TRUE))
    tibble::tibble(
      Frame               = f$label,
      `Logit AUC`         = sprintf("%.3f", lg),
      `Best flexible AUC` = if (is.finite(fx)) sprintf("%.3f", fx) else "-",
      Gain                = if (is.finite(fx)) sprintf("%+.3f", fx - lg) else "-"
    )
  })
  dhsc_table_plot(
    dplyr::bind_rows(rows),
    title    = "A more flexible model does not do better",
    subtitle = "Held-out AUC, split by student: the ceiling is the data, not the model",
    caption  = paste("Grouped 70/30 train/test split. randomForest is not installed on the",
                     "secure machine, so the flexible comparator is a pruned regression tree."))
}

# ===========================================================================
# save_importance_econ_table()  -  the decomposition as ONE journal-style
# table (booktabs rules, serif, no fills): a shares panel over a summary-
# statistics panel, in the format of an econometrics journal rather than a
# slide. Draws with plain geom_text and three horizontal rules; sizes and
# saves itself (do NOT pass this through save_slide, which forces 16:9).
#
#   save_importance_econ_table()                        -> deck_dir()/table_importance.png
#   save_importance_econ_table("C:/somewhere/tbl.png")  -> that path
#
# Sources: the three *_shapley/*_summary CSV pairs plus tbl_leaver_
# prediction.csv (leave rates). Region geography.
# ===========================================================================
save_importance_econ_table <- function(file = file.path(deck_dir(), "table_importance.png"),
                                       dpi = 300) {
  fam <- "serif"; ink <- "#111111"; grey <- "#444444"

  key_label <- c(course = "Specific course",
                 survey = "Funding-dependence answers (entry)",
                 funding_entry = "Funding-dependence answers (entry)",
                 place = "Region", considered = "Considered leaving (in-year)",
                 family = "Course family", components = "Grant components",
                 survey_year = "Survey year", cohort = "Entry cohort",
                 real_value = "Real value of the grant",
                 confidence = "Financial confidence (in-year)",
                 study_year = "Year of study")

  frames <- importance_frames_spec()          # y1, entry, cont (defined above)
  pred   <- deck_table("tbl_leaver_prediction.csv")

  shares <- list(); stats <- list()
  for (i in seq_along(frames)) {
    f   <- frames[[i]]
    shp <- deck_table(f$shp); shp <- shp[shp$geography == "region", ]
    smy <- deck_table(f$smy); smy <- smy[smy$geography == "region", ]
    br  <- pred$base_rate[pred$frame == f$pred & pred$model == "logit_main"]
    shares[[i]] <- stats::setNames(shp$share_pct, unname(key_label[shp$block]))
    stats[[i]] <- c(
      `Observations`          = format(smy$n[1], big.mark = ","),
      `Share who leave`       = if (length(br) == 1 && is.finite(br))
                                  sprintf("%.3f", br) else "-",
      `McFadden pseudo-R2`    = sprintf("%.3f", smy$r2_mcfadden_full[1]),
      `Tjur R2`               = sprintf("%.3f", smy$r2_tjur_full[1]),
      `AUC`                   = sprintf("%.3f", smy$auc_full[1]),
      `Predictor blocks (k)`  = as.character(smy$k_blocks[1]),
      `Model fits`            = as.character(3L * 2L^(smy$k_blocks[1] - 1L))
    )
  }

  labels_all <- unique(unname(key_label))
  best <- vapply(labels_all, function(l)
    max(unlist(lapply(shares, function(g) g[l])), na.rm = TRUE), numeric(1))
  labels_all <- labels_all[order(-best)]
  fmt_share <- function(g, l) { v <- g[l]; if (is.na(v)) "-" else sprintf("%.1f", v) }

  # ---- cell grid -------------------------------------------------------------
  x_lab <- 0; x_col <- c(6.0, 8.0, 10.0)   # left edge of label col; right edges of the 3 frame cols
  head1 <- c("Gone next year,", "Leaves before", "Gone next year,")
  head2 <- c("first years", "finishing, at entry", "years 2+")
  head3 <- c("(1)", "(2)", "(3)")

  rows <- list(); y <- 0
  add <- function(lab, vals, face = "plain", gap = 1) {
    y <<- y - gap
    rows[[length(rows) + 1L]] <<- data.frame(
      x = c(x_lab, x_col), y = y, hj = c(0, 1, 1, 1),
      txt = c(lab, vals), face = face, stringsAsFactors = FALSE)
  }
  add("", head1); add("", head2, gap = 0.85); add("", head3, gap = 0.9)
  y_rule_head <- y - 0.55
  add("Share of explained variation (%)", c("", "", ""), face = "italic", gap = 1.35)
  for (l in labels_all)
    add(l, vapply(shares, fmt_share, character(1), l = l))
  y_rule_mid <- y - 0.55
  first_stat <- TRUE
  for (s in names(stats[[1]])) {
    add(s, vapply(stats, function(v) v[[s]], character(1)),
        gap = if (first_stat) 1.35 else 1)
    first_stat <- FALSE
  }
  cells <- do.call(rbind, rows)
  y_top <- 0.75; y_bot <- y - 0.6

  note <- paste(
    "Notes: each column decomposes the McFadden pseudo-R2 of a logistic regression (fixest::feglm;",
    "course, cohort or year, and region entered as absorbed fixed-effect blocks) into order-independent",
    "Shapley (LMG) values over predictor blocks; shares sum to 100 within a column. One fixed estimation",
    "sample per column (complete cases; fixed-effect levels with a constant outcome removed). Region",
    "geography; a travel-to-work-area sensitivity is in the underlying tables. A dash means the block is",
    "not defined in that frame: in-year questions are not asked in year one, and grant top-ups are inside",
    "the year-specific real value. Share who leave is taken from the matching prediction samples.",
    "Associational, not causal.")

  g <- ggplot2::ggplot() +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_top, yend = y_top,
                      linewidth = 0.8, colour = ink) +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_rule_head, yend = y_rule_head,
                      linewidth = 0.35, colour = ink) +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_rule_mid, yend = y_rule_mid,
                      linewidth = 0.35, colour = ink) +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_bot, yend = y_bot,
                      linewidth = 0.8, colour = ink) +
    ggplot2::geom_text(data = cells,
                       ggplot2::aes(x = x, y = y, label = txt, hjust = hj, fontface = face),
                       size = 3.4, family = fam, colour = ink) +
    ggplot2::scale_x_continuous(limits = c(x_lab - 0.1, max(x_col) + 0.1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(y_bot - 0.4, 3.4), expand = c(0, 0)) +
    ggplot2::annotate("text", x = x_lab, y = 2.6, hjust = 0, family = fam, size = 4.1,
                      fontface = "bold", colour = ink,
                      label = "Table 1. What explains leaving: relative importance by frame") +
    ggplot2::annotate("text", x = x_lab, y = 1.7, hjust = 0, family = fam, size = 3.3,
                      colour = grey,
                      label = "Shapley decomposition of explained variation in leaving, NHSBSA LSF survey 2020-2026") +
    ggplot2::labs(caption = stringr::str_wrap(note, 128)) +
    ggplot2::theme_void(base_family = fam) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 7.6, colour = grey, hjust = 0,
                                           family = fam, lineheight = 1.15,
                                           margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(18, 22, 14, 22),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  n_rows <- abs(y_bot) + 4
  ggplot2::ggsave(file, g, width = 8.3, height = 0.235 * n_rows + 1.1,
                  dpi = dpi, bg = "white")
  message("wrote ", file)
  invisible(file)
}

# ===========================================================================
# save_prediction_econ_table()  -  the model benchmark in the same journal
# format as save_importance_econ_table(): can a more flexible model tell
# leavers from stayers better out of sample? Columns are the three frames;
# rows are held-out AUC per model class, then the sample panel. Sizes and
# saves itself (not for save_slide).
# ===========================================================================
save_prediction_econ_table <- function(file = file.path(deck_dir(), "table_prediction.png"),
                                       dpi = 300) {
  fam <- "serif"; ink <- "#111111"; grey <- "#444444"

  frames <- importance_frames_spec()
  pred   <- deck_table("tbl_leaver_prediction.csv")
  gv <- function(fr, m, col) {
    v <- pred[[col]][pred$frame == fr & pred$model == m]
    if (length(v) == 1 && is.finite(v)) v else NA_real_
  }
  f3 <- function(x) if (is.na(x)) "-" else sprintf("%.3f", x)

  auc_l <- vapply(frames, function(f) gv(f$pred, "logit_main", "auc"), numeric(1))
  auc_t <- vapply(frames, function(f) gv(f$pred, "tree",       "auc"), numeric(1))
  auc_f <- vapply(frames, function(f) gv(f$pred, "forest",     "auc"), numeric(1))
  gain  <- pmax(auc_t, auc_f, na.rm = TRUE) - auc_l
  n_tr  <- vapply(frames, function(f) gv(f$pred, "logit_main", "n_train"), numeric(1))
  n_te  <- vapply(frames, function(f) gv(f$pred, "logit_main", "n_test"),  numeric(1))
  br    <- vapply(frames, function(f) gv(f$pred, "logit_main", "base_rate"), numeric(1))

  x_lab <- 0; x_col <- c(6.0, 8.0, 10.0)
  head1 <- c("Gone next year,", "Leaves before", "Gone next year,")
  head2 <- c("first years", "finishing, at entry", "years 2+")
  head3 <- c("(1)", "(2)", "(3)")

  rows <- list(); y <- 0
  add <- function(lab, vals, face = "plain", gap = 1) {
    y <<- y - gap
    rows[[length(rows) + 1L]] <<- data.frame(
      x = c(x_lab, x_col), y = y, hj = c(0, 1, 1, 1),
      txt = c(lab, vals), face = face, stringsAsFactors = FALSE)
  }
  add("", head1); add("", head2, gap = 0.85); add("", head3, gap = 0.9)
  y_rule_head <- y - 0.55
  add("Held-out AUC", c("", "", ""), face = "italic", gap = 1.35)
  add("Logistic regression (main effects)", vapply(auc_l, f3, character(1)))
  add("Regression tree (pruned)",           vapply(auc_t, f3, character(1)))
  add("Random forest",                      vapply(auc_f, f3, character(1)))
  add("Best flexible minus logistic",
      vapply(gain, function(x) if (is.na(x)) "-" else sprintf("%+.3f", x), character(1)))
  y_rule_mid <- y - 0.55
  add("Training observations", formatC(n_tr, format = "d", big.mark = ","), gap = 1.35)
  add("Test observations",     formatC(n_te, format = "d", big.mark = ","))
  add("Share who leave (test)", vapply(br, f3, character(1)))
  cells <- do.call(rbind, rows)
  y_top <- 0.75; y_bot <- y - 0.6

  note <- paste(
    "Notes: AUC on held-out data under a grouped 70/30 train/test split (a student's rows are wholly in",
    "one split). Predictors per column match the corresponding decomposition; the tree and forest replace",
    "the course factor with course family plus a course leave-rate encoding fitted on the training split",
    "only. The tree is a pruned anova (regression) tree, 1-SE rule. randomForest is not installed on the",
    "secure environment, so those cells are blank. Flexible models do not improve on the additive logistic",
    "regression in any frame: the low predictability of leaving is a property of the data, not the model",
    "class. Associational, not causal.")

  g <- ggplot2::ggplot() +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_top, yend = y_top,
                      linewidth = 0.8, colour = ink) +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_rule_head, yend = y_rule_head,
                      linewidth = 0.35, colour = ink) +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_rule_mid, yend = y_rule_mid,
                      linewidth = 0.35, colour = ink) +
    ggplot2::annotate("segment", x = x_lab, xend = max(x_col), y = y_bot, yend = y_bot,
                      linewidth = 0.8, colour = ink) +
    ggplot2::geom_text(data = cells,
                       ggplot2::aes(x = x, y = y, label = txt, hjust = hj, fontface = face),
                       size = 3.4, family = fam, colour = ink) +
    ggplot2::scale_x_continuous(limits = c(x_lab - 0.1, max(x_col) + 0.1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(y_bot - 0.4, 3.4), expand = c(0, 0)) +
    ggplot2::annotate("text", x = x_lab, y = 2.6, hjust = 0, family = fam, size = 4.1,
                      fontface = "bold", colour = ink,
                      label = "Table 2. Does a more flexible model predict leaving better?") +
    ggplot2::annotate("text", x = x_lab, y = 1.7, hjust = 0, family = fam, size = 3.3,
                      colour = grey,
                      label = "Out-of-sample discrimination by model class, NHSBSA LSF survey 2020-2026") +
    ggplot2::labs(caption = stringr::str_wrap(note, 128)) +
    ggplot2::theme_void(base_family = fam) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 7.6, colour = grey, hjust = 0,
                                           family = fam, lineheight = 1.15,
                                           margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(18, 22, 14, 22),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  n_rows <- abs(y_bot) + 4
  ggplot2::ggsave(file, g, width = 8.3, height = 0.235 * n_rows + 1.15,
                  dpi = dpi, bg = "white")
  message("wrote ", file)
  invisible(file)
}
