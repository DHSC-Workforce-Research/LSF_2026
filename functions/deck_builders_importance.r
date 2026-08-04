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
# STANDALONE, DELIBERATELY NOT wired into functions/deck_manifest.r:
# feat/deck-text is renumbering the deck in parallel and this slide has no
# fixed slot yet. It is still sourced automatically, because
# scripts/00_config.r walks every file under functions/ with source(), so
# wrap_title() / wrap_sub() / wrapcap() / deck_palette() (functions/deck_
# builders.r), theme_dhsc_slide() (functions/deck_helpers.r) and dhsc_cols
# (functions/dhsc_theme.r) are all in scope by the time build_importance_
# slide() is actually called. This file never saves a plot or picks a slide
# number, same contract as every builder in deck_builders.r; the caller does
# both.
#
# Locating the source tables: tbl_importance_shapley.csv and tbl_importance_
# summary.csv are found with deck_table() (functions/deck_helpers.r), which
# recursively searches under outputs_dir() and derived_dir() for the newest
# file matching the name and does not assume a folder - the same rule
# tests/check_numbers.r's find_newest() uses. Run 02_analysis.r first so
# those two tables exist somewhere under outputs_dir().
#
# Usage (after 02_analysis.r has run):
#
#   source("scripts/00_config.r")
#   suppressMessages({ library(dplyr); library(readr); library(stringr)
#                      library(ggplot2); library(tibble) })
#   p <- build_importance_slide()
#   save_slide(p, file.path(deck_dir(), "slide_XX_importance.png"))
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
