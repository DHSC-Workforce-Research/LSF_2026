# ===========================================================================
# scripts/d3_visualise_demographics.r
# Sample demographics (BSA cross-tabs) -> DHSC widescreen deck slides.
#
# Separate from d2_analysis.r (which writes analysis-size exploratory PNGs to
# derived/). This script only builds presentation slides for PowerPoint:
#   - 16:9 via save_slide()  (13.33 x 7.5 in)
#   - theme_dhsc_slide()     (screen-readable type)
#   - bold title = finding; grey subtitle = technical definition
#   - title/subtitle are wrapped so they do not clip at the slide edge
#   - Ethnicity is its own slide (too many groups for a multi-facet panel)
#
# Prerequisites (work machine, secure data present):
#   1. source("scripts/d1_parse_sample_demographics_data.r")
#   2. source("scripts/d2_analysis.r")   # writes financial_confidence_by_band.csv
#   3. source("scripts/d3_visualise_demographics.r")
#
# Outputs (outputs_dir(), NW025):
#   slide_confidence_unconfident.png           - main map (excl. ethnicity)
#   slide_confidence_unconfident_ethnicity.png - ethnicity only (full height)
#   slide_confidence_hardship_rank.png         - top groups above average
#   slide_confidence_confident.png             - mirror: share rating 4-5
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
})

out <- outputs_dir()
src <- paste(
  "Source: NHS Learning Support Fund sample demographics (BSA questionnaire",
  "analysis cross-tabs), DHSC analysis. Shares are within-group; not a model",
  "of leaving. Financial confidence is asked of continuing (year 2+) students."
)
# Widescreen text wrap widths (characters). Titles are short; subtitles longer.
wrap_title <- function(x, w = 72) str_wrap(x, width = w)
wrap_sub   <- function(x, w = 118) str_wrap(x, width = w)
wrapcap    <- function(x, w = 128) str_wrap(x, width = w)
ink  <- dcol("ink", "#0B0C0C")
grey <- dcol("midgrey", "#6F777B")

# Extra theme so multi-line titles/subtitles never clip at the edge ----------
slide_text_theme <- function(title_size = 20, sub_size = 12.5) {
  theme(
    plot.title = element_text(
      size = title_size, face = "bold", colour = ink,
      lineheight = 1.12, margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = sub_size, colour = "grey30",
      lineheight = 1.18, margin = margin(b = 10)
    ),
    plot.caption = element_text(lineheight = 1.15),
    # room for 2-line title + 2-3 line sub without eating the chart
    plot.margin = margin(14, 22, 10, 14)
  )
}

# --- data: prefer d2 banded table; rebuild from long if needed ------------
band_path <- file.path(derived_dir(), "financial_confidence_by_band.csv")
long_path <- file.path(derived_dir(), "financial_confidence_long.csv")

if (file.exists(band_path)) {
  banded <- read_csv(band_path, show_col_types = FALSE)
  if (!"is_total" %in% names(banded)) {
    banded$is_total <- grepl("grand total|^total$", tolower(banded$Group))
  }
  if (!"Net_pct" %in% names(banded)) banded <- add_net(banded)
  progress("loaded banded table: ", band_path)
} else if (file.exists(long_path)) {
  progress("banded table missing; rebuilding from long")
  long   <- tidy_groups(read_csv(long_path, show_col_types = FALSE))
  banded <- add_net(confidence_by_group(long))
} else {
  stop(
    "No demography inputs found under derived_dir(). ",
    "Run d1 then d2 first.\n  expected: ", band_path, "\n  or: ", long_path
  )
}

d0 <- banded[!isTRUE(banded$is_total) & !grepl("grand total|^total$", tolower(banded$Group)), ]

# Ethnicity is dense (~15-20 groups). Keep it off multi-facet slides.
is_ethnicity <- grepl("^ethnicity$", d0$Demographic, ignore.case = TRUE)
d_main <- d0[!is_ethnicity, , drop = FALSE]
d_eth  <- d0[is_ethnicity, , drop = FALSE]
if (!nrow(d_eth)) {
  # fallback if label is still the long survey wording
  is_ethnicity <- grepl("ethnic", d0$Demographic, ignore.case = TRUE)
  d_main <- d0[!is_ethnicity, , drop = FALSE]
  d_eth  <- d0[is_ethnicity, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Slide-grade bar chart (same logic as plot_confidence_bar, deck styling).
# ---------------------------------------------------------------------------
plot_confidence_slide <- function(banded_df, band = "Unconfident",
                                  title, subtitle,
                                  demogs = NULL, ncol = 3, k = 1,
                                  show_values = TRUE, value_size = 3.2,
                                  title_size = 20, sub_size = 12.5,
                                  strip_size = 12, axis_y_size = 11,
                                  single_panel = FALSE) {
  pct_col <- paste0(band, "_pct")
  stopifnot(pct_col %in% names(banded_df))

  d <- banded_df
  if (!is.null(demogs)) d <- d[d$Demographic %in% demogs, , drop = FALSE]
  if (!nrow(d)) stop("No rows left after demographic filter for band=", band)

  d$value <- d[[pct_col]]

  ref <- stats::weighted.mean(d$value, d$n)
  s   <- sqrt(stats::weighted.mean((d$value - ref)^2, d$n))
  z   <- if (s > 0) (d$value - ref) / s else rep(0, nrow(d))

  higher_is_bad <- band %in% c("Unconfident", "Neutral")
  d$status <- ifelse(abs(z) <= k, "Typical",
              ifelse((z > 0) == higher_is_bad, "More worried than average",
                                               "Less worried than average"))
  d$status <- factor(d$status,
                     levels = c("Less worried than average",
                                "Typical",
                                "More worried than average"))

  if (single_panel) {
    # One demographic only: use full slide height, sort by value
    d$key <- factor(d$Group, levels = d$Group[order(d$value)])
  } else {
    d <- d |>
      group_by(Demographic) |>
      mutate(key = factor(Group, levels = Group[order(value)])) |>
      ungroup()
  }

  cols <- c(
    "Less worried than average" = "#01A188",
    "Typical"                   = "#B1B4B6",
    "More worried than average" = "#D4351C"
  )
  opts <- switch(band, Unconfident = "1-2", Confident = "4-5", "3")

  p <- ggplot(d, aes(x = value, y = key, fill = status)) +
    geom_col(width = 0.72) +
    geom_vline(xintercept = ref, linetype = "dashed",
               colour = grey, linewidth = 0.45) +
    scale_fill_manual(values = cols, drop = FALSE) +
    labs(
      title    = wrap_title(title),
      subtitle = wrap_sub(subtitle),
      x = sprintf("Share %s (rated %s)", tolower(band), opts),
      y = NULL,
      fill = NULL,
      caption = wrapcap(src)
    ) +
    theme_dhsc_slide(base = 14) +
    slide_text_theme(title_size = title_size, sub_size = sub_size) +
    theme(
      legend.position    = "top",
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(size = axis_y_size)
    ) +
    expand_limits(x = max(d$value, na.rm = TRUE) * 1.14)

  if (!single_panel) {
    p <- p +
      facet_wrap(~ Demographic, scales = "free_y", ncol = ncol) +
      theme(strip.text = element_text(face = "bold", hjust = 0, size = strip_size))
  }

  if (show_values) {
    p <- p + geom_text(aes(label = sprintf("%.0f%%", value)),
                       hjust = -0.12, size = value_size, colour = ink)
  }
  p
}

# Ranked callout: groups furthest above the survey average on low confidence
plot_hardship_rank_slide <- function(banded_df, title, subtitle,
                                     top_n = 12, k = 1) {
  d <- banded_df
  d$value <- d$Unconfident_pct
  ref <- stats::weighted.mean(d$value, d$n)
  d$gap <- d$value - ref
  d <- d[order(-d$gap), , drop = FALSE]
  d <- utils::head(d, top_n)
  d$label <- paste0(d$Group, "  (", d$Demographic, ")")
  d$label <- factor(d$label, levels = rev(d$label))

  s   <- sqrt(stats::weighted.mean((banded_df$Unconfident_pct - ref)^2, banded_df$n))
  z   <- if (s > 0) (d$value - ref) / s else rep(0, nrow(d))
  d$status <- ifelse(abs(z) <= k, "Typical", "More worried than average")
  d$status <- factor(d$status,
                     levels = c("Typical", "More worried than average"))

  cols <- c("Typical" = "#B1B4B6", "More worried than average" = "#D4351C")

  ggplot(d, aes(x = value, y = label, fill = status)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = ref, linetype = "dashed",
               colour = grey, linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%  (+%.0fpp)", value, gap)),
              hjust = -0.08, size = 4.0, colour = ink) +
    annotate("text", x = ref, y = Inf, vjust = 1.4, hjust = -0.05,
             label = sprintf("Survey average  %.0f%%", ref),
             colour = grey, size = 4.0) +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_x_continuous(limits = c(0, max(d$value) * 1.22),
                       labels = function(z) paste0(z, "%")) +
    labs(
      title    = wrap_title(title),
      subtitle = wrap_sub(subtitle),
      x = "Share unconfident about covering living expenses next year (rated 1-2)",
      y = NULL,
      fill = NULL,
      caption = wrapcap(src)
    ) +
    theme_dhsc_slide(base = 15) +
    slide_text_theme(title_size = 20, sub_size = 13) +
    theme(
      legend.position    = "top",
      panel.grid.major.y = element_blank()
    )
}

# --- titles: finding (title) + technical definition (subtitle) ------------
# Keep titles short enough to wrap cleanly on 16:9; put detail in the sub.
ref_u_main <- stats::weighted.mean(d_main$Unconfident_pct, d_main$n)
ref_u_all  <- stats::weighted.mean(d0$Unconfident_pct, d0$n)
ref_u_eth  <- if (nrow(d_eth)) stats::weighted.mean(d_eth$Unconfident_pct, d_eth$n) else ref_u_all
ref_c_main <- stats::weighted.mean(d_main$Confident_pct, d_main$n)

title_u <- "Financial confidence is uneven across claimant groups"
sub_u <- paste0(
  "Share rating 1-2 on covering living expenses next year. ",
  "Dashed line = n-weighted average of groups on this slide (",
  sprintf("%.0f%%", ref_u_main),
  "). Colour: more than 1 SD from that average (red = more worried, teal = less). ",
  "Ethnicity is on the next slide (too many categories to fit here)."
)

title_eth <- "Low financial confidence varies sharply by ethnicity"
sub_eth <- paste0(
  "Share rating 1-2 on covering living expenses next year, ethnicity only. ",
  "Dashed line = n-weighted average across ethnicity groups on this slide (",
  sprintf("%.0f%%", ref_u_eth),
  "). Colour: more than 1 SD from that average (red = more worried, teal = less)."
)

title_rank <- "These groups report the highest rates of low financial confidence"
sub_rank <- paste0(
  "Top groups by percentage points above the survey average on low confidence (ratings 1-2). ",
  "Average across all demography groups = ", sprintf("%.0f%%", ref_u_all),
  ". This is a hardship marker, not a model of who leaves the course."
)

title_c <- "High financial confidence is also uneven across groups"
sub_c <- paste0(
  "Share rating 4-5 on covering living expenses next year. ",
  "Dashed line = n-weighted average of groups on this slide (",
  sprintf("%.0f%%", ref_c_main),
  "). Colour: more than 1 SD from that average (teal = more confident). ",
  "Ethnicity omitted here for space; see the ethnicity low-confidence slide."
)

# --- build & save ---------------------------------------------------------
# Slide 1 (hero): unconfident map without ethnicity — readable multi-facet
p1 <- plot_confidence_slide(
  d_main, band = "Unconfident",
  title = title_u, subtitle = sub_u,
  ncol = 3, value_size = 3.2,
  title_size = 20, sub_size = 12.5,
  strip_size = 12, axis_y_size = 11
)
save_slide(p1, file.path(out, "slide_confidence_unconfident.png"))

# Slide 1b: ethnicity alone, full height, larger y labels
if (nrow(d_eth)) {
  p1b <- plot_confidence_slide(
    d_eth, band = "Unconfident",
    title = title_eth, subtitle = sub_eth,
    single_panel = TRUE,
    value_size = 3.6,
    title_size = 20, sub_size = 12.5,
    axis_y_size = 12
  )
  save_slide(p1b, file.path(out, "slide_confidence_unconfident_ethnicity.png"))
} else {
  progress("NOTE: no Ethnicity rows found; skipped ethnicity slide")
}

# Slide 2: ranked hardship callout
p2 <- plot_hardship_rank_slide(
  d0,
  title = title_rank, subtitle = sub_rank,
  top_n = 12
)
save_slide(p2, file.path(out, "slide_confidence_hardship_rank.png"))

# Slide 3: confident mirror (main demogs only — same space rule as slide 1)
p3 <- plot_confidence_slide(
  d_main, band = "Confident",
  title = title_c, subtitle = sub_c,
  ncol = 3, value_size = 3.2,
  title_size = 20, sub_size = 12.5
)
save_slide(p3, file.path(out, "slide_confidence_confident.png"))

progress("done. demography slides written to ", out)
progress("  slide_confidence_unconfident.png")
if (nrow(d_eth)) progress("  slide_confidence_unconfident_ethnicity.png")
progress("  slide_confidence_hardship_rank.png")
progress("  slide_confidence_confident.png")
