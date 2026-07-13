# ===========================================================================
# scripts/d3_visualise_demographics.r
# Sample demographics (BSA cross-tabs) -> DHSC widescreen deck slides.
#
# Separate from d2_analysis.r (which writes analysis-size exploratory PNGs to
# derived/). This script only builds presentation slides for PowerPoint:
#   - 16:9 via save_slide()  (13.33 x 7.5 in)
#   - theme_dhsc_slide()     (screen-readable type)
#   - bold title = finding; grey subtitle = technical definition
#
# Prerequisites (work machine, secure data present):
#   1. source("scripts/d1_parse_sample_demographics_data.r")
#   2. source("scripts/d2_analysis.r")   # writes financial_confidence_by_band.csv
#   3. source("scripts/d3_visualise_demographics.r")
#
# Outputs (outputs_dir(), NW025):
#   slide_confidence_unconfident.png  - hardship signal, all demographics
#   slide_confidence_hardship_rank.png - groups furthest above average (low conf)
#   slide_confidence_confident.png    - mirror: share rating 4-5
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
wrapcap <- function(x, w = 128) str_wrap(x, w)
ink  <- dcol("ink", "#0B0C0C")
grey <- dcol("midgrey", "#6F777B")

# --- data: prefer d2 banded table; rebuild from long if needed ------------
band_path <- file.path(derived_dir(), "financial_confidence_by_band.csv")
long_path <- file.path(derived_dir(), "financial_confidence_long.csv")

if (file.exists(band_path)) {
  banded <- read_csv(band_path, show_col_types = FALSE)
  # d2 may have written without is_total; rebuild flag if missing
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

# Drop total rows for plotting; keep n for weighted average
d0 <- banded[!isTRUE(banded$is_total) & !grepl("grand total|^total$", tolower(banded$Group)), ]

# ---------------------------------------------------------------------------
# Slide-grade bar chart (same logic as plot_confidence_bar, deck styling).
# title   = finding (bold)
# subtitle= technical definition (grey)
# demogs  = optional character vector of Demographic values to keep
# ---------------------------------------------------------------------------
plot_confidence_slide <- function(banded_df, band = "Unconfident",
                                  title, subtitle,
                                  demogs = NULL, ncol = 3, k = 1,
                                  show_values = TRUE, value_size = 3.2) {
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

  # Order within each facet: lowest to highest (worst first for Unconfident)
  d <- d |>
    group_by(Demographic) |>
    mutate(key = factor(Group, levels = Group[order(value)])) |>
    ungroup()

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
    facet_wrap(~ Demographic, scales = "free_y", ncol = ncol) +
    labs(
      title    = title,
      subtitle = subtitle,
      x = sprintf("Share %s (rated %s)", tolower(band), opts),
      y = NULL,
      fill = NULL,
      caption = wrapcap(src)
    ) +
    theme_dhsc_slide(base = 14) +
    theme(
      legend.position    = "top",
      panel.grid.major.y = element_blank(),
      strip.text         = element_text(face = "bold", hjust = 0, size = 12),
      axis.text.y        = element_text(size = 11),
      plot.subtitle      = element_text(size = 13, colour = "grey30",
                                        margin = margin(b = 10)),
      plot.caption       = element_text(lineheight = 1.15)
    ) +
    expand_limits(x = max(d$value, na.rm = TRUE) * 1.12)

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
      title    = title,
      subtitle = subtitle,
      x = "Share unconfident about covering living expenses next year (rated 1-2)",
      y = NULL,
      fill = NULL,
      caption = wrapcap(src)
    ) +
    theme_dhsc_slide(base = 15) +
    theme(
      legend.position    = "top",
      panel.grid.major.y = element_blank(),
      plot.subtitle      = element_text(size = 13.5, colour = "grey30",
                                        margin = margin(b = 10)),
      plot.caption       = element_text(lineheight = 1.15)
    )
}

# --- titles: finding (title) + technical definition (subtitle) ------------
# Subtitles inject the live survey average once the data are loaded.
ref_u <- stats::weighted.mean(d0$Unconfident_pct, d0$n)
ref_c <- stats::weighted.mean(d0$Confident_pct,   d0$n)

title_u <- "Financial confidence is uneven: some claimant groups sit well above average on low confidence"
sub_u <- paste0(
  "Share rating 1-2 on \"How confident are you about being able to cover living expenses in the next year?\". ",
  "Dashed line = n-weighted survey average across plotted groups (", sprintf("%.0f%%", ref_u), "). ",
  "Colour: more than 1 SD from that average (red = more worried, teal = less)."
)

title_rank <- "These groups report the highest rates of low financial confidence"
sub_rank <- paste0(
  "Top groups by percentage points above the survey average on low confidence (ratings 1-2). ",
  "Average across all plotted demography groups = ", sprintf("%.0f%%", ref_u), ". ",
  "This is a hardship marker, not a model of who leaves the course."
)

title_c <- "The mirror: high financial confidence is also uneven across groups"
sub_c <- paste0(
  "Share rating 4-5 on the same living-expenses confidence question. ",
  "Dashed line = n-weighted survey average (", sprintf("%.0f%%", ref_c), "). ",
  "Colour: more than 1 SD from that average (teal = less worried / more confident)."
)

# --- build & save ---------------------------------------------------------
# Slide 1: full unconfident map (all demographics). ncol=3 fits 16:9.
p1 <- plot_confidence_slide(
  d0, band = "Unconfident",
  title = title_u, subtitle = sub_u,
  ncol = 3, value_size = 3.0
)
save_slide(p1, file.path(out, "slide_confidence_unconfident.png"))

# Slide 2: ranked hardship callout (readable at a glance in a meeting)
p2 <- plot_hardship_rank_slide(
  d0,
  title = title_rank, subtitle = sub_rank,
  top_n = 12
)
save_slide(p2, file.path(out, "slide_confidence_hardship_rank.png"))

# Slide 3: confident mirror (optional for deck; same house style)
p3 <- plot_confidence_slide(
  d0, band = "Confident",
  title = title_c, subtitle = sub_c,
  ncol = 3, value_size = 3.0
)
save_slide(p3, file.path(out, "slide_confidence_confident.png"))

progress("done. demography slides written to ", out)
progress("  slide_confidence_unconfident.png")
progress("  slide_confidence_hardship_rank.png")
progress("  slide_confidence_confident.png")
