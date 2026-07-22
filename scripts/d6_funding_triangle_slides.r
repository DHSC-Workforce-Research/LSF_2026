# ===========================================================================
# scripts/d6_funding_triangle_slides.r
#
# Equity slides for the funding-decision triangle (risk + dependence), from
# the rates table (d5). DHSC 16:9, same grammar as d3:
#   - bold title = finding; grey subtitle = technical definition
#   - bar per group, sorted; dashed line = n-weighted average of shown groups
#   - colour: more than 1 SD from that average (red = above, teal = below)
#   - Ethnicity and Religion get their own full-height slides (too many groups
#     for a facet cell); the sparser characteristics share a faceted hero slide
#   - a ranked callout lists the groups furthest above the average
#
# Non-response ("(blank)"/"NULL") already dropped in d5; "Prefer not to say"
# and suppressed cells are excluded from the plotted comparisons here.
#
# EQUITY / DISTRIBUTIONAL framing, NOT causal. Colour marks where risk and
# dependence CONCENTRATE; it is not a claim that a grant cut causes a group to
# leave. The associational ceiling from the main analysis still holds.
#
# Prereq:  d4 -> d5 -> d6
#   source("scripts/d6_funding_triangle_slides.r", encoding = "UTF-8")
# Output -> outputs_dir()/slide_<question>_{equity,ethnicity,religion,rank}.png
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(stringr); library(tidyr)
})

out   <- outputs_dir()
rates <- read_csv(file.path(derived_dir(), "funding_triangle_rates.csv"),
                  show_col_types = FALSE)

ink  <- dcol("ink", "#0B0C0C")
grey <- dcol("midgrey", "#6F777B")
TEAL <- "#01A188"; RED <- "#D4351C"; GREY <- "#B1B4B6"
DENSE <- c("Ethnicity", "Religion")

wrap_title <- function(x, w = 72)  str_wrap(x, width = w)
wrap_sub   <- function(x, w = 118) str_wrap(x, width = w)
wrapcap    <- function(x, w = 128) str_wrap(x, width = w)
src <- paste(
  "Source: NHS Learning Support Fund demographic cross-tabs (BSA questionnaire",
  "analysis), DHSC analysis. Within-group shares; denominator is the summed",
  "response cells. Equity/distributional, not a model of leaving. Non-response",
  "and 'prefer not to say' excluded; groups under n=10 suppressed."
)

slide_text_theme <- function(title_size = 20, sub_size = 12.5) {
  theme(
    plot.title    = element_text(size = title_size, face = "bold", colour = ink,
                                 lineheight = 1.12, margin = margin(b = 6)),
    plot.subtitle = element_text(size = sub_size, colour = "grey30",
                                 lineheight = 1.18, margin = margin(b = 10)),
    plot.caption  = element_text(lineheight = 1.15),
    plot.margin   = margin(14, 22, 10, 14)
  )
}

# attach status vs the n-weighted average of the supplied rows -----------------
add_status <- function(d, above, below, k = 1) {
  ref <- stats::weighted.mean(d$pct, d$n)
  s   <- sqrt(stats::weighted.mean((d$pct - ref)^2, d$n))
  z   <- if (s > 0) (d$pct - ref) / s else rep(0, nrow(d))
  d$ref    <- ref
  d$status <- ifelse(abs(z) <= k, "Typical", ifelse(z > 0, above, below))
  d$status <- factor(d$status, levels = c(below, "Typical", above))
  d
}
statcols <- function(above, below) {
  setNames(c(TEAL, GREY, RED), c(below, "Typical", above))
}

# faceted equity map (several sparse demographics) -----------------------------
plot_equity_facet <- function(d, title, subtitle, above, below, axis,
                              ncol = 3, value_size = 3.1) {
  d <- add_status(d, above, below)
  d <- d |>
    group_by(demog) |>
    mutate(key = factor(Group, levels = Group[order(pct)])) |>
    ungroup()
  ggplot(d, aes(pct, key, fill = status)) +
    geom_col(width = 0.72) +
    geom_vline(aes(xintercept = ref), linetype = "dashed", colour = grey, linewidth = 0.45) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.12, size = value_size, colour = ink) +
    scale_fill_manual(values = statcols(above, below), drop = FALSE) +
    facet_wrap(~ demog, scales = "free_y", ncol = ncol) +
    expand_limits(x = max(d$pct, na.rm = TRUE) * 1.14) +
    labs(title = wrap_title(title), subtitle = wrap_sub(subtitle),
         x = axis, y = NULL, fill = NULL, caption = wrapcap(src)) +
    theme_dhsc_slide(base = 14) + slide_text_theme() +
    theme(legend.position = "top", panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0, size = 12),
          axis.text.y = element_text(size = 10.5))
}

# single dense demographic, full height ---------------------------------------
plot_equity_single <- function(d, title, subtitle, above, below, axis,
                               value_size = 3.5) {
  d <- add_status(d, above, below)
  d$key <- factor(d$Group, levels = d$Group[order(d$pct)])
  ggplot(d, aes(pct, key, fill = status)) +
    geom_col(width = 0.72) +
    geom_vline(aes(xintercept = ref), linetype = "dashed", colour = grey, linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.12, size = value_size, colour = ink) +
    scale_fill_manual(values = statcols(above, below), drop = FALSE) +
    expand_limits(x = max(d$pct, na.rm = TRUE) * 1.16) +
    labs(title = wrap_title(title), subtitle = wrap_sub(subtitle),
         x = axis, y = NULL, fill = NULL, caption = wrapcap(src)) +
    theme_dhsc_slide(base = 14) + slide_text_theme() +
    theme(legend.position = "top", panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 12))
}

# ranked callout: groups furthest above the whole-question average -------------
plot_rank <- function(d, title, subtitle, above, axis, top_n = 12) {
  ref <- stats::weighted.mean(d$pct, d$n)
  d$gap <- d$pct - ref
  d <- d[order(-d$gap), , drop = FALSE]
  d <- utils::head(d, top_n)
  d$label <- paste0(d$Group, "  (", d$demog, ")")
  d$label <- factor(d$label, levels = rev(d$label))
  ggplot(d, aes(pct, label)) +
    geom_col(width = 0.7, fill = RED) +
    geom_vline(xintercept = ref, linetype = "dashed", colour = grey, linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%  (+%.0fpp)", pct, gap)),
              hjust = -0.08, size = 3.9, colour = ink) +
    annotate("text", x = ref, y = Inf, vjust = 1.4, hjust = -0.05,
             label = sprintf("Survey average  %.0f%%", ref), colour = grey, size = 3.9) +
    scale_x_continuous(limits = c(0, max(d$pct) * 1.24), labels = function(z) paste0(z, "%")) +
    labs(title = wrap_title(title), subtitle = wrap_sub(subtitle),
         x = axis, y = NULL, caption = wrapcap(src)) +
    theme_dhsc_slide(base = 15) + slide_text_theme(title_size = 20, sub_size = 13) +
    theme(panel.grid.major.y = element_blank())
}

# ---- per-question config ----------------------------------------------------
Q <- tibble::tribble(
  ~slug,                      ~above,                          ~below,                          ~axis,                                                        ~lever,
  "leave_course",             "More at-risk than average",     "Less at-risk than average",     "Share who felt they may leave their course (%)",             "retention risk",
  "funding_influence_course", "More dependent than average",   "Less dependent than average",   "Share rating funding important to WHAT to study (4-5, %)",    "funding dependence",
  "funding_influence_hei",    "More dependent than average",   "Less dependent than average",   "Share rating funding important to WHERE to study (4-5, %)",   "funding dependence"
)

plot_dat <- rates |> filter(!suppressed, !is_pref)

hero_title <- c(
  leave_course             = "Who feels most at risk of leaving is uneven across claimant groups",
  funding_influence_course = "Funding dependence for what to study is uneven across groups",
  funding_influence_hei    = "Funding dependence for where to study is uneven across groups"
)

purrr::walk(seq_len(nrow(Q)), function(i) {
  q  <- Q[i, ]
  d  <- filter(plot_dat, question_slug == q$slug)
  if (!nrow(d)) { progress("d6: no rows for ", q$slug, " - skipped"); return(invisible()) }

  refq <- round(stats::weighted.mean(d$pct, d$n), 0)
  sub_common <- sprintf(
    "Dashed line = n-weighted average across shown groups (~%d%%). Colour: more than 1 SD from that average (red = above, teal = below). %s lever; equity/distributional, not causal.",
    refq, str_to_sentence(q$lever))

  sparse <- filter(d, !demog %in% DENSE)
  eth    <- filter(d, demog == "Ethnicity")
  rel    <- filter(d, demog == "Religion")

  save_slide(
    plot_equity_facet(sparse, hero_title[[q$slug]], sub_common, q$above, q$below, q$axis),
    file.path(out, paste0("slide_", q$slug, "_equity.png")))

  if (nrow(eth))
    save_slide(
      plot_equity_single(eth,
        sprintf("%s: %s varies sharply by ethnicity", str_to_sentence(q$lever),
                if (q$slug == "leave_course") "risk" else "dependence"),
        sub_common, q$above, q$below, q$axis),
      file.path(out, paste0("slide_", q$slug, "_ethnicity.png")))

  if (nrow(rel))
    save_slide(
      plot_equity_single(rel,
        sprintf("%s also varies by religion", str_to_sentence(q$lever)),
        sub_common, q$above, q$below, q$axis),
      file.path(out, paste0("slide_", q$slug, "_religion.png")))

  save_slide(
    plot_rank(d,
      sprintf("Groups reporting the highest %s", q$lever),
      sprintf("Top groups by percentage points above the survey average (%d%%). A targeting signal for where support concentrates, not a model of leaving.", refq),
      q$above, q$axis),
    file.path(out, paste0("slide_", q$slug, "_rank.png")))

  progress("d6: ", q$slug, " slides written")
})

progress("d6: done -> ", out)