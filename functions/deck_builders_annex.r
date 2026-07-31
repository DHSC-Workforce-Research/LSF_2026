# ===========================================================================
# functions/deck_builders_annex.r
#
# Builders for the placement (15-17) and equity (18-21) annex sections, plus
# the plotting helpers they need. Split out of deck_builders.r only for size.
#
# The helper functions below are moved verbatim from d3, d6, d7 and p4, with
# ONE class of change: every script defined its own wrap_title/wrap_sub/
# wrapcap/slide_text_theme at different widths, and those names would collide
# once they share a file. They are prefixed by source (d3_, d6_, d7_, p4_) and
# the widths preserved exactly, because the width decides where every title
# breaks on the slide.
# ===========================================================================

# ---- d3: financial confidence ---------------------------------------------
d3_ink <- function() dcol("ink", "#0B0C0C")
d3_grey <- function() dcol("midgrey", "#6F777B")
d3_src <- function() deck_text_common()$src_d3
d3_wrap_title <- function(x, w = 72)  str_wrap(x, width = w)
d3_wrap_sub   <- function(x, w = 118) str_wrap(x, width = w)
d3_wrapcap    <- function(x, w = 128) str_wrap(x, width = w)
d3_slide_text_theme <- function(title_size = 20, sub_size = 12.5) {
  theme(
    plot.title = element_text(
      size = title_size, face = "bold", colour = d3_ink(),
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

d3_plot_confidence_slide <- function(banded_df, band = "Unconfident",
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
               colour = d3_grey(), linewidth = 0.45) +
    scale_fill_manual(values = cols, drop = FALSE) +
    labs(
      title    = d3_wrap_title(title),
      subtitle = d3_wrap_sub(subtitle),
      x = sprintf("Share %s (rated %s)", tolower(band), opts),
      y = NULL,
      fill = NULL,
      caption = d3_wrapcap(d3_src())
    ) +
    theme_dhsc_slide(base = 14) +
    d3_slide_text_theme(title_size = title_size, sub_size = sub_size) +
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
                       hjust = -0.12, size = value_size, colour = d3_ink())
  }
  p
}

# Ranked callout: groups furthest above the survey average on low confidence

# ---- d6: funding triangle equity ------------------------------------------
d6_ink <- function() dcol("ink", "#0B0C0C")
d6_grey <- function() dcol("midgrey", "#6F777B")
# d6 used flat hex constants for the status fills, not dcol lookups. Kept as-is.
TEAL <- "#01A188"; RED <- "#D4351C"; GREY <- "#B1B4B6"
d6_wrap_title <- function(x, w = 72)  str_wrap(x, width = w)
d6_wrap_sub   <- function(x, w = 118) str_wrap(x, width = w)
d6_wrapcap    <- function(x, w = 128) str_wrap(x, width = w)
d6_src <- function() deck_text_common()$src_d6
d6_slide_text_theme <- function(title_size = 20, sub_size = 12.5) {
  theme(
    plot.title    = element_text(size = title_size, face = "bold", colour = d6_ink(),
                                 lineheight = 1.12, margin = margin(b = 6)),
    plot.subtitle = element_text(size = sub_size, colour = "grey30",
                                 lineheight = 1.18, margin = margin(b = 10)),
    plot.caption  = element_text(lineheight = 1.15),
    plot.margin   = margin(14, 22, 10, 14)
  )
}

# attach status vs the n-weighted average of the supplied rows -----------------
d6_add_status <- function(d, above, below, k = 1) {
  ref <- stats::weighted.mean(d$pct, d$n)
  s   <- sqrt(stats::weighted.mean((d$pct - ref)^2, d$n))
  z   <- if (s > 0) (d$pct - ref) / s else rep(0, nrow(d))
  d$ref    <- ref
  d$status <- ifelse(abs(z) <= k, "Typical", ifelse(z > 0, above, below))
  d$status <- factor(d$status, levels = c(below, "Typical", above))
  d
}
d6_statcols <- function(above, below) {
  setNames(c(TEAL, GREY, RED), c(below, "Typical", above))
}

# faceted equity map (several sparse demographics) -----------------------------
d6_plot_equity_facet <- function(d, title, subtitle, above, below, axis,
                              ncol = 3, value_size = 3.1) {
  d <- d6_add_status(d, above, below)
  d <- d |>
    group_by(demog) |>
    mutate(key = factor(Group, levels = Group[order(pct)])) |>
    ungroup()
  ggplot(d, aes(pct, key, fill = status)) +
    geom_col(width = 0.72) +
    geom_vline(aes(xintercept = ref), linetype = "dashed", colour = d6_grey(), linewidth = 0.45) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.12, size = value_size, colour = d6_ink()) +
    scale_fill_manual(values = d6_statcols(above, below), drop = FALSE) +
    facet_wrap(~ demog, scales = "free_y", ncol = ncol) +
    expand_limits(x = max(d$pct, na.rm = TRUE) * 1.14) +
    labs(title = d6_wrap_title(title), subtitle = d6_wrap_sub(subtitle),
         x = axis, y = NULL, fill = NULL, caption = d6_wrapcap(d6_src())) +
    theme_dhsc_slide(base = 14) + d6_slide_text_theme() +
    theme(legend.position = "top", panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0, size = 12),
          axis.text.y = element_text(size = 10.5))
}

# single dense demographic, full height ---------------------------------------
d6_plot_equity_single <- function(d, title, subtitle, above, below, axis,
                               value_size = 3.5) {
  d <- d6_add_status(d, above, below)
  d$key <- factor(d$Group, levels = d$Group[order(d$pct)])
  ggplot(d, aes(pct, key, fill = status)) +
    geom_col(width = 0.72) +
    geom_vline(aes(xintercept = ref), linetype = "dashed", colour = d6_grey(), linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.12, size = value_size, colour = d6_ink()) +
    scale_fill_manual(values = d6_statcols(above, below), drop = FALSE) +
    expand_limits(x = max(d$pct, na.rm = TRUE) * 1.16) +
    labs(title = d6_wrap_title(title), subtitle = d6_wrap_sub(subtitle),
         x = axis, y = NULL, fill = NULL, caption = d6_wrapcap(d6_src())) +
    theme_dhsc_slide(base = 14) + d6_slide_text_theme() +
    theme(legend.position = "top", panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 12))
}

# ranked callout: groups furthest above the whole-question average -------------
d6_plot_rank <- function(d, title, subtitle, above, axis, top_n = 12) {
  ref <- stats::weighted.mean(d$pct, d$n)
  d$gap <- d$pct - ref
  d <- d[order(-d$gap), , drop = FALSE]
  d <- utils::head(d, top_n)
  d$label <- paste0(d$Group, "  (", d$demog, ")")
  d$label <- factor(d$label, levels = rev(d$label))
  ggplot(d, aes(pct, label)) +
    geom_col(width = 0.7, fill = RED) +
    geom_vline(xintercept = ref, linetype = "dashed", colour = d6_grey(), linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%  (+%.0fpp)", pct, gap)),
              hjust = -0.08, size = 3.9, colour = d6_ink()) +
    annotate("text", x = ref, y = Inf, vjust = 1.4, hjust = -0.05,
             label = sprintf("Survey average  %.0f%%", ref), colour = d6_grey(), size = 3.9) +
    scale_x_continuous(limits = c(0, max(d$pct) * 1.24), labels = function(z) paste0(z, "%")) +
    labs(title = d6_wrap_title(title), subtitle = d6_wrap_sub(subtitle),
         x = axis, y = NULL, caption = d6_wrapcap(d6_src())) +
    theme_dhsc_slide(base = 15) + d6_slide_text_theme(title_size = 20, sub_size = 13) +
    theme(panel.grid.major.y = element_blank())
}

# ---- per-question config ----------------------------------------------------

# ---- d7: funding triangle cross-question -----------------------------------
d7_ink <- function() dcol("ink", "#0B0C0C")
d7_grey <- function() dcol("midgrey", "#6F777B")
d7_wrap_title <- function(x, w = 74)  str_wrap(x, width = w)
d7_wrap_sub   <- function(x, w = 116) str_wrap(x, width = w)
d7_wrapcap    <- function(x, w = 128) str_wrap(x, width = w)
d7_src <- function() deck_text_common()$src_d7

d7_slide_text_theme <- function(title_size = 20, sub_size = 12.5) theme(
  plot.title    = element_text(size = title_size, face = "bold", colour = d7_ink(),
                               lineheight = 1.12, margin = margin(b = 6)),
  plot.subtitle = element_text(size = sub_size, colour = "grey30",
                               lineheight = 1.18, margin = margin(b = 10)),
  plot.caption  = element_text(lineheight = 1.15),
  plot.margin   = margin(14, 22, 10, 14))

d7_demog_cols <- c(
  "Age" = "#12436D", "Gender" = "#28A197", "Ethnicity" = "#801650",
  "Religion" = "#F46A25", "Disability" = "#3D3D3D", "Marital status" = "#A285D1",
  "Sexual orientation" = "#6BACE4", "Trans" = "#00703C",
  "Pregnancy/maternity" = "#D4351C")


# ---- p4: placement hours ---------------------------------------------------
p4_wrap_title <- function(x, w = 56)  str_wrap(x, width = w)
p4_wrap_sub   <- function(x, w = 100) str_wrap(x, width = w)
p4_wrapcap    <- function(x, w = 128) str_wrap(x, width = w)

# p4 used to carry its own label list plus a stray placement_slide_labels.json
# found by a recursive search of the outputs tree. Both are gone: placement text
# is in functions/deck_text.r with every other slide, under the manifest slugs
# placement_programme_lollipop / _interaction / _family_fe.


# ===========================================================================
# E. PLACEMENT ANNEX (15-17), from scripts/p4_placement_slides.r
# ===========================================================================

band_lvl <- function() c("low", "medium", "high", "unmatched")

# --- 15. placement hours by programme --------------------------------------
build_slide_placement_lollipop <- function(tables) {
  pal <- deck_palette()
  blue <- dcol("af_blue", "#12436D"); orange <- dcol("af_orange", "#F46A25")
  prog <- tables[["P1_outcomes_by_programme.csv"]]

  d <- prog |>
    mutate(hours = suppressWarnings(as.numeric(hours_per_year)),
           exit  = suppressWarnings(as.numeric(pct_left_before_finish)),
           fam   = if_else(is.na(course_family), "unmatched", as.character(course_family))) |>
    filter(!is.na(hours)) |>
    arrange(hours) |>
    mutate(prog_lab = sprintf("%s  (%s)", programme_name, fam),
           prog_lab = factor(prog_lab, levels = prog_lab),
           x_hours  = -(hours / max(hours, na.rm = TRUE)),
           x_exit   = exit / max(exit, na.rm = TRUE))

  dl <- d |>
    pivot_longer(c(x_hours, x_exit), names_to = "metric", values_to = "x") |>
    mutate(metric = if_else(metric == "x_hours",
                            "Placement hours per year", "% left before finishing"),
           metric = factor(metric, levels = c("Placement hours per year",
                                              "% left before finishing")))

  ggplot(dl, aes(x, prog_lab, colour = metric)) +
    geom_vline(xintercept = 0, colour = pal$grey, linewidth = 0.6) +
    geom_segment(aes(x = 0, xend = x, yend = prog_lab), linewidth = 1, na.rm = TRUE) +
    geom_point(size = 3.6, na.rm = TRUE) +
    geom_text(data = d, inherit.aes = FALSE,
              aes(x = x_hours, y = prog_lab, label = round(hours)),
              hjust = 1.35, size = 3.2, colour = pal$ink) +
    geom_text(data = subset(d, !is.na(exit)), inherit.aes = FALSE,
              aes(x = x_exit, y = prog_lab, label = sprintf("%.0f%%", exit)),
              hjust = -0.35, size = 3.2, colour = pal$ink) +
    scale_colour_manual(values = c("Placement hours per year" = blue,
                                   "% left before finishing" = orange), name = NULL) +
    scale_x_continuous(limits = c(-1.5, 1.5), breaks = NULL) +
    coord_cartesian(clip = "off") +
    labs(title    = lbl("placement_programme_lollipop", "title"),
         subtitle = lbl("placement_programme_lollipop", "subtitle"),
         x = NULL, y = NULL,
         caption  = lbl("placement_programme_lollipop", "caption")) +
    theme_dhsc_slide(15) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 40, 12, 14))
}

# --- 16. hours x real value interaction ------------------------------------
build_slide_placement_interaction <- function(tables) {
  pal <- deck_palette()
  orange <- dcol("af_orange", "#F46A25")
  marg <- tables[["P2_marginal_by_hours.csv"]]

  d <- marg |>
    filter(outcome == "left_before_finish") |>
    mutate(band = factor(hours_band, levels = band_lvl()),
           pct    = pct_higher_odds,
           pct_lo = 100 * (lo - 1),
           pct_hi = 100 * (hi - 1),
           crosses0 = pct_lo <= 0 & pct_hi >= 0,
           lab = sprintf("%s hours/yr\n(%s band)", hours_per_year, hours_band),
           lab = factor(lab, levels = rev(lab))) |>
    filter(!is.na(band))
  if (!nrow(d)) stop("no modelled hours bands in P2_marginal_by_hours.csv", call. = FALSE)

  xr  <- range(c(d$pct_lo, d$pct_hi, 0), na.rm = TRUE)
  pad <- 0.16 * diff(xr)

  ggplot(d, aes(pct, lab, colour = crosses0)) +
    geom_vline(xintercept = 0, colour = pal$grey, linewidth = 0.6) +
    geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = 0.16, linewidth = 1.1) +
    geom_point(size = 5) +
    geom_text(aes(label = sprintf("%+.0f%%", pct)), vjust = -1.2, size = 4.2, colour = pal$ink) +
    scale_colour_manual(values = c(`TRUE` = pal$grey, `FALSE` = orange), guide = "none") +
    scale_x_continuous(limits = c(xr[1] - pad, xr[2] + pad), labels = function(z) paste0(z, "%")) +
    labs(title    = lbl("placement_interaction", "title"),
         subtitle = lbl("placement_interaction", "subtitle"),
         x = lbl_raw("placement_interaction", "x_lab"),
         y = NULL,
         caption  = lbl("placement_interaction", "caption")) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 22, 12, 14))
}

# --- 17. placement hours under family FE -----------------------------------
build_slide_placement_family_fe <- function(tables) {
  pal <- deck_palette(); LAB <- p4_labels()
  fam <- tables[["P3_family_fe_results.csv"]]

  d <- fam |>
    filter(outcome == "left_before_finish") |>
    mutate(estimated = is.finite(OR),
           lab = ifelse(scope == "pooled", "All families pooled", scope),
           lab = factor(lab, levels = rev(unique(lab))),
           flag = ifelse(!is.na(note) & nzchar(note) & estimated, "!", ""))
  est <- filter(d, estimated)
  non <- filter(d, !estimated)
  if (!nrow(est)) stop("no estimated family coefficients in P3_family_fe_results.csv", call. = FALSE)

  xr  <- range(c(est$lo, est$hi, 1), na.rm = TRUE)
  pad <- 0.14 * diff(xr)

  ggplot(est, aes(OR, lab)) +
    geom_vline(xintercept = 1, colour = pal$grey, linewidth = 0.6) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16, linewidth = 1.1, colour = pal$teal) +
    geom_point(size = 5, colour = pal$teal) +
    geom_text(aes(label = sprintf("%.2f%s", OR, flag)), vjust = -1.2, size = 4.2, colour = pal$ink) +
    scale_x_continuous(limits = c(xr[1] - pad, xr[2] + pad)) +
    labs(title    = lbl("placement_family_fe", "title"),
         subtitle = lbl("placement_family_fe", "subtitle"),
         x = lbl_raw("placement_family_fe", "x_lab"),
         y = NULL,
         caption  = lbl("placement_family_fe", "caption",
           not_estimated = if (nrow(non))
             paste0(" Not estimated (no identifying variation): ",
                    paste(sprintf("%s - %s", non$lab, non$note), collapse = "; "), ".")
           else "")) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 22, 12, 14))
}


# ===========================================================================
# F. EQUITY ANNEX (18-21)
# ===========================================================================

# Shared setup for the two confidence slides, from d3's script body: drop total
# rows, split ethnicity out (too many categories for a multi-facet slide), and
# compute the n-weighted reference lines the subtitles quote.
confidence_ctx <- function(banded) {
  if (!"is_total" %in% names(banded))
    banded$is_total <- grepl("grand total|^total$", tolower(banded$Group))
  if (!"Net_pct" %in% names(banded)) banded <- add_net(banded)
  d0 <- banded[!isTRUE(banded$is_total) & !grepl("grand total|^total$", tolower(banded$Group)), ]
  is_eth <- grepl("^ethnicity$", d0$Demographic, ignore.case = TRUE)
  d_main <- d0[!is_eth, , drop = FALSE]; d_eth <- d0[is_eth, , drop = FALSE]
  if (!nrow(d_eth)) {
    is_eth <- grepl("ethnic", d0$Demographic, ignore.case = TRUE)
    d_main <- d0[!is_eth, , drop = FALSE]; d_eth <- d0[is_eth, , drop = FALSE]
  }
  list(d0 = d0, d_main = d_main, d_eth = d_eth,
       ref_u_main = stats::weighted.mean(d_main$Unconfident_pct, d_main$n),
       ref_u_all  = stats::weighted.mean(d0$Unconfident_pct, d0$n),
       ref_c_main = stats::weighted.mean(d_main$Confident_pct, d_main$n))
}

# --- 18. financial confidence, confident group (d3 slide 3) ----------------
build_slide_confidence_confident <- function(tables) {
  ctx <- confidence_ctx(tables[["financial_confidence_by_band.csv"]])
  d3_plot_confidence_slide(
    ctx$d_main, band = "Confident",
    title    = lbl("confidence_confident", "title"),
    subtitle = lbl("confidence_confident", "subtitle",
                   ref = sprintf("%.0f%%", ctx$ref_c_main)),
    ncol = 3, value_size = 3.2, title_size = 20, sub_size = 12.5)
}

# --- 19. financial confidence, unconfident group (d3 slide 1, the hero) ----
build_slide_confidence_unconfident <- function(tables) {
  ctx <- confidence_ctx(tables[["financial_confidence_by_band.csv"]])
  d3_plot_confidence_slide(
    ctx$d_main, band = "Unconfident",
    title    = lbl("confidence_unconfident", "title"),
    subtitle = lbl("confidence_unconfident", "subtitle",
                   ref = sprintf("%.0f%%", ctx$ref_u_main)),
    ncol = 3, value_size = 3.2, title_size = 20, sub_size = 12.5,
    strip_size = 12, axis_y_size = 11)
}

# --- 20. triangle: risk vs dependence (d7 scatter) -------------------------
# d7's scatter_slide() both drew and saved; here it only draws.
build_slide_triangle_risk_dep <- function(tables) {
  ink <- dcol("ink", "#0B0C0C"); grey <- dcol("midgrey", "#6F777B")
  gw <- tables[["triangle_group_matrix.csv"]]
  xv <- "risk"; yv <- "dep_where"
  d <- gw[is.finite(gw[[xv]]) & is.finite(gw[[yv]]), , drop = FALSE]
  if (nrow(d) < 5) stop("too few groups for the triangle scatter", call. = FALSE)
  rho <- suppressWarnings(cor(d[[xv]], d[[yv]], method = "spearman"))
  ggplot(d, aes(.data[[xv]], .data[[yv]])) +
    geom_smooth(method = "lm", se = FALSE, colour = grey, linewidth = 0.5, linetype = "dashed") +
    geom_point(aes(size = n_size, colour = demog), alpha = 0.85) +
    geom_text(aes(label = Group), size = 3, colour = ink, vjust = -0.9, check_overlap = TRUE) +
    scale_size(range = c(2, 9), guide = "none") +
    scale_colour_manual(values = d7_demog_cols, name = NULL) +
    labs(title    = lbl("triangle_scatter_risk_dependence", "title"),
         subtitle = lbl("triangle_scatter_risk_dependence", "subtitle",
                        rho = sprintf("%.2f", rho), n_groups = nrow(d)),
         x = lbl_raw("triangle_scatter_risk_dependence", "x_lab"),
         y = lbl_raw("triangle_scatter_risk_dependence", "y_lab"),
         caption = d7_wrapcap(d7_src())) +
    theme_dhsc_slide(14) + d7_slide_text_theme() +
    theme(legend.position = "bottom")
}

# --- 21. triangle rates by group (d6 equity facet, leave_course lever) -----
# d6 produced four slides per question across three questions. The manifest
# takes one: the retention-risk lever's facet slide, which is d6's own hero.
build_slide_triangle_rates <- function(tables) {
  rates <- tables[["funding_triangle_rates.csv"]]
  DENSE <- c("Ethnicity", "Religion")
  plot_dat <- rates |> filter(!suppressed, !is_pref) |> filter(question_slug == "leave_course")
  if (!nrow(plot_dat)) stop("no leave_course rows in funding_triangle_rates.csv", call. = FALSE)
  refq <- round(stats::weighted.mean(plot_dat$pct, plot_dat$n), 0)
  sub_common <- lbl("triangle_rates", "subtitle", ref = refq)
  sparse <- filter(plot_dat, !demog %in% DENSE)
  # title / axis / status labels taken verbatim from d6's Q tribble and
  # hero_title vector for the leave_course row.
  d6_plot_equity_facet(sparse,
    lbl("triangle_rates", "title"), sub_common,
    lbl_raw("triangle_rates", "lab_above"),
    lbl_raw("triangle_rates", "lab_below"),
    lbl_raw("triangle_rates", "x_lab"))
}
