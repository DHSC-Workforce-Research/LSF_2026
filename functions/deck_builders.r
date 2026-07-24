# ===========================================================================
# functions/deck_builders.r
#
# One builder per manifest row. Each takes the named list of source tables that
# functions/deck_helpers.r has already read, and RETURNS a ggplot object. A
# builder never saves, never names a file, and never fits a model: 03_deck.r
# owns saving, functions/deck_manifest.r owns numbering, 02_analysis.r owns
# every number on the slide.
#
# Plotting code is moved from the scripts named in the manifest's source_script
# column. The one systematic change is the data source: each block used to read
# an in-memory object left behind by the analysis in the same file, and now
# reads the persisted CSV. Where a plot relied on a factor's level order, the
# builder rebuilds the factor FROM ROW ORDER, because factor levels do not
# survive a CSV and the analysis layer writes each table in plot order.
#
# Shared slide furniture (palette, wrappers) lives here rather than being
# redefined per builder as it was across the old slide scripts.
# ===========================================================================

# ---- shared slide furniture ------------------------------------------------
deck_palette <- function() {
  list(
    teal = dcol("dhsc_teal", "#01A188"),
    blue = dcol("dhsc_blue", "#0063BE"),
    risk = dcol("risk", "#D4351C"),
    grey = dcol("midgrey", "#6F777B"),
    ink  = dcol("ink", "#0B0C0C")
  )
}
wrap_title <- function(x, w = 54)  stringr::str_wrap(x, width = w)
wrap_sub   <- function(x, w = 96)  stringr::str_wrap(x, width = w)
wrapcap    <- function(x, w = 122) stringr::str_wrap(x, width = w)
gbp        <- function(z) paste0("£", format(round(z), big.mark = ",", trim = TRUE))
gbp_axis   <- function(z) paste0("£", format(z, big.mark = ",", trim = TRUE))


# ===========================================================================
# A. PROBLEM
# ===========================================================================

# --- 1. erosion (from scripts/05c_erosion_slide.r) -------------------------
# The one slide that legitimately computes from a committed reference file
# rather than an analysis output: the series is CORE x CPI(2020)/CPI(year),
# arithmetic on the CPI index, with no model behind it. The harness recomputes
# the same three numbers the same way.
build_slide_rv_erosion <- function(tables) {
  p   <- deck_palette()
  CORE <- CORE_GRANT
  cpi <- tables[["reference/cpi_index.csv"]]
  if (!all(c("year", "cpi") %in% names(cpi)))
    stop("cpi_index.csv missing year/cpi (run 90_build_reference.r).")
  cpi_base <- cpi$cpi[cpi$year == BASE_YEAR][1]
  if (is.na(cpi_base)) stop("No CPI value for base year ", BASE_YEAR, ".")

  ero <- cpi |>
    transmute(
      year    = as.integer(year),
      nominal = CORE,
      real    = CORE * cpi_base / cpi
    ) |>
    arrange(year)

  end      <- ero |> slice_max(year, n = 1)
  loss_gbp <- round(end$nominal - end$real)
  loss_pct <- round(100 * (1 - end$real / end$nominal))
  real_end <- round(end$real)

  ero_long <- ero |>
    pivot_longer(c(nominal, real), names_to = "series", values_to = "gbp") |>
    mutate(series = recode(series,
                           nominal = "Cash value (frozen)",
                           real    = "Real value (CPI-adjusted)"))

  ggplot(ero, aes(x = year)) +
    # shaded lost-purchasing-power wedge between the two lines
    geom_ribbon(aes(ymin = real, ymax = nominal), fill = p$risk, alpha = 0.12) +
    geom_line(data = ero_long,
              aes(y = gbp, colour = series, linetype = series), linewidth = 1.3) +
    geom_point(data = ero_long, aes(y = gbp, colour = series), size = 2.2) +
    annotate("text", x = end$year, y = (end$nominal + end$real) / 2,
             label = sprintf("%s lost\n(-%d%%)", gbp(loss_gbp), loss_pct),
             hjust = 1.05, vjust = 0.5, colour = p$risk, fontface = "bold",
             size = 4.6, lineheight = 0.95) +
    scale_colour_manual(values = c("Cash value (frozen)" = p$grey,
                                   "Real value (CPI-adjusted)" = p$teal)) +
    scale_linetype_manual(values = c("Cash value (frozen)" = "22",
                                     "Real value (CPI-adjusted)" = "solid")) +
    scale_x_continuous(breaks = ero$year, expand = expansion(mult = c(0.02, 0.10))) +
    scale_y_continuous(limits = c(0, CORE * 1.05), labels = gbp_axis,
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = wrap_title(sprintf(
        "The Learning Support Fund has lost about %d%% of its real value since 2020", loss_pct)),
      subtitle = wrap_sub(sprintf(
        paste0("The core training grant has been frozen at %s since 2020. After general inflation ",
               "(CPI), it is worth about %s in 2020 money by %d, roughly %d%% less. The shaded ",
               "wedge is purchasing power lost to a frozen cash value."),
        gbp(CORE), gbp(real_end), end$year, loss_pct)),
      x = NULL, y = NULL, colour = NULL, linetype = NULL,
      caption = wrapcap(paste0(
        "Source: ONS CPI all-items index (D7BT), DHSC analysis. Real value = ", gbp(CORE),
        " x CPI(2020)/CPI(year). CPI excludes owner-occupier housing; where local rents rose faster ",
        "than the national basket, the real-value loss for students in high-cost areas is larger."))
    ) +
    theme_dhsc_slide(15) +
    theme(legend.position = "top",
          panel.grid.major.x = element_blank(),
          plot.margin = margin(16, 22, 12, 14))
}

# --- 2. place (from scripts/05d_framing_slides.r) --------------------------
# tbl_rv_place.csv is written ascending by real value, which is the plotting
# order; lab is refactored from row order rather than sorted again.
build_slide_rv_place <- function(tables) {
  p  <- deck_palette()
  d  <- tables[["tbl_rv_place.csv"]]
  yr <- d$year[1]
  CORE <- d$face_value[1]
  matched <- nrow(d)
  d <- d |> mutate(lab = factor(lab, levels = lab))

  ggplot(d, aes(x = rv, y = lab)) +
    geom_vline(xintercept = CORE, linetype = "22", colour = p$grey, linewidth = 0.5) +
    geom_segment(aes(x = 0, xend = rv, yend = lab), colour = "grey85", linewidth = 0.8) +
    geom_point(aes(colour = rv), size = 5) +
    geom_text(aes(label = gbp(rv)), hjust = -0.15, size = 3.7, colour = p$ink) +
    annotate("text", x = CORE, y = matched + 0.95, label = paste("Face value", gbp(CORE)),
             hjust = 0.5, vjust = 0, size = 3.4, colour = p$grey, family = "Arial") +
    scale_colour_gradient(low = p$risk, high = p$teal, guide = "none") +
    scale_x_continuous(limits = c(0, max(d$rv) * 1.20), labels = gbp_axis,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.6))) +
    labs(
      title = wrap_title("The same grant is worth far less where the cost of living is high"),
      subtitle = wrap_sub(paste0(
        "How far the universal ", gbp(CORE), " training grant stretches against LOCAL RENT in ", yr,
        ", scaled so it is worth the full ", gbp(CORE), " where rents are lowest and less where they are ",
        "higher. The cheapest and most expensive university areas in the country are shown at the ends.")),
      x = NULL, y = NULL,
      caption = wrapcap(paste0(
        "Source: ONS private rents (TTWA), DHSC analysis. ", gbp(CORE), " deflated by local rent only, ",
        "anchored so the lowest-rent English university area equals face value. Recognisable ",
        "providers plus the national cheapest and most expensive."))
    ) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 22, 12, 14))
}

# --- 3. package (from scripts/05d_framing_slides.r) ------------------------
# tbl_rv_package.csv is written ascending by amount; cat is refactored from row
# order so the four tiers stack in schedule order.
build_slide_rv_package <- function(tables) {
  p     <- deck_palette()
  pkg   <- tables[["tbl_rv_package.csv"]]
  n_tot <- pkg$n_total[1]
  pkg   <- pkg |> mutate(cat = factor(cat, levels = cat))

  ggplot(pkg, aes(x = pct, y = cat, fill = amount)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 4.4, colour = p$ink) +
    scale_fill_gradient(low = p$blue, high = p$teal, guide = "none") +
    scale_x_continuous(limits = c(0, max(pkg$pct) * 1.20),
                       labels = function(z) paste0(z, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_discrete(limits = rev) +
    labs(
      title = wrap_title("On top of the core grant, some students receive more"),
      subtitle = wrap_sub(paste0(
        "Every LSF student gets the universal ", gbp(CORE_GRANT), " training grant. Parents and carers add ",
        gbp(PARENTAL_SUPPORT), "; shortage-specialist subjects add ", gbp(SPECIALIST_SUBJECT),
        "; some get both, up to ", gbp(CORE_GRANT + PARENTAL_SUPPORT + SPECIALIST_SUBJECT), ". ",
        "Share of students at each total package (n = ", format(n_tot, big.mark = ","), ").")),
      x = NULL, y = NULL,
      caption = wrapcap(paste0(
        "Source: NHS LSF analysis sample, DHSC. Non-means-tested core components (training / parental / ",
        "specialist); hardship and expenditure elements excluded."))
    ) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 22, 12, 14))
}


# ===========================================================================
# B. RETENTION  and  D. ROBUSTNESS (survivorship, AUC)
#
# All six moved from scripts/03_visualise.r. That script uses a DIFFERENT
# palette from the real-value slides: af_teal #28A197 and af_orange #F46A25,
# not dhsc_teal #01A188. The difference is preserved rather than unified,
# because unifying it would silently change how six existing slides look. It
# is worth settling deliberately after checkpoint 2, not during a code move.
# ===========================================================================

deck_palette_panel <- function() {
  list(
    risk   = dcol("risk", "#D4351C"),
    teal   = dcol("af_teal", "#28A197"),
    orange = dcol("af_orange", "#F46A25"),
    grey   = dcol("midgrey", "#6F777B"),
    ink    = dcol("ink", "#0B0C0C"),
    blue   = dcol("dhsc_blue", "#0063BE")
  )
}
panel_src <- function() "Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis."
capt_theme <- function() theme(plot.caption = element_text(lineheight = 1.15))
panel_wrapcap <- function(x, w = 135) str_wrap(x, w)
pred_levels <- function()
  rev(c("Aware of grant before applying", "Grant influenced enrolment",
        "Grant helps me stay", "Funding critical to WHERE to study",
        "Funding critical to WHAT to study"))

# --- 4. retention funnel (03_visualise p8) ---------------------------------
build_slide_retention <- function(tables) {
  p <- deck_palette_panel()
  ret <- tables[["tbl_retention_funnel.csv"]] |> mutate(grp = paste0(length_years, "-year courses"))
  ggplot(ret, aes(study_year, survival_pct, colour = grp, group = grp)) +
    geom_line(linewidth = 1.1) + geom_point(size = 3.4) +
    geom_text(data = filter(ret, length_years == 3), aes(label = sprintf("%.0f%%", survival_pct)), vjust = -1.2, size = 4.2, show.legend = FALSE) +
    geom_text(data = filter(ret, length_years == 4), aes(label = sprintf("%.0f%%", survival_pct)), vjust = 2.0, size = 4.2, show.legend = FALSE) +
    scale_x_continuous(breaks = 1:4, labels = paste("Year", 1:4), expand = expansion(add = c(.15, .35))) +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20), labels = \(z) paste0(z, "%")) +
    scale_colour_manual(values = c("3-year courses" = p$teal, "4-year courses" = p$orange)) +
    labs(title = lbl("retention","title"), subtitle = lbl("retention","subtitle"),
         x = "Year of study", y = "Share of starters still enrolled", colour = NULL,
         caption = panel_wrapcap(paste("Course length derived from the data. Only cohorts old enough to be observed to their final year are included (3-year: 2021-2023 starts; 4-year: 2021-2022). 2020 pilot excluded.", panel_src()))) +
    theme_dhsc_slide(base = 15) + capt_theme() + theme(legend.position = "top", panel.grid.major.x = element_blank())
}

# --- 5. retention by course (03_visualise p9) ------------------------------
build_slide_retention_courses <- function(tables) {
  p  <- deck_palette_panel()
  rc <- tables[["tbl_retention_by_course.csv"]]
  ord <- rc |> distinct(course, starters) |> arrange(desc(starters)) |> pull(course)
  rc <- rc |> mutate(course = factor(course, levels = ord))
  ggplot(rc, aes(study_year, survival_pct)) +
    geom_line(colour = p$teal, linewidth = 1) + geom_point(colour = p$teal, size = 2.4) +
    geom_text(aes(label = sprintf("%.0f%%", survival_pct)), vjust = -0.8, size = 3, colour = p$ink) +
    facet_wrap(~course, ncol = 4) +
    scale_x_continuous(breaks = 1:4, labels = paste0("Y", 1:4)) +
    scale_y_continuous(limits = c(0, 115), breaks = c(0, 50, 100), labels = \(z) paste0(z, "%")) +
    labs(title = lbl("retention_courses","title"), subtitle = lbl("retention_courses","subtitle"),
         x = "Year of study", y = "Share of starters still enrolled",
         caption = panel_wrapcap(paste("Observed proportions, not modelled. Only cohorts observed to their final year are included. 2020 pilot excluded.", panel_src()))) +
    theme_dhsc_slide(base = 13) + capt_theme() + theme(strip.text = element_text(size = 10), panel.grid.major.x = element_blank())
}

# --- 6. considered-leaving intention (03_visualise p7) ---------------------
build_slide_intention <- function(tables) {
  p <- deck_palette_panel()
  intent <- tables[["tbl_dynamics_intention.csv"]] |>
    mutate(grp = ifelse(considered_leaving, "Said they might leave", "Did not"),
           grp = factor(grp, levels = c("Did not", "Said they might leave")))
  ggplot(intent, aes(left_next_pct, grp, fill = considered_leaving)) +
    geom_col(width = .55, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.0f%%", left_next_pct)), hjust = -0.25, size = 5.5, colour = p$ink) +
    scale_fill_manual(values = c(`FALSE` = p$teal, `TRUE` = p$orange)) +
    scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 10), labels = \(z) paste0(z, "%")) +
    labs(title = lbl("intention","title"), subtitle = lbl("intention","subtitle"),
         x = "Share no longer claiming the following year", y = NULL,
         caption = panel_wrapcap(paste("Counted only where the student had course left and a full next year of data existed.", panel_src()))) +
    theme_dhsc_slide(base = 15) + capt_theme() + theme(panel.grid.major.y = element_blank())
}

# --- 7. what drives leaving (03_visualise p2) ------------------------------
build_slide_factors <- function(tables) {
  p   <- deck_palette_panel()
  fac <- tables[["tbl_factors.csv"]] |>
    mutate(direction = ifelse(OR >= 1, "More likely to leave", "Less likely to leave")) |> arrange(OR)
  fac$factor <- factor(fac$factor, levels = fac$factor)
  ggplot(fac, aes(OR, factor, colour = direction)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = p$grey) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25, linewidth = .5) + geom_point(size = 3.2) +
    geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1.1, size = 3.6, show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0.8, 1.3, 0.1)) +
    scale_colour_manual(values = c("More likely to leave" = p$risk, "Less likely to leave" = p$teal)) +
    labs(title = lbl("factors","title"), subtitle = lbl("factors","subtitle"),
         x = "Odds of leaving before finishing (1.0 = no difference)", y = NULL, colour = NULL,
         caption = panel_wrapcap(paste("Single-factor logistic models, course and cohort fixed effects.", panel_src()))) +
    theme_dhsc_slide(base = 15) + capt_theme() + theme(legend.position = "top")
}

# --- 13. survivorship and selection (03_visualise p4) ----------------------
build_slide_survivorship <- function(tables) {
  p <- deck_palette_panel()
  surv <- tables[["tbl_survivorship.csv"]] |>
    mutate(spec = factor(spec, levels = c("All students", "Reached year 2", "Year 2 + financial confidence")),
           predictor = factor(predictor, levels = pred_levels()))
  ggplot(surv, aes(OR, predictor, colour = spec)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = p$grey) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .45, position = position_dodge(width = .6)) +
    geom_point(size = 2.8, position = position_dodge(width = .6)) +
    scale_x_continuous(breaks = seq(0.8, 1.5, 0.1)) +
    scale_colour_manual(values = c("All students" = p$grey, "Reached year 2" = p$orange,
                                   "Year 2 + financial confidence" = p$blue)) +
    labs(title = lbl("survivorship","title"), subtitle = lbl("survivorship","subtitle"),
         x = "Odds ratio (1.0 = no difference)", y = NULL, colour = NULL,
         caption = panel_wrapcap(paste("Logistic models, course and cohort fixed effects.", panel_src()))) +
    theme_dhsc_slide(base = 15) + capt_theme() + theme(legend.position = "top")
}

# --- 14. honest ceiling, AUC by predicted-risk decile (03_visualise p1) ----
build_slide_auc <- function(tables) {
  p   <- deck_palette_panel()
  dec <- tables[["tbl_auc_decile.csv"]]
  sm  <- tables[["tbl_auc_summary.csv"]] |> filter(outcome == "left_before_finish")
  base <- sm$base_rate * 100; a <- sm$auc_survey
  dec <- dec |> mutate(top = decile == max(decile))
  g <- ggplot(dec, aes(decile, leave_rate)) +
    geom_hline(yintercept = base, linetype = "dashed", colour = p$grey, linewidth = .6) +
    geom_col(aes(fill = top), width = .78, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.0f%%", leave_rate)), vjust = -0.6, size = 4.2, colour = p$ink) +
    annotate("text", x = 0.6, y = base + 2.2, hjust = 0, label = sprintf("Overall average, %.0f%%", base), colour = p$grey, size = 4.2) +
    scale_fill_manual(values = c(`FALSE` = p$teal, `TRUE` = p$orange)) +
    scale_x_continuous(breaks = 1:10, labels = c("Lowest\npredicted\nrisk", 2:9, "Highest\npredicted\nrisk"), expand = expansion(add = .6)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = \(z) paste0(z, "%")) +
    labs(title = lbl("auc","title"), subtitle = lbl("auc","subtitle"),
         x = "Students ranked by the model's predicted risk of leaving (lowest to highest)",
         y = "Share who left before finishing",
         caption = panel_wrapcap(paste("Logistic model of the five entry funding items.", panel_src()))) +
    theme_dhsc_slide(base = 15) + capt_theme() +
    theme(panel.grid.major.x = element_blank(), plot.margin = margin(16, 22, 12, 12))
  box_txt <- lbl("auc","box")
  box_txt <- gsub("{auc}",    sprintf("%.2f", a),       box_txt, fixed = TRUE)
  box_txt <- gsub("{aucpct}", sprintf("%.0f", 100 * a), box_txt, fixed = TRUE)
  if (nzchar(box_txt))
    g <- g + annotate("label", x = 0.55, y = 96, hjust = 0, vjust = 1, label = box_txt,
                      fill = dcol("gridgrey", "#E6E6E6"), colour = p$ink, label.size = 0,
                      size = 4.1, lineheight = 1.03)
  g
}


# ===========================================================================
# C. FINDING  and  D. ROBUSTNESS (spec ladder)
#
# Moved from 07 (leave curve), 08 (hazard, three-arms note), 05e (recruitment)
# and 05b (spec ladder).
#
# WRAP WIDTHS AND THEMES ARE PER-SOURCE, ON PURPOSE. 07 and 08 wrap at 52/95/118
# and use their own theme_comms(); 05e wraps at 56/100/128; 05b at 130 and plain
# theme_dhsc_slide(); the section A slides at 54/96/122. Those differences change
# where every title breaks, so they are preserved rather than harmonised. 07 and
# 08 also carried near-identical private copies of theme_comms(), y_zoom_limits()
# and caption_y_zoom(); the copies differ only in two legend theme settings and
# in how the caption text is assembled, and both are kept below. Harmonising all
# of this is a deliberate job for after checkpoint 2.
# ===========================================================================

comms_wrap_title <- function(x, w = 52)  str_wrap(x, width = w)
comms_wrap_sub   <- function(x, w = 95)  str_wrap(x, width = w)
comms_wrapcap    <- function(x, w = 118) str_wrap(x, width = w)

# identical in 07 and 08
y_zoom_limits <- function(ymin, ymax, pad = 0.28) {
  span <- max(ymax - ymin, 0.01)
  lo <- max(0, ymin - pad * span)
  hi <- min(1, ymax + 0.12 * span)
  if (lo < 0.02) lo <- 0
  if (lo > 0) {
    step <- if (span < 0.05) 0.01 else if (span < 0.15) 0.02 else 0.05
    lo <- max(0, floor(lo / step) * step)
  }
  c(lo, hi)
}
scales_pretty_pct <- function(yl, n = 5) pretty(yl, n = n)

# 07's variant: adds legend.box / legend.margin. 08's omits both.
theme_comms <- function(base = 14, legend_box = TRUE) {
  t <- theme_dhsc_slide(base = base) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base * 1.35, face = "bold", lineheight = 1.05,
                                         margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(size = base * 0.92, colour = "grey30", lineheight = 1.12,
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = base * 0.72, colour = "grey45", hjust = 0,
                                           lineheight = 1.1, margin = ggplot2::margin(t = 8)),
      plot.margin = ggplot2::margin(14, 20, 12, 16),
      legend.position = "top")
  if (legend_box)
    t <- t + ggplot2::theme(legend.box = "vertical", legend.margin = ggplot2::margin(b = 4))
  t
}

# CORRECTED 2026-07-24. This read "frozen nominal x CPIH x local rent (TTWA)",
# which is the DOUBLE-COUNTED product Lee identified and replaced on 2026-07-15
# (housing is already inside CPIH, so multiplying by a full local-rent haircut
# counts it twice and overstates erosion). The 2026-07-15 label sweep fixed the
# captions in 04/05/06/08 but missed this one in 07, so the wrong methodology
# statement was still printing under the Arm 1 curve while the axis label on the
# same slide said "Weighted CoL rent+CPI". Now derived from HOUSING_WEIGHT so it
# cannot drift from the measure again.
comms_src <- function() sprintf(paste0(
  "Source: NHS LSF panel 2020-2026, DHSC analysis. Real LSF = nominal package deflated by a weighted ",
  "cost-of-living index (%.0f%% local rent TTWA, %.0f%% CPI). ",
  "Predicted probabilities for a typical student profile. Associational, not causal."),
  100 * HOUSING_WEIGHT, 100 * (1 - HOUSING_WEIGHT))

caption_y_zoom_curves <- function(y_lo) {
  base <- comms_wrapcap(comms_src())
  if (!is.na(y_lo) && y_lo > 0.005)
    paste0(base, "\nNote: y-axis does not start at 0% (scale zoomed to show the gradient).")
  else base
}
caption_y_zoom_hazard <- function(y_lo, extra = "") {
  base <- comms_wrapcap(paste0(
    "Source: NHS LSF panel 2020-2026, DHSC analysis. Real LSF = ", PRIMARY_LBL, ". ",
    "Associational, not causal. ", extra))
  if (!is.na(y_lo) && y_lo > 0.005)
    paste0(base, "\nNote: y-axis does not start at 0% (scale zoomed to show the gradient).")
  else base
}

# 07's generic curve plotter, unchanged except that it takes its palette as an
# argument rather than closing over script-level colour bindings.
curve_plot <- function(curves, title, subtitle, y_lab, colours = NULL,
                       annotate_pp = NULL, zoom_y = TRUE) {
  p <- deck_palette()
  orange <- dcol("af_orange", "#F46A25"); purple <- dcol("af_purple", "#A285D1")
  curves <- curves |> mutate(spec = factor(spec, levels = unique(spec)))
  if (is.null(colours)) {
    labs_s <- levels(curves$spec)
    cols <- c(p$teal, p$blue, orange, purple)[seq_along(labs_s)]
    names(cols) <- labs_s
  } else cols <- colours

  y_data_lo <- min(curves$lo, na.rm = TRUE)
  y_data_hi <- max(curves$hi, na.rm = TRUE)
  yl <- if (isTRUE(zoom_y)) y_zoom_limits(y_data_lo, y_data_hi) else c(0, min(1, y_data_hi * 1.08 + 0.02))
  x_max <- max(curves$rv_gbp, na.rm = TRUE)
  callout_y <- yl[1] + 0.92 * (yl[2] - yl[1])

  g <- ggplot(curves, aes(rv_gbp, p, colour = spec, fill = spec)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    scale_y_continuous(labels = function(z) paste0(round(100 * z), "%"), limits = yl,
                       expand = expansion(mult = c(0.02, 0.04)), breaks = scales_pretty_pct(yl)) +
    scale_x_continuous(labels = function(z) format(round(z), big.mark = ",", scientific = FALSE),
                       expand = expansion(mult = c(0.02, 0.04))) +
    labs(title = comms_wrap_title(title), subtitle = comms_wrap_sub(subtitle),
         x = paste0("Real LSF value (£, ", PRIMARY_LBL, ")"),
         y = comms_wrap_title(y_lab, w = 28), colour = NULL, fill = NULL,
         caption = caption_y_zoom_curves(yl[1])) +
    theme_comms(base = 14) +
    theme(panel.grid.major.x = element_line(colour = "#E6E6E6", linewidth = 0.4),
          axis.title.y = element_text(margin = ggplot2::margin(r = 8)))

  if (!is.null(annotate_pp) && nrow(annotate_pp) > 0) {
    a <- annotate_pp |> dplyr::slice_tail(n = 1)
    txt <- sprintf("£1,000 lower real LSF\n(around the mean):\nabout %+.1f pp on\npredicted probability\n(%s)",
                   a$pp_increase_if_1k_less, sub("^S([0-9]).*", "S\\1", a$spec))
    g <- g + annotate("label", x = x_max, y = callout_y, hjust = 1, vjust = 1, label = txt,
                      fill = "#F4F4F4", colour = p$ink, size = 3.3, label.size = 0, lineheight = 1.05)
  }
  g
}

# --- 8. Arm 1 leave curve (07) ---------------------------------------------
# tbl_rv_pred_curves.csv holds six curve families stacked; filter to the leaving
# outcome by its `outcome` column rather than by spec label.
build_slide_rv_leave_curve <- function(tables) {
  curves <- tables[["tbl_rv_pred_curves.csv"]] |> filter(outcome == "left_before_finish")
  pp     <- tables[["tbl_rv_pp_per_1k.csv"]]   |> filter(outcome == "left_before_finish")
  curve_plot(
    curves,
    title = "As real LSF at entry falls, predicted leaving rises",
    subtitle = paste0(
      "ARM 1 (main retention): one observation per student. Real LSF is measured at course entry only ",
      "(not year 2/3 while still enrolled). Outcome = left before finishing (once). ",
      "S0 = real LSF only; S1 = course + entry-year FE; S2 = + funding survey answers. ",
      "Ribbon = 95% CI for a typical student. Not individual prediction."),
    y_lab = "Predicted probability of leaving before finishing",
    annotate_pp = pp)
}

# --- 9. Arm 2 hazard (08) --------------------------------------------------
build_slide_rv_hazard_leave_next <- function(tables) {
  p <- deck_palette()
  haz_curves <- tables[["tbl_hazard_pred_curves.csv"]]
  pp_haz     <- tryCatch(deck_table("tbl_hazard_pp_1k.csv"), error = function(e) NULL)
  yl_h <- y_zoom_limits(min(haz_curves$lo), max(haz_curves$hi))

  g <- ggplot(haz_curves, aes(rv_gbp, p, colour = spec, fill = spec)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = c("S0: real LSF only" = p$teal, "S1: course + year FE" = p$blue)) +
    scale_fill_manual(values = c("S0: real LSF only" = p$teal, "S1: course + year FE" = p$blue)) +
    scale_y_continuous(labels = function(z) paste0(round(100 * z), "%"), limits = yl_h,
                       expand = expansion(mult = c(0.02, 0.04))) +
    scale_x_continuous(labels = function(z) format(round(z), big.mark = ","),
                       expand = expansion(mult = c(0.02, 0.04))) +
    labs(
      title = comms_wrap_title("Thinner real LSF this year, higher chance of leaving next year"),
      subtitle = comms_wrap_sub(paste0(
        "Hazard: among students still expected to have course left, predicted probability that this year ",
        "is their last LSF claim. One exit per spell (not double-counted across earlier years). ",
        "S1 = course and survey-year fixed effects. Ribbon = 95% CI for a typical profile.")),
      x = paste0("Real LSF value this year (£, ", PRIMARY_LBL, ")"),
      y = "Predicted probability of leaving by next year",
      colour = NULL, fill = NULL,
      caption = caption_y_zoom_hazard(yl_h[1],
        "At-risk sample uses course-length expected finish; exit = last claim year.")) +
    theme_comms(14, legend_box = FALSE)

  if (!is.null(pp_haz) && nrow(pp_haz) > 0) {
    a <- pp_haz |> filter(grepl("S1", spec)) |> slice(1)
    if (nrow(a))
      g <- g + annotate("label", x = max(haz_curves$rv_gbp),
                        y = yl_h[1] + 0.9 * (yl_h[2] - yl_h[1]), hjust = 1, vjust = 1,
                        label = sprintf("£1,000 lower real LSF\n(around the mean):\nabout %+.1f pp on\nleave-next probability\n(S1)", a$pp),
                        fill = "#F4F4F4", colour = p$ink, size = 3.3, label.size = 0, lineheight = 1.05)
  }
  g
}

# --- 10. three arms summary (08 note slide) --------------------------------
# A branded table image, not a chart. It is a methods-clarity slide: the content
# is fixed text, so it needs no source table. The manifest still names tables
# for it; they are read and ignored, which keeps every row uniform.
build_slide_rv_three_arms <- function(tables) {
  note_df <- tibble::tribble(
    ~Arm, ~Unit, ~`Real LSF timing`, ~Outcome,
    "1. Entry leave (MAIN)", "One row per student", "Entry year only", "Left before finishing (once)",
    "2. Hazard", "Student-year (at risk)", "This survey year", "Gone next year (last claim)",
    "3. Recruitment", "Provider x year", "Place-year (core grant)", "Count of first-year claimants"
  )
  dhsc_table_plot(
    note_df,
    title = comms_wrap_title("Three ways we use real LSF", w = 48),
    subtitle = comms_wrap_sub(paste0(
      "Arm 1 does not use year-2/year-3 real LSF and does not double-count multi-wave students. ",
      "Arm 2 asks whether thinner real LSF this year raises the chance of not returning next year. ",
      "Arm 3 asks whether places where real LSF fell harder also saw fewer first-year claimants.")),
    caption = comms_wrapcap(paste0(
      "Source: NHS LSF panel 2020-2026. Real LSF construct: ", PRIMARY_LBL,
      " on nominal package (arms 1-2) or core training grant (arm 3). Associational.")),
    base_size = 13)
}

# --- 11. Arm 3 recruitment effect (05e) ------------------------------------
# The provider-year cell table is optional: 05e tolerated its absence and
# dropped the counts from the caption, so the same tryCatch is kept here rather
# than making it a required manifest table.
build_slide_rv_recruit_effect <- function(tables) {
  pal    <- deck_palette()
  orange <- dcol("af_orange", "#F46A25")
  w_title <- function(x) str_wrap(x, width = 56)
  w_sub   <- function(x) str_wrap(x, width = 100)
  w_cap   <- function(x) str_wrap(x, width = 128)

  fe    <- tables[["tbl_recruit_fe_results.csv"]]
  cells <- tryCatch(deck_table("tbl_recruit_provider_year.csv"), error = function(e) NULL)
  n_py   <- if (!is.null(cells)) nrow(cells) else NA_integer_
  n_prov <- if (!is.null(cells)) dplyr::n_distinct(cells$provider) else NA_integer_

  # 95% CI on the "% change per £1,000 less" from the fitted b and se.
  # effect of £1,000 LESS = exp(-1000*b); CI from b +/- 1.96*se (the -1000 flips
  # the interval, so the low end uses b + 1.96se and the high end b - 1.96se).
  d <- fe |>
    mutate(
      pct      = pct_change_starters_if_1k_less,
      pct_lo   = 100 * (exp(-1000 * (b + 1.96 * se)) - 1),
      pct_hi   = 100 * (exp(-1000 * (b - 1.96 * se)) - 1),
      crosses0 = pct_lo <= 0 & pct_hi >= 0,
      lab = dplyr::recode(spec,
        "S0: no FE"                          = "Raw comparison\n(no adjustment)",
        "S1: provider + year FE (preferred)" = "Each provider vs itself\nover time (preferred)",
        "S2: region + year FE"               = "Within region,\nover time"),
      lab = factor(lab, levels = rev(lab)))

  xr  <- range(c(d$pct_lo, d$pct_hi, 0))
  pad <- 0.14 * diff(xr)

  ggplot(d, aes(pct, lab, colour = crosses0)) +
    geom_vline(xintercept = 0, colour = pal$grey, linewidth = 0.6) +
    geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = 0.16, linewidth = 1.1) +
    geom_point(size = 5) +
    geom_text(aes(label = sprintf("%+.0f%%", pct)), vjust = -1.2, size = 4.2, colour = pal$ink) +
    scale_colour_manual(values = c(`TRUE` = pal$grey, `FALSE` = orange), guide = "none") +
    scale_x_continuous(limits = c(xr[1] - pad, xr[2] + pad), labels = function(z) paste0(z, "%")) +
    labs(
      title = w_title("Recruitment: no reliable link once you compare like with like"),
      subtitle = w_sub(paste0(
        "Estimated change in the number of first-year LSF claimants if a place's real grant value were ",
        "£1,000 lower. The raw comparison (orange) looks large and negative, but it just reflects big ",
        "expensive cities vs small cheap towns. Comparing each provider with itself over time, the range ",
        "crosses zero, so there is no reliable effect.")),
      x = "Change in first-year claimants per £1,000 lower real LSF  (dot = estimate, bar = 95% CI)",
      y = NULL,
      caption = w_cap(paste0(
        "Source: NHS LSF panel",
        if (!is.na(n_py)) paste0(", ", format(n_py, big.mark = ","),
                                 " provider-years across ", n_prov, " providers") else "",
        ". Grey = 95% range includes zero (no reliable effect); orange = raw, unadjusted (confounded by place)."))
    ) +
    theme_dhsc_slide(15) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 22, 12, 14))
}

# --- 12. spec ladder (05b slide A) -----------------------------------------
build_slide_rv_spec_ladder <- function(tables) {
  pal  <- deck_palette()
  wcap <- function(x) str_wrap(x, 130)
  src  <- paste0("Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis. ",
                 "Associational models; course and entry-year fixed effects.")
  lad <- tables[["tbl_rv_spec_ladder.csv"]] |>
    filter(outcome_var == "left_before_finish", scale == "per 1 SD",
           spec %in% c("S0", "S1", "S2", "S3")) |>
    mutate(
      spec_label = factor(spec_label, levels = rev(unique(spec_label[order(spec)]))),
      direction = ifelse(OR < 1, "Higher real value -> less leaving", "Higher real value -> more leaving"))

  ggplot(lad, aes(OR, spec_label, colour = direction)) +
    geom_vline(xintercept = 1, colour = pal$grey, linewidth = 0.35, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.55) +
    geom_point(size = 3.2) +
    geom_text(aes(label = sprintf("%.3f", OR)), vjust = -1.05, size = 3.6, colour = pal$ink, show.legend = FALSE) +
    scale_colour_manual(values = c(
      "Higher real value -> less leaving" = pal$teal,
      "Higher real value -> more leaving" = pal$risk)) +
    labs(
      title = "Does real LSF value still predict leaving once we control for survey answers?",
      subtitle = paste0(
        "Odds ratio per 1 SD of rent-adjusted (TTWA) real grant value. ",
        "S0 = real value only; S1 = + funding survey items; S2 = + grant components; ",
        "S3 = S1 + financial confidence (year-2 survivors)."),
      x = "Odds ratio of leaving before finishing (1.0 = no association)",
      y = NULL, colour = NULL,
      caption = wcap(src)) +
    theme_dhsc_slide(base = 14) +
    theme(legend.position = "top")
}
