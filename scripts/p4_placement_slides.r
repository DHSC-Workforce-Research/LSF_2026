# ===========================================================================
# scripts/p4_placement_slides.r
#
# U6 - DHSC-theme 16:9 PNGs for the deck. Reads the CSV tables written by p1,
# p1b, p2 and p3 and refits nothing, so a label tweak costs a second rather
# than a model run.
#
# Three slides:
#   1. diverging lollipop: placement hours (left) vs leaving before finishing
#      (right), one row per programme, ranked by hours. If leaving tracked
#      hours the two sides would mirror. They do not.
#   2. Arm P1, the £1,000 effect at a low-hours vs a high-hours course.
#   3. Arm P2, per-family coefficients with families that lack identifying
#      variation shown as marked-not-estimated rather than dropped.
#
# Slide text lives in a local JSON that is NOT committed (data-leak
# precaution already in force on main). If it is absent, defaults below are
# used and the slide still renders.
#
# ASCII only in code; £ appears in label strings, so source with encoding.
#   source("scripts/p4_placement_slides.r", encoding = "UTF-8")
# Outputs -> outputs_dir()/placement_hours_pack_YYYYMMDD/slide_*.png
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2); library(stringr)
})
update_geom_defaults("text", list(family = "Arial"))

teal   <- dcol("dhsc_teal", "#01A188")
blue   <- dcol("af_blue",   "#12436D")
orange <- dcol("af_orange", "#F46A25")
grey   <- dcol("midgrey",   "#6F777B")
ink    <- dcol("ink",       "#0B0C0C")
wrap_title <- function(x, w = 56)  str_wrap(x, width = w)
wrap_sub   <- function(x, w = 100) str_wrap(x, width = w)
wrapcap    <- function(x, w = 128) str_wrap(x, width = w)

stamp <- format(Sys.Date(), "%Y%m%d")
pack  <- file.path(outputs_dir(), paste0("placement_hours_pack_", stamp))
if (!dir.exists(pack)) stop("no pack folder at ", pack,
                            "\n  Run p1/p1b/p2/p3 first.", call. = FALSE)

# ---- labels ----------------------------------------------------------------
lab_path <- file.path(pack, "placement_slide_labels.json")
LAB <- list(
  s1_title = "Placement hours vary sixfold across programmes; leaving before finishing does not follow",
  s2_title = "A thinner grant raises leaving on every course, not just long-placement ones",
  s3_title = "Within a subject, more placement hours send mixed signals (exploratory)",
  source   = "Source: NHS LSF panel 2020-2026; DHSC programme placement hours, FY26/27."
)
if (file.exists(lab_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  user_lab <- tryCatch(jsonlite::read_json(lab_path, simplifyVector = TRUE),
                       error = function(e) NULL)
  if (!is.null(user_lab)) LAB <- utils::modifyList(LAB, user_lab)
}

rd <- function(f) {
  p <- file.path(pack, f)
  if (!file.exists(p)) { message("p4: missing ", f, " - slide skipped"); return(NULL) }
  read_csv(p, show_col_types = FALSE, progress = FALSE)
}

band_lvl <- c("low", "medium", "high", "unmatched")

# ===========================================================================
# Slide 1: diverging lollipop - placement hours vs leaving, by programme.
# Left (blue) = mean placement hours/yr. Right (orange) = % left before
# finishing. Programmes ranked by hours. Bar length scaled within each metric;
# tip prints the real value. If leaving tracked hours the sides would mirror.
# ===========================================================================
prog <- rd("P1_outcomes_by_programme.csv")
if (!is.null(prog)) {
  progress("p4: slide 1 - programme lollipop ...")

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

  s1 <- ggplot(dl, aes(x, prog_lab, colour = metric)) +
    geom_vline(xintercept = 0, colour = grey, linewidth = 0.6) +
    geom_segment(aes(x = 0, xend = x, yend = prog_lab), linewidth = 1, na.rm = TRUE) +
    geom_point(size = 3.6, na.rm = TRUE) +
    geom_text(data = d, inherit.aes = FALSE,
              aes(x = x_hours, y = prog_lab, label = round(hours)),
              hjust = 1.35, size = 3.2, colour = ink) +
    geom_text(data = subset(d, !is.na(exit)), inherit.aes = FALSE,
              aes(x = x_exit, y = prog_lab, label = sprintf("%.0f%%", exit)),
              hjust = -0.35, size = 3.2, colour = ink) +
    scale_colour_manual(values = c("Placement hours per year" = blue,
                                   "% left before finishing" = orange), name = NULL) +
    scale_x_continuous(limits = c(-1.5, 1.5), breaks = NULL) +
    coord_cartesian(clip = "off") +
    labs(title = wrap_title(LAB$s1_title),
         subtitle = wrap_sub(paste0(
           "Left (blue): mean placement hours per year by programme, DHSC FY26/27. ",
           "Right (orange): unadjusted percentage leaving before finishing. Programmes ",
           "ordered by placement hours. Bar length is scaled within each metric; the tip ",
           "prints the observed value. If leaving tracked hours the two sides would mirror.")),
         x = NULL, y = NULL,
         caption = wrapcap(paste0(
           LAB$source,
           " Unadjusted, no controls. Programme cells below n=10 suppressed (no percentage shown)."))) +
    theme_dhsc_slide(15) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          plot.margin = margin(16, 40, 12, 14))

  save_slide(s1, file.path(pack, "slide_placement_programme_lollipop.png"))
}

# ===========================================================================
# Slide 2: Arm P1 marginal effect at low vs high hours.
# ===========================================================================
marg <- rd("P2_marginal_by_hours.csv")
if (!is.null(marg)) {
  progress("p4: slide 2 - Arm P1 marginal effects ...")

  d <- marg |>
    filter(outcome == "left_before_finish") |>
    mutate(band = factor(hours_band, levels = band_lvl),
           pct    = pct_higher_odds,
           pct_lo = 100 * (lo - 1),
           pct_hi = 100 * (hi - 1),
           crosses0 = pct_lo <= 0 & pct_hi >= 0,
           lab = sprintf("%s hours/yr\n(%s band)", hours_per_year, hours_band),
           lab = factor(lab, levels = rev(lab))) |>
    filter(!is.na(band))

  if (nrow(d)) {
    xr  <- range(c(d$pct_lo, d$pct_hi, 0), na.rm = TRUE)
    pad <- 0.16 * diff(xr)

    s2 <- ggplot(d, aes(pct, lab, colour = crosses0)) +
      geom_vline(xintercept = 0, colour = grey, linewidth = 0.6) +
      geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = 0.16, linewidth = 1.1) +
      geom_point(size = 5) +
      geom_text(aes(label = sprintf("%+.0f%%", pct)), vjust = -1.2, size = 4.2, colour = ink) +
      scale_colour_manual(values = c(`TRUE` = grey, `FALSE` = orange), guide = "none") +
      scale_x_continuous(limits = c(xr[1] - pad, xr[2] + pad),
                         labels = function(z) paste0(z, "%")) +
      labs(title = wrap_title(LAB$s2_title),
           subtitle = wrap_sub(paste0(
             "Marginal effect from a logistic model of leaving-before-finishing on ",
             "(real LSF value x placement hours), with course and entry-year fixed effects. ",
             "Points: change in odds per £1,000 lower real grant value, evaluated at a ",
             "short- and a long-placement course. Identified from within-course variation in ",
             "real value across areas and years.")),
           x = "Change in odds of leaving per £1,000 lower real LSF  (dot = estimate, bar = 95% CI)",
           y = NULL,
           caption = wrapcap(paste0(
             LAB$source,
             " The placement-hours main effect is absorbed by course fixed effects by design",
             " and is not estimated here. Grey = 95% range includes zero."))) +
      theme_dhsc_slide(15) +
      theme(panel.grid.major.y = element_blank(),
            plot.margin = margin(16, 22, 12, 14))

    save_slide(s2, file.path(pack, "slide_placement_interaction.png"))
  }
}

# ===========================================================================
# Slide 3: Arm P2 per-family coefficients, non-identified families marked.
# ===========================================================================
fam <- rd("P3_family_fe_results.csv")
if (!is.null(fam)) {
  progress("p4: slide 3 - Arm P2 per-family ...")

  d <- fam |>
    filter(outcome == "left_before_finish") |>
    mutate(estimated = is.finite(OR),
           lab = ifelse(scope == "pooled", "All families pooled", scope),
           lab = factor(lab, levels = rev(unique(lab))),
           flag = ifelse(!is.na(note) & nzchar(note) & estimated, "!", ""))

  est <- filter(d, estimated)
  non <- filter(d, !estimated)

  if (nrow(est)) {
    xr  <- range(c(est$lo, est$hi, 1), na.rm = TRUE)
    pad <- 0.14 * diff(xr)

    s3 <- ggplot(est, aes(OR, lab)) +
      geom_vline(xintercept = 1, colour = grey, linewidth = 0.6) +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16,
                     linewidth = 1.1, colour = teal) +
      geom_point(size = 5, colour = teal) +
      geom_text(aes(label = sprintf("%.2f%s", OR, flag)),
                vjust = -1.2, size = 4.2, colour = ink) +
      scale_x_continuous(limits = c(xr[1] - pad, xr[2] + pad)) +
      labs(title = wrap_title(LAB$s3_title),
           subtitle = wrap_sub(paste0(
             "Odds ratio for leaving-before-finishing per 100 additional placement hours ",
             "per year, logistic model with course-family (not course) fixed effects. ",
             "Identified from between-course, within-family variation in programme hours. ",
             "EXPLORATORY: subject differences within a family are uncontrolled, and no ",
             "demographic covariates exist on this branch.")),
           x = "Odds ratio per 100 extra placement hours per year (1 = no difference)",
           y = NULL,
           caption = wrapcap(paste0(
             LAB$source,
             if (nrow(non))
               paste0(" Not estimated (no identifying variation): ",
                      paste(sprintf("%s - %s", non$lab, non$note), collapse = "; "), ".")
             else "",
             " ! = crosswalk-ambiguous. No demographic controls are available on this branch."))) +
      theme_dhsc_slide(15) +
      theme(panel.grid.major.y = element_blank(),
            plot.margin = margin(16, 22, 12, 14))

    save_slide(s3, file.path(pack, "slide_placement_family_fe.png"))
  } else {
    message("p4: no estimated family coefficients - slide 3 skipped")
  }
}

progress("p4: done -> ", pack)