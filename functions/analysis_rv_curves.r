# ===========================================================================
# functions/analysis_rv_curves.r
#
# analysis_rv_curves()  -  the comms layer's arithmetic: predicted-probability
# curves across the real-value range, and the percentage-point effect of
# 1,000 pounds less real LSF. Three families of curve: leaving, unconfident
# (year-2 and later), and the aware / funding-salient associations.
#
# Moved from scripts/07_real_value_comms.r, ANALYSIS HALF ONLY. The plot helper
# and seven slide blocks stay behind for task 6; the curves they draw are
# persisted here, which is what lets the deck layer fit nothing.
#
# Changes, and only these: constants from scripts/00_config.r (GRID_N, 60L in
# this script, is now GRID_N_CURVE since 08 used the same name for 50L); the
# function wrapper and two spaces of indentation. No statistical code is
# touched.
#
# Writes to outputs_dir()/real_value_comms/:
#   tbl_rv_pred_curves.csv          predicted-probability curves (deck slide 8)
#   tbl_rv_pp_per_1k.csv            percentage-point effect per 1,000 less
#   tbl_rv_outcome_prevalence.csv   base rates behind the curves
# ===========================================================================

analysis_rv_curves <- function() {
  set.seed(1)
  
  # ---- CONFIG (constants live in scripts/00_config.r) ------------------------


  out_root <- outputs_dir()
  out <- file.path(out_root, "real_value_comms")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)

  teal   <- dcol("dhsc_teal", "#01A188")
  blue   <- dcol("dhsc_blue", "#0063BE")
  orange <- dcol("af_orange", "#F46A25")
  purple <- dcol("af_purple", "#A285D1")
  grey   <- dcol("midgrey", "#6F777B")
  ink    <- dcol("ink", "#0B0C0C")
  risk   <- dcol("risk", "#D4351C")
  src    <- paste0(
    "Source: NHS LSF panel 2020-2026, DHSC analysis. Real LSF = frozen nominal x CPIH x local rent (TTWA). ",
    "Predicted probabilities for a typical student profile. Associational, not causal."
  )
  # Widescreen 16:9: keep titles/subtitles inside the frame (Lee feedback 2026-07-13)
  wrap_title <- function(x, w = 52) stringr::str_wrap(x, width = w)
  wrap_sub   <- function(x, w = 95) stringr::str_wrap(x, width = w)
  wrapcap    <- function(x, w = 118) stringr::str_wrap(x, width = w)

  # Theme bits shared by all comms slides: multi-line title/subtitle + breathing room
  theme_comms <- function(base = 14) {
    theme_dhsc_slide(base = base) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = base * 1.35, face = "bold", lineheight = 1.05,
          margin = ggplot2::margin(b = 6)),
        plot.subtitle = ggplot2::element_text(
          size = base * 0.92, colour = "grey30", lineheight = 1.12,
          margin = ggplot2::margin(b = 10)),
        plot.caption = ggplot2::element_text(
          size = base * 0.72, colour = "grey45", hjust = 0, lineheight = 1.1,
          margin = ggplot2::margin(t = 8)),
        plot.margin = ggplot2::margin(14, 20, 12, 16),
        legend.position = "top",
        legend.box = "vertical",
        legend.margin = ggplot2::margin(b = 4)
      )
  }

  # Zoom y to data range (not forced from 0) so modest gradients are visible.
  # No // axis-break symbol (removed: looked poor on these slides). Caption notes
  # when the scale does not start at zero.
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

  # Caption only (no visual // mark)
  caption_y_zoom <- function(y_lo) {
    base <- wrapcap(src)
    if (!is.na(y_lo) && y_lo > 0.005) {
      paste0(base, "\nNote: y-axis does not start at 0% (scale zoomed to show the gradient).")
    } else {
      base
    }
  }

  progress("07: load + build real LSF ...")
  SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

  samp <- build_real_value(SAMPLE, ref, awards, cpih, base_year = 2020)
  if (!"parental"   %in% names(samp)) samp$parental   <- 0L
  if (!"specialist" %in% names(samp)) samp$specialist <- 0L
  if (!"regional"   %in% names(samp)) samp$regional   <- 0L

  samp <- samp |>
    mutate(
      crit_course = as.integer(suppressWarnings(as.integer(funding_imp_crse)) >= 4L),
      crit_uni    = as.integer(suppressWarnings(as.integer(funding_imp_uni))  >= 4L),
      across(any_of(c(
        "fund_availability", "grant_influence", "grant_helps_stay",
        "parental", "specialist", "left_before_finish", "one_wave_only",
        "left_2y_plus_early", "considered_leaving"
      )), to_01),
      course     = as.character(course),
      entry_year = as.integer(entry_year),
      confidence = suppressWarnings(as.integer(confidence)),
      rv_gbp     = as.numeric(.data[[PRIMARY]]),
      # outcomes for comms
      unconfident = as.integer(!is.na(confidence) & confidence <= 2L),
      aware       = fund_availability,
      # at least one "funding was critical / influenced me" signal
      funding_salient = as.integer(
        coalesce(crit_course, 0L) == 1L |
        coalesce(crit_uni, 0L) == 1L |
        coalesce(grant_influence, 0L) == 1L |
        coalesce(grant_helps_stay, 0L) == 1L
      ),
      # tighter variants (for optional facet table)
      funding_choice = as.integer(
        coalesce(crit_course, 0L) == 1L | coalesce(crit_uni, 0L) == 1L
      ),
      funding_enrol = as.integer(
        coalesce(grant_influence, 0L) == 1L | coalesce(grant_helps_stay, 0L) == 1L
      )
    )

  # drop rows with no real LSF
  samp <- samp |> filter(!is.na(rv_gbp))

  # ---- typical profile (for smooth curves) ----------------------------------
  # entry_year may be integer or factor (after as_model_df); median needs numeric.
  as_year_num <- function(x) {
    if (is.null(x)) return(integer(0))
    if (is.factor(x)) {
      # prefer labels ("2021") over codes (1,2,3)
      return(suppressWarnings(as.integer(as.character(x))))
    }
    if (is.character(x)) return(suppressWarnings(as.integer(x)))
    as.integer(x)
  }

  modal_course <- samp |>
    count(course, sort = TRUE) |>
    slice(1) |>
    pull(course)

  med_year_of <- function(x) {
    y <- as_year_num(x)
    y <- y[!is.na(y)]
    if (!length(y)) return(2022L)
    as.integer(stats::median(y))
  }

  typical <- function(data) {
    list(
      course = {
        x <- data$course
        x <- x[!is.na(x)]
        if (!length(x)) as.character(modal_course) else names(sort(table(x), decreasing = TRUE))[1]
      },
      entry_year = med_year_of(data$entry_year),
      fund_availability = mean(as.numeric(data$fund_availability), na.rm = TRUE),
      grant_influence   = mean(as.numeric(data$grant_influence), na.rm = TRUE),
      crit_course       = mean(as.numeric(data$crit_course), na.rm = TRUE),
      crit_uni          = mean(as.numeric(data$crit_uni), na.rm = TRUE),
      grant_helps_stay  = mean(as.numeric(data$grant_helps_stay), na.rm = TRUE)
    )
  }

  # grid over p5-p95 of real LSF (avoid extremes)
  rv_lo <- as.numeric(stats::quantile(samp$rv_gbp, 0.05, na.rm = TRUE))
  rv_hi <- as.numeric(stats::quantile(samp$rv_gbp, 0.95, na.rm = TRUE))
  rv_grid <- seq(rv_lo, rv_hi, length.out = GRID_N_CURVE)

  # ---- fit + predict helpers ------------------------------------------------
  fit_logit <- function(data, y, rhs) {
    f <- stats::as.formula(paste0(y, " ~ ", rhs))
    suppressWarnings(stats::glm(f, data = data, family = stats::binomial()))
  }

  # predicted P(y=1) over rv_grid for a typical profile, with Wald ribbon on link scale
  pred_curve <- function(model, data, y_name, spec_label, grid = rv_grid) {
    typ <- typical(data)
    # build one-row skeleton then expand grid
    # factor levels must match the fitted model exactly
    course_lv <- if ("course" %in% names(model$xlevels)) model$xlevels$course else levels(factor(data$course))
    year_lv   <- if ("entry_year" %in% names(model$xlevels)) model$xlevels$entry_year else {
      yn <- as_year_num(data$entry_year)
      as.character(sort(unique(yn[!is.na(yn)])))
    }
    course_use <- as.character(typ$course)
    if (!length(course_lv) || !course_use %in% course_lv) course_use <- course_lv[1]
    year_use <- as.character(typ$entry_year)
    if (!length(year_lv) || !year_use %in% year_lv) year_use <- year_lv[1]

    base <- data.frame(
      rv_gbp = grid,
      course = factor(course_use, levels = course_lv),
      entry_year = factor(year_use, levels = year_lv),
      fund_availability = typ$fund_availability,
      grant_influence   = typ$grant_influence,
      crit_course       = typ$crit_course,
      crit_uni          = typ$crit_uni,
      grant_helps_stay  = typ$grant_helps_stay,
      stringsAsFactors = FALSE
    )
    pr <- stats::predict(model, newdata = base, type = "link", se.fit = TRUE)
    tibble(
      outcome = y_name,
      spec = spec_label,
      rv_gbp = grid,
      p  = stats::plogis(pr$fit),
      lo = stats::plogis(pr$fit - 1.96 * pr$se.fit),
      hi = stats::plogis(pr$fit + 1.96 * pr$se.fit),
      n  = nrow(model$model)
    )
  }

  # pp change when real LSF falls by £1,000 around the mean
  pp_per_1k <- function(curve) {
    mu <- mean(curve$rv_gbp)
    # interpolate p at mu and mu-1000
    p_at <- function(x) {
      stats::approx(curve$rv_gbp, curve$p, xout = x, rule = 2)$y
    }
    p0 <- p_at(mu)
    p1 <- p_at(mu - 1000)
    tibble(
      outcome = curve$outcome[1],
      spec = curve$spec[1],
      at_mean_gbp = mu,
      p_at_mean = p0,
      p_at_mean_minus_1k = p1,
      pp_increase_if_1k_less = 100 * (p1 - p0),
      n = curve$n[1]
    )
  }

  # force factor versions for FE specs
  as_model_df <- function(d) {
    d |>
      mutate(
        course = factor(course),
        entry_year = factor(entry_year)
      )
  }

  # ---- 1) LEAVE curves ------------------------------------------------------
  progress("07: leave probability curves ...")

  d_leave <- samp |>
    filter(!is.na(left_before_finish), !is.na(rv_gbp), !is.na(course), !is.na(entry_year)) |>
    as_model_df()

  specs_leave <- list(
    list(lab = "S0: real LSF only",
         rhs = "rv_gbp",
         need = c("left_before_finish", "rv_gbp")),
    list(lab = "S1: + course + entry year",
         rhs = "rv_gbp + course + entry_year",
         need = c("left_before_finish", "rv_gbp", "course", "entry_year")),
    list(lab = "S2: + funding survey answers",
         rhs = paste("rv_gbp + course + entry_year + fund_availability + grant_influence",
                     "+ crit_course + crit_uni + grant_helps_stay"),
         need = c("left_before_finish", "rv_gbp", "course", "entry_year",
                  "fund_availability", "grant_influence", "crit_course",
                  "crit_uni", "grant_helps_stay"))
  )

  curves_leave <- list()
  for (sp in specs_leave) {
    dd <- d_leave
    ok <- Reduce(`&`, lapply(sp$need, function(v) !is.na(dd[[v]])))
    dd <- dd[ok, , drop = FALSE]
    message("  leave / ", sp$lab, " n=", nrow(dd))
    m <- fit_logit(dd, "left_before_finish", sp$rhs)
    curves_leave[[sp$lab]] <- pred_curve(m, dd, "left_before_finish", sp$lab)
  }
  leave_curves <- bind_rows(curves_leave)
  leave_pp <- bind_rows(lapply(curves_leave, pp_per_1k))

  # ---- 2) UNCONFIDENT curves (year-2+ only) ---------------------------------
  progress("07: unconfident probability curves ...")

  d_conf <- samp |>
    filter(!is.na(unconfident), !is.na(confidence), !is.na(rv_gbp),
           !is.na(course), !is.na(entry_year)) |>
    as_model_df()

  specs_conf <- list(
    list(lab = "S0: real LSF only",
         rhs = "rv_gbp",
         need = c("unconfident", "rv_gbp")),
    list(lab = "S1: + course + entry year",
         rhs = "rv_gbp + course + entry_year",
         need = c("unconfident", "rv_gbp", "course", "entry_year"))
  )

  curves_conf <- list()
  for (sp in specs_conf) {
    dd <- d_conf
    ok <- Reduce(`&`, lapply(sp$need, function(v) !is.na(dd[[v]])))
    dd <- dd[ok, , drop = FALSE]
    message("  unconfident / ", sp$lab, " n=", nrow(dd))
    m <- fit_logit(dd, "unconfident", sp$rhs)
    curves_conf[[sp$lab]] <- pred_curve(m, dd, "unconfident", sp$lab)
  }
  conf_curves <- bind_rows(curves_conf)
  conf_pp <- bind_rows(lapply(curves_conf, pp_per_1k))

  # ---- 3) AWARE + FUNDING SALIENT (association with real LSF) ---------------
  progress("07: aware + funding-salient curves ...")

  assoc_one <- function(y, lab) {
    dd <- samp |>
      filter(!is.na(.data[[y]]), !is.na(rv_gbp), !is.na(course), !is.na(entry_year)) |>
      as_model_df()
    specs <- list(
      list(lab = "S0: real LSF only", rhs = "rv_gbp"),
      list(lab = "S1: + course + entry year", rhs = "rv_gbp + course + entry_year")
    )
    out <- list()
    for (sp in specs) {
      m <- fit_logit(dd, y, sp$rhs)
      out[[sp$lab]] <- pred_curve(m, dd, y, sp$lab)
    }
    list(curves = bind_rows(out), pp = bind_rows(lapply(out, pp_per_1k)), n = nrow(dd), label = lab)
  }

  aware_res   <- assoc_one("aware", "Aware of grant before applying")
  salient_res <- assoc_one("funding_salient", "Funding salient (any critical/influence item)")
  choice_res  <- assoc_one("funding_choice", "Funding critical to course or university")
  enrol_res   <- assoc_one("funding_enrol", "Grant influenced enrolment or helps stay")

  # ---- save tables ----------------------------------------------------------
  all_curves <- bind_rows(
    leave_curves, conf_curves,
    aware_res$curves, salient_res$curves,
    choice_res$curves, enrol_res$curves
  )
  all_pp <- bind_rows(
    leave_pp, conf_pp,
    aware_res$pp, salient_res$pp,
    choice_res$pp, enrol_res$pp
  ) |>
    mutate(
      across(c(p_at_mean, p_at_mean_minus_1k, pp_increase_if_1k_less),
             ~ round(.x, 3)),
      at_mean_gbp = round(at_mean_gbp, 0)
    )

  write_csv(all_curves, file.path(out, "tbl_rv_pred_curves.csv"))
  write_csv(all_pp, file.path(out, "tbl_rv_pp_per_1k.csv"))

  # prevalence callouts
  prev <- tibble(
    outcome = c("left_before_finish", "unconfident", "aware", "funding_salient"),
    rate = c(
      mean(d_leave$left_before_finish, na.rm = TRUE),
      mean(d_conf$unconfident, na.rm = TRUE),
      mean(samp$aware, na.rm = TRUE),
      mean(samp$funding_salient, na.rm = TRUE)
    ),
    n = c(nrow(d_leave), nrow(d_conf),
          sum(!is.na(samp$aware)), sum(!is.na(samp$funding_salient)))
  )
  write_csv(prev, file.path(out, "tbl_rv_outcome_prevalence.csv"))

  invisible(TRUE)
}
