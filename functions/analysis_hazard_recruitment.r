# ===========================================================================
# functions/analysis_hazard_recruitment.r
#
# analysis_hazard_recruitment()  -  Arms 2 and 3.
#   Arm 2 HAZARD:      real LSF in year t -> left next year (student-year panel)
#   Arm 3 RECRUITMENT: place-year real LSF -> first-year starter counts
#                      (a clean null once provider or region FE are applied)
#
# Moved from scripts/08_hazard_and_recruitment.r, ANALYSIS HALF ONLY. The six
# slide blocks stay behind for task 6 to move into the deck layer; every number
# they draw is already persisted here as a CSV, which is why the split is
# possible at all.
#
# Kept as ONE function rather than the separate analysis_hazard() and
# analysis_recruitment() the plan names, because both arms are built off the
# same panel and place-year tables. Splitting them means either duplicating
# that build or persisting an intermediate, which is a behaviour change and
# belongs in the one-real-value-build task after the merge, not here.
#
# Changes, and only these: constants from scripts/00_config.r (GRID_N, 50L in
# this script, is now GRID_N_HAZARD since 07 used the same name for 60L); two
# em dashes in section comments converted to ASCII; the function wrapper and
# two spaces of indentation. No statistical code is touched.
#
# Writes to outputs_dir()/real_value_hazard_recruit/:
#   tbl_hazard_1k.csv                 OR per 1k less, by spec (HARNESS-PINNED)
#   tbl_hazard_pred_curves.csv        predicted-probability curves for the deck
#   tbl_hazard_pp_1k.csv              percentage-point effect around the mean
#   tbl_recruit_provider_year.csv     provider-year cells
#   tbl_recruit_within_provider_var.csv  within-provider variation diagnostic
#   tbl_recruit_fe_results.csv        Arm 3 FE results (HARNESS-PINNED)
#   tbl_recruit_first_diff.csv        first-difference cells
#   00_README.txt
# ===========================================================================

analysis_hazard_recruitment <- function() {
  set.seed(1)
  
  # ---- CONFIG (constants live in scripts/00_config.r) ------------------------

  out <- file.path(outputs_dir(), "real_value_hazard_recruit")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)

  teal   <- dcol("dhsc_teal", "#01A188")
  blue   <- dcol("dhsc_blue", "#0063BE")
  orange <- dcol("af_orange", "#F46A25")
  grey   <- dcol("midgrey", "#6F777B")
  ink    <- dcol("ink", "#0B0C0C")

  wrap_title <- function(x, w = 52) stringr::str_wrap(x, width = w)
  wrap_sub   <- function(x, w = 95) stringr::str_wrap(x, width = w)
  wrapcap    <- function(x, w = 118) stringr::str_wrap(x, width = w)
  theme_comms <- function(base = 14) {
    theme_dhsc_slide(base = base) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = base * 1.35, face = "bold", lineheight = 1.05,
                                  margin = ggplot2::margin(b = 6)),
        plot.subtitle = ggplot2::element_text(size = base * 0.92, colour = "grey30", lineheight = 1.12,
                                     margin = ggplot2::margin(b = 10)),
        plot.caption = ggplot2::element_text(size = base * 0.72, colour = "grey45", hjust = 0,
                                    lineheight = 1.1, margin = ggplot2::margin(t = 8)),
        plot.margin = ggplot2::margin(14, 20, 12, 16),
        legend.position = "top"
      )
  }
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
  caption_y_zoom <- function(y_lo, extra = "") {
    base <- paste0(
      "Source: NHS LSF panel 2020-2026, DHSC analysis. Real LSF = ", PRIMARY_LBL, ". ",
      "Associational, not causal. ", extra
    )
    base <- wrapcap(base)
    if (!is.na(y_lo) && y_lo > 0.005)
      paste0(base, "\nNote: y-axis does not start at 0% (scale zoomed to show the gradient).")
    else base
  }

  or_from_b <- function(b, se) {
    tibble(OR = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))
  }
  pound_effect <- function(b, se, pounds_less = 1000) {
    bb <- -pounds_less * b
    ss <- pounds_less * se
    or <- exp(bb)
    tibble(
      pounds_less = pounds_less,
      OR_if_reduced = or,
      lo = exp(bb - 1.96 * ss),
      hi = exp(bb + 1.96 * ss),
      pct_higher_odds = 100 * (or - 1)
    )
  }
  coef_row <- function(m, term) {
    if (is.null(m)) return(tibble(term = term, b = NA_real_, se = NA_real_, p = NA_real_))
    ct <- tryCatch(as.data.frame(fixest::coeftable(m)), error = function(e) NULL)
    if (is.null(ct) || !nrow(ct)) return(tibble(term = term, b = NA_real_, se = NA_real_, p = NA_real_))
    rn <- rownames(ct)
    row <- c(term, paste0(term, "TRUE"))[c(term, paste0(term, "TRUE")) %in% rn][1]
    if (is.na(row)) {
      hit <- which(startsWith(rn, term))
      if (length(hit)) row <- rn[hit[1]] else
        return(tibble(term = term, b = NA_real_, se = NA_real_, p = NA_real_))
    }
    pcol <- intersect(c("Pr(>|z|)", "Pr(>|t|)"), names(ct))
    tibble(term = term, b = ct[row, "Estimate"], se = ct[row, "Std. Error"],
           p = if (length(pcol)) ct[row, pcol[1]] else NA_real_)
  }

  # defensive I/O: a locked CSV (e.g. open in Excel) or a single failed render
  # must NOT abort the run and swallow every slide after it. Warn and continue.
  safe_write <- function(x, f) tryCatch(write_csv(x, f),
    error = function(e) message("!! write skipped [", basename(f), "]: ", conditionMessage(e)))
  safe_slide <- function(p, f) tryCatch(save_slide(p, f),
    error = function(e) message("!! slide skipped [", basename(f), "]: ", conditionMessage(e)))

  # ===========================================================================
  # LOAD + BUILD PANEL
  # ===========================================================================
  progress("08: load data ...")
  SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

  long_path <- file.path(derived_dir(), "lsf_panel_long_2020_2026.csv")
  traj_path <- file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv")
  if (!file.exists(long_path)) stop("Need long panel: run 01 first.")

  long <- read_csv(
    long_path,
    col_select = c(UniqueID, year, first_year, course, college, confidence, leave_course),
    show_col_types = FALSE
  ) |>
    mutate(
      year = as.integer(year),
      first_year = as.logical(first_year),
      confidence = suppressWarnings(as.integer(confidence)),
      leave_course = to_01(leave_course)
    )

  if (file.exists(traj_path)) {
    tr <- read_csv(traj_path, show_col_types = FALSE) |>
      select(UniqueID, any_of(c("last_wave", "course_first_year_wave", "expected_finish",
                                "n_waves", "first_wave")))
  } else {
    tr <- long |>
      group_by(UniqueID) |>
      summarise(
        first_wave = min(year, na.rm = TRUE),
        last_wave  = max(year, na.rm = TRUE),
        n_waves = n(), .groups = "drop"
      ) |>
      mutate(course_first_year_wave = first_wave, expected_finish = NA_integer_)
  }
  long <- long |> left_join(tr, by = "UniqueID")

  entry_col <- long |>
    filter(first_year %in% TRUE | year == course_first_year_wave) |>
    group_by(UniqueID) |>
    slice_min(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(UniqueID, college_entry = college, entry_year = year,
              course_entry = course)

  long <- long |>
    left_join(entry_col, by = "UniqueID") |>
    mutate(
      college_use = dplyr::coalesce(college, college_entry),
      course_use  = dplyr::coalesce(course, course_entry),
      at_risk = !is.na(expected_finish) & year < expected_finish & year <= 2024L,
      left_next = as.integer(at_risk & year == last_wave)
    )

  # Wave-specific real LSF (full package flags from analysis sample if available)
  if (!"parental" %in% names(SAMPLE)) SAMPLE$parental <- FALSE
  if (!"specialist" %in% names(SAMPLE)) SAMPLE$specialist <- FALSE
  flags <- SAMPLE |>
    transmute(
      UniqueID,
      parental = to_01(parental),
      specialist = to_01(specialist)
    )

  wave_samp <- long |>
    left_join(flags, by = "UniqueID") |>
    transmute(
      UniqueID, year,
      college = college_use,
      entry_year = year,   # join year for costs
      course = course_use,
      parental = coalesce(parental, 0L),
      specialist = coalesce(specialist, 0L)
    ) |>
    distinct()

  wave_rv <- build_real_value(
    as.data.frame(wave_samp), ref, awards, cpih, base_year = 2020,
    provider_col = "college", year_col = "entry_year",
    .label = "08 wave hazard"
  ) |>
    transmute(
      UniqueID, year,
      rv_gbp = as.numeric(.data[[PRIMARY]]),
      region = region,
      lad_code = lad_code,
      nominal = nominal
    )

  panel <- long |>
    left_join(wave_rv, by = c("UniqueID", "year")) |>
    filter(!is.na(rv_gbp))

  # Place-year real LSF for recruitment: core £5k deflated by the SAME weighted
  # cost-of-living index as arms 1-2 (housing weight = HOUSING_WEIGHT, set in
  # functions/real_value.r). No student package mix (place-level, not per-student).
  # Uses the g1 building blocks gen_rel + rent_rel_ttwa; falls back to the legacy
  # product columns only if an older reference build lacks them.
  w_place <- if (exists("HOUSING_WEIGHT")) HOUSING_WEIGHT else 0.5
  if (all(c("gen_rel", "rent_rel_ttwa") %in% names(ref))) {
    place_year_rv <- ref |>
      distinct(provider, year, region, lad_code, gen_rel, rent_rel_ttwa) |>
      mutate(
        year = as.integer(year),
        rv_place = 5000 / (w_place * rent_rel_ttwa + (1 - w_place) * gen_rel)
      )
  } else if ("real_value_rent_ttwa_cpih_gbp" %in% names(ref)) {
    place_year_rv <- ref |>
      distinct(provider, year, region, lad_code,
               rv_place = real_value_rent_ttwa_cpih_gbp) |>
      mutate(year = as.integer(year))
  } else {
    place_year_rv <- ref |>
      distinct(provider, year, region, lad_code, infl_factor, rent_factor_ttwa) |>
      mutate(
        year = as.integer(year),
        rv_place = 5000 * infl_factor * rent_factor_ttwa
      )
  }

  # ===========================================================================
  # ARM 2 - HAZARD
  # ===========================================================================
  progress("08: arm 2 hazard models ...")

  d_haz <- panel |>
    filter(at_risk %in% TRUE, !is.na(left_next), !is.na(rv_gbp),
           !is.na(course_use), !is.na(year)) |>
    mutate(
      course = factor(course_use),
      year_f = factor(year),
      left_next = as.integer(left_next)
    )

  # Odds / £1k
  m_haz0 <- tryCatch(
    feglm(left_next ~ rv_gbp, family = binomial, data = d_haz, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  m_haz1 <- tryCatch(
    feglm(left_next ~ rv_gbp | course + year_f, family = binomial, data = d_haz,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  haz_tbl <- bind_rows(
    {
      cr <- coef_row(m_haz0, "rv_gbp"); pe <- pound_effect(cr$b, cr$se, 1000)
      tibble(spec = "S0: real LSF only", n = nrow(d_haz), b = cr$b, se = cr$se, p = cr$p,
             OR_1k_less = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
             pct = pe$pct_higher_odds)
    },
    {
      cr <- coef_row(m_haz1, "rv_gbp"); pe <- pound_effect(cr$b, cr$se, 1000)
      tibble(spec = "S1: course + year FE", n = nrow(d_haz), b = cr$b, se = cr$se, p = cr$p,
             OR_1k_less = pe$OR_if_reduced, lo = pe$lo, hi = pe$hi,
             pct = pe$pct_higher_odds)
    }
  )
  write_csv(haz_tbl, file.path(out, "tbl_hazard_1k.csv"))
  message("Hazard £1k less -> leave next year:")
  print(as.data.frame(haz_tbl))

  # Predicted probability curve (glm for se.fit on typical profile)
  fit_logit <- function(data, y, rhs) {
    suppressWarnings(glm(as.formula(paste0(y, " ~ ", rhs)), data = data, family = binomial()))
  }
  as_year_num <- function(x) {
    if (is.factor(x)) suppressWarnings(as.integer(as.character(x))) else as.integer(x)
  }
  med_year_of <- function(x) {
    y <- as_year_num(x); y <- y[!is.na(y)]
    if (!length(y)) return(2022L)
    as.integer(median(y))
  }

  d_haz_m <- d_haz |>
    mutate(course = factor(course_use), year = factor(year))

  rv_lo <- as.numeric(quantile(d_haz_m$rv_gbp, 0.05, na.rm = TRUE))
  rv_hi <- as.numeric(quantile(d_haz_m$rv_gbp, 0.95, na.rm = TRUE))
  rv_grid <- seq(rv_lo, rv_hi, length.out = GRID_N_HAZARD)

  pred_haz_curve <- function(rhs, lab) {
    m <- fit_logit(d_haz_m, "left_next", rhs)
    course_lv <- if ("course" %in% names(m$xlevels)) m$xlevels$course else levels(d_haz_m$course)
    year_lv   <- if ("year" %in% names(m$xlevels)) m$xlevels$year else levels(d_haz_m$year)
    course_use <- names(sort(table(d_haz_m$course), decreasing = TRUE))[1]
    year_use <- as.character(med_year_of(d_haz_m$year))
    if (!course_use %in% course_lv) course_use <- course_lv[1]
    if (!year_use %in% year_lv) year_use <- year_lv[1]
    base <- data.frame(
      rv_gbp = rv_grid,
      course = factor(course_use, levels = course_lv),
      year = factor(year_use, levels = year_lv)
    )
    pr <- predict(m, newdata = base, type = "link", se.fit = TRUE)
    tibble(
      spec = lab, rv_gbp = rv_grid,
      p = plogis(pr$fit),
      lo = plogis(pr$fit - 1.96 * pr$se.fit),
      hi = plogis(pr$fit + 1.96 * pr$se.fit),
      n = nrow(m$model)
    )
  }

  haz_curves <- bind_rows(
    pred_haz_curve("rv_gbp", "S0: real LSF only"),
    pred_haz_curve("rv_gbp + course + year", "S1: course + year FE")
  )
  write_csv(haz_curves, file.path(out, "tbl_hazard_pred_curves.csv"))

  # pp if £1k less around mean
  pp_haz <- haz_curves |>
    group_by(spec) |>
    group_modify(function(d, ...) {
      mu <- mean(d$rv_gbp)
      p0 <- approx(d$rv_gbp, d$p, xout = mu, rule = 2)$y
      p1 <- approx(d$rv_gbp, d$p, xout = mu - 1000, rule = 2)$y
      tibble(at_mean = mu, p_mean = p0, p_minus_1k = p1, pp = 100 * (p1 - p0), n = d$n[1])
    }) |>
    ungroup()
  write_csv(pp_haz, file.path(out, "tbl_hazard_pp_1k.csv"))

  # ===========================================================================
  # ARM 3 - RECRUITMENT (provider x year first-year counts)
  # ===========================================================================
  progress("08: arm 3 recruitment (provider x year) ...")

  # Match first-year rows to provider names via same alias path as real_value
  fy <- long |>
    filter(first_year %in% TRUE, !is.na(college_use), !is.na(year)) |>
    filter(!year %in% DROP_YEARS) |>
    transmute(UniqueID, year, college = college_use, course = course_use)

  # Normalise college to reference provider for join
  fy <- fy |>
    mutate(
      .resolved = dplyr::coalesce(unname(PROVIDER_ALIASES[college]), college),
      .key = rv_norm(.resolved)
    )
  ref_u <- place_year_rv |>
    mutate(.key = rv_norm(provider)) |>
    distinct(.key, provider, year, region, rv_place)

  cells <- fy |>
    left_join(ref_u, by = c(".key", "year")) |>
    filter(!is.na(rv_place), !is.na(provider)) |>
    group_by(provider, year, region, rv_place) |>
    summarise(n_starters = n_distinct(UniqueID), .groups = "drop")

  # Keep viable panel cells
  cells <- cells |>
    group_by(provider) |>
    filter(n() >= MIN_PROVIDER_YEARS, mean(n_starters) >= MIN_PROVIDER_N / 2) |>
    ungroup() |>
    filter(n_starters >= 10L) |>  # drop tiny noisy cells
    mutate(
      log_n = log(n_starters),
      provider_f = factor(provider),
      year_f = factor(year),
      region_f = factor(region)
    )

  safe_write(cells, file.path(out, "tbl_recruit_provider_year.csv"))

  # Within-provider variation in real LSF (critical for FE)
  var_diag <- cells |>
    group_by(provider) |>
    summarise(
      n_years = n(),
      mean_n = mean(n_starters),
      sd_rv = sd(rv_place),
      range_rv = max(rv_place) - min(rv_place),
      .groups = "drop"
    )
  safe_write(var_diag, file.path(out, "tbl_recruit_within_provider_var.csv"))

  message(sprintf(
    "Recruitment cells: %s provider-years, %s providers. Median within-provider SD(real LSF)=£%.0f; median range=£%.0f",
    nrow(cells), n_distinct(cells$provider),
    median(var_diag$sd_rv, na.rm = TRUE),
    median(var_diag$range_rv, na.rm = TRUE)
  ))

  # FE models: log starters ~ real LSF
  m_rec0 <- tryCatch(
    feols(log_n ~ rv_place, data = cells, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  m_rec1 <- tryCatch(
    feols(log_n ~ rv_place | provider_f + year_f, data = cells, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  m_rec_reg <- tryCatch(
    feols(log_n ~ rv_place | region_f + year_f, data = cells, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  rec_row <- function(m, lab) {
    cr <- coef_row(m, "rv_place")
    # effect of £1000 LESS real LSF on log n -> multiply n by exp(-1000*b)
    mult <- exp(-1000 * cr$b)
    tibble(
      spec = lab,
      n_cells = if (!is.null(m)) stats::nobs(m) else NA_integer_,
      b = cr$b, se = cr$se, p = cr$p,
      mult_starters_if_1k_less = mult,
      pct_change_starters_if_1k_less = 100 * (mult - 1)
    )
  }
  rec_tbl <- bind_rows(
    rec_row(m_rec0, "S0: no FE"),
    rec_row(m_rec1, "S1: provider + year FE (preferred)"),
    rec_row(m_rec_reg, "S2: region + year FE")
  )
  safe_write(rec_tbl, file.path(out, "tbl_recruit_fe_results.csv"))
  message("Recruitment: % change in first-year counts if real LSF £1k lower:")
  print(as.data.frame(rec_tbl))

  # First-difference style scatter for intuition: within-provider change
  cells_ord <- cells |>
    arrange(provider, year) |>
    group_by(provider) |>
    mutate(
      d_log_n = log_n - dplyr::lag(log_n),
      d_rv = rv_place - dplyr::lag(rv_place)
    ) |>
    ungroup() |>
    filter(!is.na(d_log_n), !is.na(d_rv))

  safe_write(cells_ord, file.path(out, "tbl_recruit_first_diff.csv"))

  # ===========================================================================
  # CONSOLE + README
  # ===========================================================================
  readme <- c(
    "Real LSF hazard + recruitment outputs",
    paste("Generated:", as.character(Sys.time())),
    "",
    "Arm 1 clarity: slide_rv_three_arms.png",
    "Arm 2 hazard:  slide_rv_hazard_leave_next.png, slide_rv_hazard_controls.png",
    "Arm 3 recruit: slide_rv_recruit_within_var.png, slide_rv_recruit_first_diff.png,",
    "               slide_rv_recruit_fe_table.png",
    "",
    "Tables: tbl_hazard_*.csv, tbl_recruit_*.csv",
    "",
    "See docs/real_value_three_arms.md for design."
  )
  writeLines(readme, file.path(out, "00_README.txt"))

  cat("\n=== HAZARD: £1k less real LSF -> leave next year ===\n")
  print(as.data.frame(haz_tbl))
  cat("\n=== RECRUITMENT: % change starters if place real LSF £1k less ===\n")
  print(as.data.frame(rec_tbl))
  cat("\nWithin-provider real LSF: median SD £",
      round(median(var_diag$sd_rv, na.rm = TRUE)),
      ", median range £", round(median(var_diag$range_rv, na.rm = TRUE)), "\n", sep = "")
  cat("\nOutputs -> ", out, "\n", sep = "")

  invisible(list(out = out, hazard = haz_tbl, recruit = rec_tbl, var = var_diag))
}
