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
