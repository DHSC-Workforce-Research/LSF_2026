# ===========================================================================
# functions/deck_builders_technical.r
#
# Builders for the technical annex: how the geography is built, how real value
# is calculated, and what the models actually estimate. These slides fit no
# models and read no result tables. Their content is the method, so it is
# fixed text and diagram, drawn in pure ggplot2 like dhsc_table_plot().
#
# Every string is in functions/deck_text.r, keyed by the manifest slug, so
# these builders hold geometry only.
# ===========================================================================

# ---- shared flow-diagram primitives ---------------------------------------

# a rounded-ish box with a heading and up to three body lines
tech_box <- function(x, y, w, h, head, body = NULL, fill = "#FFFFFF",
                     border = "#12436D", head_col = "#12436D",
                     head_size = 4.4, body_size = 3.5) {
  list(
    ggplot2::annotate("rect", xmin = x - w/2, xmax = x + w/2,
                      ymin = y - h/2, ymax = y + h/2,
                      fill = fill, colour = border, linewidth = 0.6),
    ggplot2::annotate("text", x = x, y = y + h/2 - 0.30, label = head,
                      colour = head_col, fontface = "bold", size = head_size,
                      lineheight = 0.95, vjust = 1),
    if (!is.null(body))
      ggplot2::annotate("text", x = x, y = y + h/2 - 0.95, label = body,
                        colour = "#3D3D3D", size = body_size,
                        lineheight = 1.12, vjust = 1)
  )
}

tech_arrow <- function(x1, y1, x2, y2, colour = "#6F777B") {
  ggplot2::annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
                    colour = colour, linewidth = 0.7,
                    arrow = grid::arrow(length = grid::unit(0.16, "cm"),
                                        type = "closed"))
}

tech_note <- function(x, y, label, size = 3.1, colour = "#6F777B", hjust = 0.5) {
  ggplot2::annotate("text", x = x, y = y, label = label, colour = colour,
                    size = size, hjust = hjust, lineheight = 1.15,
                    fontface = "italic")
}

# blank canvas with the house title block, sized in arbitrary units
tech_canvas <- function(slug, xlim, ylim,
                        w = get0("HOUSING_WEIGHT", ifnotfound = 0.5)) {
  list(
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE),
    ggplot2::labs(title    = lbl(slug, "title",    w = w),
                  subtitle = lbl(slug, "subtitle", w = w),
                  caption  = lbl(slug, "caption",  w = w)),
    ggplot2::theme_void(base_size = 15),
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 22,
                                            colour = "#0B0C0C",
                                            margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 14, colour = "#3D3D3D",
                                            lineheight = 1.15,
                                            margin = ggplot2::margin(b = 12)),
      plot.caption  = ggplot2::element_text(size = 10.5, colour = "#6F777B",
                                            hjust = 0, lineheight = 1.2),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(18, 22, 14, 20))
  )
}

# ===========================================================================
# T1. Where a student's cost of living comes from
# ===========================================================================
build_slide_tech_geography <- function(tables = NULL) {
  y_src  <- 8.4    # source row
  y_step <- 5.6    # transformation row
  y_out  <- 2.4    # output row

  g <- ggplot2::ggplot() + tech_canvas("tech_geography", c(0, 100), c(0, 10.6))

  # --- four sources across the top -----------------------------------------
  srcs <- list(
    list(x = 13, head = "Provider register",
         body = "JISC Learning Providers Plus\nUKPRN, name, postcode"),
    list(x = 38, head = "postcodes.io",
         body = "postcode to\nLAD, MSOA, LSOA, region"),
    list(x = 63, head = "ONS area lookup",
         body = "Output Area 2021 to\nTTWA 2011 to LAD 2022"),
    list(x = 88, head = "ONS PIPR",
         body = "average private rent\nby local authority, by year")
  )
  for (s in srcs)
    g <- g + tech_box(s$x, y_src, 21, 2.5, s$head, s$body, fill = "#F2F7FA")

  # --- the merge chain below -----------------------------------------------
  steps <- list(
    list(x = 13, head = "One postcode per provider",
         body = "the registered main address"),
    list(x = 38, head = "Provider to LAD",
         body = "geocoded; two dead register\npostcodes corrected by hand"),
    list(x = 63, head = "LAD to TTWA",
         body = "each LAD takes the TTWA\nholding most of its output areas"),
    list(x = 88, head = "TTWA rent per year",
         body = "output-area weighted mean\nof member LADs")
  )
  for (s in steps)
    g <- g + tech_box(s$x, y_step, 21, 2.5, s$head, s$body,
                      fill = "#FFFFFF", border = "#28A197", head_col = "#0B0C0C")

  # verticals source -> step
  for (s in srcs) g <- g + tech_arrow(s$x, y_src - 1.25, s$x, y_step + 1.25)
  # horizontals along the chain
  for (x in c(13, 38, 63)) g <- g + tech_arrow(x + 10.5, y_step, x + 14.5, y_step)

  # --- the output ----------------------------------------------------------
  g <- g +
    tech_arrow(88, y_step - 1.25, 88, y_out + 1.5) +
    ggplot2::annotate("rect", xmin = 26, xmax = 100, ymin = y_out - 1.5,
                      ymax = y_out + 1.5, fill = "#12436D", colour = NA) +
    ggplot2::annotate("text", x = 30, y = y_out + 0.75, hjust = 0,
                      label = "rent_rel_ttwa", colour = "white",
                      fontface = "bold", size = 5.2, family = "mono") +
    ggplot2::annotate("text", x = 30, y = y_out - 0.35, hjust = 0,
                      label = paste("rent in the travel-to-work area around the provider,",
                                    "\ndivided by the England average rent in 2020"),
                      colour = "white", size = 4.1, lineheight = 1.1) +
    # the caveat, stated where it cannot be missed
    ggplot2::annotate("rect", xmin = 0, xmax = 24, ymin = y_out - 1.5,
                      ymax = y_out + 1.5, fill = "#FBEAE7", colour = "#D4351C",
                      linewidth = 0.6) +
    ggplot2::annotate("text", x = 12, y = y_out + 0.75,
                      label = "This is the provider's\nlocation, never the student's",
                      colour = "#D4351C", fontface = "bold", size = 4,
                      lineheight = 1.05) +
    ggplot2::annotate("text", x = 12, y = y_out - 0.7,
                      label = "TTWA rather than council area\nbecause students rent across\na housing market, not a boundary",
                      colour = "#3D3D3D", size = 3.3, lineheight = 1.1)

  g
}

# a colour-keyed explanation card: coloured spine, bold heading, body text
tech_card <- function(x1, x2, y1, y2, accent, head, body,
                      head_size = 4.3, body_size = 3.8) {
  list(
    ggplot2::annotate("rect", xmin = x1, xmax = x2, ymin = y1, ymax = y2,
                      fill = "#F7F9FA", colour = NA),
    ggplot2::annotate("rect", xmin = x1, xmax = x1 + 0.7, ymin = y1, ymax = y2,
                      fill = accent, colour = NA),
    ggplot2::annotate("text", x = x1 + 2, y = y2 - 0.45, hjust = 0, vjust = 1,
                      label = head, fontface = "bold", size = head_size,
                      colour = accent),
    ggplot2::annotate("text", x = x1 + 2, y = y2 - 1.25, hjust = 0, vjust = 1,
                      label = body, size = body_size, colour = "#3D3D3D",
                      lineheight = 1.2)
  )
}

# ===========================================================================
# T2. How the frozen £5,000 becomes a real value

# Analysis Function palette. Literals rather than dcol(), because functions/ is
# sourced alphabetically and dhsc_cols does not exist yet at this point in the
# walk. These four must match dhsc_theme.r; tests/check_deck_text.r asserts it.
TECH_BLUE   <- "#12436D"
TECH_TEAL   <- "#28A197"
TECH_ORANGE <- "#F46A25"
TECH_RED    <- "#D4351C"

build_slide_tech_realvalue <- function(tables = NULL) {
  g <- ggplot2::ggplot() + tech_canvas("tech_realvalue", c(0, 100), c(0, 10.6))

  ey <- 8.6   # fraction bar

  g <- g +
    # ---- equation ----------------------------------------------------------
    ggplot2::annotate("text", x = 24, y = ey, hjust = 1, label = "real value  =",
                      size = 6.6, fontface = "bold", colour = "#0B0C0C") +
    ggplot2::annotate("text", x = 50, y = ey + 0.75, label = "nominal award",
                      size = 5.8, colour = TECH_BLUE, fontface = "bold") +
    ggplot2::annotate("segment", x = 26, xend = 74, y = ey + 0.15, yend = ey + 0.15,
                      colour = "#0B0C0C", linewidth = 0.8) +
    ggplot2::annotate("text", x = 27, y = ey - 0.55, hjust = 0,
                      label = "w \u00d7 (TTWA rent\u1d67 \u00f7 England rent\u2082\u2080\u2082\u2080)",
                      size = 5.0, colour = TECH_TEAL, fontface = "bold") +
    ggplot2::annotate("text", x = 53.5, y = ey - 0.55, hjust = 0, label = "+",
                      size = 5.0, colour = "#0B0C0C") +
    ggplot2::annotate("text", x = 55.8, y = ey - 0.55, hjust = 0,
                      label = "(1 \u2212 w) \u00d7 (CPI\u1d67 \u00f7 CPI\u2082\u2080\u2082\u2080)",
                      size = 5.0, colour = TECH_ORANGE, fontface = "bold") +

    # ---- three cards, colour-keyed to the equation -------------------------
    tech_card(2, 32, 2.7, 6.9, TECH_BLUE, "nominal award",
              paste0("£5,000 training grant, paid to\n",
                     "every eligible student\n",
                     "+ £2,000 parental support\n",
                     "+ £1,000 specialist subject\n\n",
                     "Frozen in cash since 2020.")) +
    tech_card(35, 65, 2.7, 6.9, TECH_TEAL, "housing leg",
              paste0("Local rent in the provider's travel-\n",
                     "to-work area, against the England\n",
                     "average in 2020. Carries place and\n",
                     "time at once.\n\n",
                     "w = 0.5, the housing share of a\n",
                     "student's budget (DfE SIES), tested\n",
                     "at 0.4 and 0.6.", collapse = "")) +
    tech_card(68, 98, 2.7, 6.9, TECH_ORANGE, "everything else",
              paste0("CPI series D7BT, which excludes\n",
                     "owner-occupier housing costs.\n\n",
                     "So housing enters the index once,\n",
                     "through local rent, at the weight\n",
                     "a student actually spends on it.")) +

    # ---- two footnotes -----------------------------------------------------
    ggplot2::annotate("rect", xmin = 2, xmax = 51, ymin = 0.5, ymax = 2.3,
                      fill = "#FBEAE7", colour = TECH_RED, linewidth = 0.5) +
    ggplot2::annotate("text", x = 4, y = 1.95, hjust = 0, vjust = 1,
                      label = "The first version multiplied a CPIH haircut by a rent haircut.",
                      fontface = "bold", size = 3.7, colour = TECH_RED) +
    ggplot2::annotate("text", x = 4, y = 1.30, hjust = 0, vjust = 1,
                      label = paste0("Housing is inside CPIH already, so it was counted twice and the erosion\n",
                                     "was overstated. The weighted index counts it once."),
                      size = 3.6, colour = "#3D3D3D", lineheight = 1.15) +
    ggplot2::annotate("text", x = 54, y = 1.95, hjust = 0, vjust = 1,
                      label = "No cap is applied.",
                      fontface = "bold", size = 3.7, colour = "#0B0C0C") +
    ggplot2::annotate("text", x = 54, y = 1.30, hjust = 0, vjust = 1,
                      label = paste0("Where an area was cheap in 2020 the grant stretches further and the\n",
                                     "measure is allowed to exceed face value, rather than being clipped to it."),
                      size = 3.6, colour = "#3D3D3D", lineheight = 1.15)

  g
}

# ===========================================================================
# T3. What the models estimate

# ---------------------------------------------------------------------------
# T3 (rebuilt): what the models estimate
# ---------------------------------------------------------------------------
build_slide_tech_model <- function(tables = NULL) {
  g <- ggplot2::ggplot() + tech_canvas("tech_model", c(0, 100), c(0, 10.6))

  ey <- 9.0
  g <- g +
    ggplot2::annotate("text", x = 2, y = ey, hjust = 0, size = 5.0,
                      fontface = "bold", colour = "#0B0C0C",
                      label = "logit  P(left before finishing)") +
    ggplot2::annotate("text", x = 33.5, y = ey, hjust = 0, size = 5.0,
                      colour = "#0B0C0C", label = "=") +
    ggplot2::annotate("text", x = 36, y = ey, hjust = 0, size = 5.0,
                      fontface = "bold", colour = TECH_BLUE,
                      label = "\u03b2 \u00d7 real value") +
    ggplot2::annotate("text", x = 50, y = ey, hjust = 0, size = 5.0,
                      fontface = "bold", colour = TECH_TEAL,
                      label = "+  controls") +
    ggplot2::annotate("text", x = 62.5, y = ey, hjust = 0, size = 5.0,
                      fontface = "bold", colour = TECH_ORANGE,
                      label = "+  course  +  entry year") +

    tech_card(2, 32, 4.25, 8.1, TECH_BLUE, "what is estimated",
              paste0("One row per student. Outcome is\n",
                     "leaving before the expected finish\n",
                     "year.\n\n",
                     "\u03b2 is reported as an odds ratio, per\n",
                     "£1,000 or per 1 SD of real value.")) +
    tech_card(35, 65, 4.25, 8.1, TECH_TEAL, "what is controlled",
              paste0("The five funding survey items, the\n",
                     "grant components, and financial\n",
                     "confidence, added one rung at a time\n",
                     "so you can see what each absorbs.")) +
    tech_card(68, 98, 4.25, 8.1, TECH_ORANGE, "what is absorbed",
              paste0("Course and entry-year fixed effects.\n\n",
                     "Every comparison is therefore between\n",
                     "students on the same course who\n",
                     "started in the same year. Differences\n",
                     "between courses do no work.")) +

    ggplot2::annotate("text", x = 2, y = 3.75, hjust = 0, fontface = "bold",
                      size = 4.3, colour = "#0B0C0C",
                      label = "The specification ladder")

  rungs <- list(c("S0", "real value only"),
                c("S1", "+ the five funding survey items"),
                c("S2", "+ grant components (parental, specialist, regional)"),
                c("S3", "+ financial confidence, among students who reach year 2"))
  for (i in seq_along(rungs)) {
    yy <- 2.95 - (i - 1) * 0.70
    g <- g +
      ggplot2::annotate("rect", xmin = 2, xmax = 7, ymin = yy - 0.26, ymax = yy + 0.26,
                        fill = TECH_BLUE, colour = NA) +
      ggplot2::annotate("text", x = 4.5, y = yy, label = rungs[[i]][1],
                        colour = "white", fontface = "bold", size = 3.7) +
      ggplot2::annotate("text", x = 8.5, y = yy, hjust = 0, label = rungs[[i]][2],
                        size = 3.8, colour = "#3D3D3D")
  }

  g +
    ggplot2::annotate("rect", xmin = 60, xmax = 98, ymin = 0.35, ymax = 3.85,
                      fill = "#FBEAE7", colour = TECH_RED, linewidth = 0.5) +
    ggplot2::annotate("text", x = 62, y = 3.5, hjust = 0, vjust = 1,
                      label = "What this cannot do", fontface = "bold",
                      size = 4.2, colour = TECH_RED) +
    ggplot2::annotate("text", x = 62, y = 2.75, hjust = 0, vjust = 1,
                      label = paste0(
                        "Every student in the data receives the grant, so there is no\n",
                        "unfunded group to compare against. Nothing here estimates\n",
                        "whether the LSF causes anyone to stay.\n\n",
                        "Fixed effects remove composition, not financial precarity,\n",
                        "which is unmeasured and plausibly drives both."),
                      size = 3.6, colour = "#3D3D3D", lineheight = 1.2)
}
