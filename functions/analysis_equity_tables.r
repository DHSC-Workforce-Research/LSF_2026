# ===========================================================================
# functions/analysis_equity_tables.r
#
# The equity (funding-triangle) tables. Two functions:
#
#   analysis_confidence_bands()  financial confidence banded by demographic
#                                group -> financial_confidence_by_band.csv
#   analysis_triangle_cross()    the group matrix across risk / dependence /
#                                precarity, plus their Spearman correlations
#
# analysis_confidence_bands() is taken from scripts/d2_analysis.r. The plan
# lists d2 as "exploratory PNGs, archived", which is only half true: d2 also
# writes financial_confidence_by_band.csv, which BOTH d3's confidence slides
# (deck 18 and 19) and d7's precarity measure read. Archiving d2 wholesale
# would have silently broken them. The table half moves here; the two
# exploratory PNGs stay in d2 for task 7 to archive.
#
# analysis_triangle_cross() is the analysis half of
# scripts/d7_triangle_cross_question.r; its heatmap and scatter slides stay
# behind for task 6.
#
# Writes to derived_dir():
#   financial_confidence_by_band.csv  (deck slides 18-19)
#   triangle_group_matrix.csv         (deck slide 20)
#   triangle_correlations.csv
# ===========================================================================

analysis_confidence_bands <- function() {
  long <- tidy_groups(read_csv(file.path(derived_dir(), "financial_confidence_long.csv"),
                               show_col_types = FALSE))

  banded <- add_net(confidence_by_group(long))
  write_csv(banded, file.path(derived_dir(), "financial_confidence_by_band.csv"))

  # Ranked table, MOST unconfident first (the hardship signal)
  ranking <- banded[order(-banded$Unconfident_pct),
                    c("Demographic", "Group", "n",
                      "Unconfident_pct", "Neutral_pct", "Confident_pct")]
  cat("\n==== ranked by % unconfident (highest first) ====\n\n")
  print(as.data.frame(ranking), row.names = FALSE)

  invisible(banded)
}


analysis_triangle_cross <- function() {

  ink  <- dcol("ink", "#0B0C0C"); grey <- dcol("midgrey", "#6F777B")
  wrap_title <- function(x, w = 74)  str_wrap(x, width = w)
  wrap_sub   <- function(x, w = 116) str_wrap(x, width = w)
  wrapcap    <- function(x, w = 128) str_wrap(x, width = w)
  src <- paste(
    "Source: NHS Learning Support Fund demographic cross-tabs (BSA), DHSC analysis.",
    "GROUP-LEVEL (ecological) associations, not individual. leave_course and",
    "confidence are continuing (y2+) items; funding-influence items are first-year,",
    "so links assume stable group composition across waves. PNTS/non-response",
    "excluded; groups under n=10 suppressed.")

  slide_text_theme <- function(title_size = 20, sub_size = 12.5) theme(
    plot.title    = element_text(size = title_size, face = "bold", colour = ink,
                                 lineheight = 1.12, margin = margin(b = 6)),
    plot.subtitle = element_text(size = sub_size, colour = "grey30",
                                 lineheight = 1.18, margin = margin(b = 10)),
    plot.caption  = element_text(lineheight = 1.15),
    plot.margin   = margin(14, 22, 10, 14))

  demog_cols <- c(
    "Age" = "#12436D", "Gender" = "#28A197", "Ethnicity" = "#801650",
    "Religion" = "#F46A25", "Disability" = "#3D3D3D", "Marital status" = "#A285D1",
    "Sexual orientation" = "#6BACE4", "Trans" = "#00703C",
    "Pregnancy/maternity" = "#D4351C")

  # ---- funding triangle rates -> wide by group -------------------------------
  fr <- read_csv(file.path(derived_dir(), "funding_triangle_rates.csv"),
                 show_col_types = FALSE) |>
    filter(!is_pref, !suppressed)

  fw <- fr |>
    select(demog, Group, question_slug, pct, n) |>
    pivot_wider(names_from = question_slug, values_from = c(pct, n))

  # ---- optional: financial confidence (precarity) ----------------------------
  load_conf_long <- function(lp) {
    x <- read_csv(lp, show_col_types = FALSE) |> tidy_groups(drop_pnts = FALSE, min_n = 0)
    x |>
      filter(!is.na(count)) |>
      group_by(Demographic, Group) |>
      summarise(denom = sum(count), un = sum(count[response %in% c("1", "2")]),
                .groups = "drop") |>
      filter(!str_detect(str_to_lower(Group), "prefer not to say")) |>
      transmute(demog = Demographic, Group,
                unconf_pct = if_else(denom < 10, NA_real_, 100 * un / denom),
                n_unconf = denom)
  }
  load_conf <- function() {
    bp <- file.path(derived_dir(), "financial_confidence_by_band.csv")
    lp <- file.path(derived_dir(), "financial_confidence_long.csv")
    if (file.exists(bp)) {
      b <- read_csv(bp, show_col_types = FALSE)
      if (!"Unconfident_pct" %in% names(b)) return(load_conf_long(lp))
      b |>
        filter(!grepl("grand total|^total$", tolower(Group))) |>
        mutate(demog = short_demographic(Demographic), Group = relabel(Group, group_map)) |>
        filter(!str_detect(str_to_lower(Group), "prefer not to say|\\(blank\\)|^null$")) |>
        group_by(demog, Group) |>
        summarise(unconf_pct = mean(Unconfident_pct), n_unconf = sum(n), .groups = "drop")
    } else if (file.exists(lp)) load_conf_long(lp)
    else stop("no financial_confidence_* in derived_dir()")
  }
  conf <- tryCatch(load_conf(), error = function(e) {
    message("d7: confidence data unavailable (", conditionMessage(e), "); precarity omitted.")
    NULL
  })

  # ---- assemble group matrix -------------------------------------------------
  gw <- fw
  if (!is.null(conf)) gw <- left_join(gw, conf, by = c("demog", "Group"))

  gw <- gw |>
    mutate(risk      = pct_leave_course,
           dep_what  = pct_funding_influence_course,
           dep_where = pct_funding_influence_hei,
           precarity = if ("unconf_pct" %in% names(gw)) unconf_pct else NA_real_,
           n_size    = n_funding_influence_hei)

  meas <- c(risk = "Retention risk", dep_what = "Dependence (what)",
            dep_where = "Dependence (where)", precarity = "Precarity (unconfident)")
  meas <- meas[names(meas) %in% names(gw)]
  meas <- meas[vapply(names(meas), function(v) any(is.finite(gw[[v]])), logical(1))]

  write_csv(gw |> select(demog, Group, any_of(names(meas)),
                         starts_with("n_")), file.path(derived_dir(), "triangle_group_matrix.csv"))

  # ---- Spearman correlation matrix -------------------------------------------
  M <- as.matrix(gw[, names(meas), drop = FALSE])
  C <- suppressWarnings(cor(M, method = "spearman", use = "pairwise.complete.obs"))
  colnames(C) <- rownames(C) <- unname(meas)
  message("\n-- Spearman rank correlations across demographic groups --")
  print(round(C, 2))
  write_csv(as.data.frame(C) |> tibble::rownames_to_column("measure"),
            file.path(derived_dir(), "triangle_correlations.csv"))


  progress("d7 tables done -> ", derived_dir())
  invisible(TRUE)
}
