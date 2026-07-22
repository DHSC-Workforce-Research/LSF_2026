# ===========================================================================
# scripts/d7_triangle_cross_question.r
#
# Cross-question, GROUP-LEVEL (ecological) view of the funding-decision
# triangle. Each demographic group is one observation carrying its rate on
# each lever:
#   risk       = leave_course % yes           (felt may leave)
#   dep_what   = funding_influence_course 4-5  (funding shaped WHAT to study)
#   dep_where  = funding_influence_hei 4-5      (funding shaped WHERE to study)
#   precarity  = financial confidence 1-2       (unconfident; if available)
#
# Question answered: do risk, dependence and precarity concentrate in the SAME
# groups (reinforcing disadvantage) or in different ones? Spearman rank
# correlations across groups, a heatmap, and two scatters.
#
# HARD LIMITS, printed on every output:
#  - ECOLOGICAL: group-level, not individual. A group correlation is NOT an
#    individual one (ecological fallacy).
#  - WAVE MISMATCH: leave_course and confidence are continuing (y2+) items;
#    the funding-influence items are first-year. Cross-item links assume stable
#    group composition across waves. No year dimension in these cross-tabs.
#
# Prereq: d5 (funding_triangle_rates.csv). Confidence is optional.
#   source("scripts/d7_triangle_cross_question.r", encoding = "UTF-8")
# Outputs -> derived_dir()/triangle_group_matrix.csv, triangle_correlations.csv
#            outputs_dir()/slide_triangle_{corr,scatter_risk,scatter_precarity}.png
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2); library(stringr)
})

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

# ---- heatmap slide ---------------------------------------------------------
cl <- as.data.frame(as.table(C)) |> setNames(c("x", "y", "rho"))
cl$x <- factor(cl$x, levels = unname(meas)); cl$y <- factor(cl$y, levels = rev(unname(meas)))
hm <- ggplot(cl, aes(x, y, fill = rho)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.2f", rho)), colour = ink, size = 5) +
  scale_fill_gradient2(low = "#D4351C", mid = "white", high = "#12436D",
                       midpoint = 0, limits = c(-1, 1), name = "Spearman") +
  coord_equal() +
  labs(title = wrap_title("Do the funding-decision levers concentrate in the same groups?"),
       subtitle = wrap_sub(sprintf(
         "Rank correlation of demographic-group rates across %d groups. Positive = groups high on one lever tend to be high on the other. Ecological; see note.",
         nrow(gw))),
       x = NULL, y = NULL, caption = wrapcap(src)) +
  theme_dhsc_slide(14) + slide_text_theme() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 20, hjust = 1))
save_slide(hm, file.path(outputs_dir(), "slide_triangle_corr_heatmap.png"))

# ---- scatter helper --------------------------------------------------------
scatter_slide <- function(d, xv, yv, xlab, ylab, title, file) {
  d <- d[is.finite(d[[xv]]) & is.finite(d[[yv]]), , drop = FALSE]
  if (nrow(d) < 5) { message("d7: too few groups for ", file, " - skipped"); return(invisible()) }
  rho <- suppressWarnings(cor(d[[xv]], d[[yv]], method = "spearman"))
  p <- ggplot(d, aes(.data[[xv]], .data[[yv]])) +
    geom_smooth(method = "lm", se = FALSE, colour = grey, linewidth = 0.5, linetype = "dashed") +
    geom_point(aes(size = n_size, colour = demog), alpha = 0.85) +
    geom_text(aes(label = Group), size = 3, colour = ink, vjust = -0.9, check_overlap = TRUE) +
    scale_size(range = c(2, 9), guide = "none") +
    scale_colour_manual(values = demog_cols, name = NULL) +
    labs(title = wrap_title(title),
         subtitle = wrap_sub(sprintf("Each point a demographic group (size = n). Spearman rho = %.2f across %d groups. Ecological; wave-mismatched (see note).", rho, nrow(d))),
         x = xlab, y = ylab, caption = wrapcap(src)) +
    theme_dhsc_slide(14) + slide_text_theme() +
    theme(legend.position = "bottom")
  save_slide(p, file.path(outputs_dir(), file))
}

# risk vs dependence-where: are at-risk groups also funding-driven in choice?
scatter_slide(gw, "risk", "dep_where",
  "Felt may leave course (%)", "Funding important to WHERE to study (4-5, %)",
  "Retention risk vs funding-driven choice, across groups",
  "slide_triangle_scatter_risk_dependence.png")

# precarity vs dependence-where: the decoupling (Chinese case)
if ("precarity" %in% names(meas))
  scatter_slide(gw, "precarity", "dep_where",
    "Unconfident about living costs (rated 1-2, %)", "Funding important to WHERE to study (4-5, %)",
    "Precarity and funding-driven choice are decoupled across groups",
    "slide_triangle_scatter_precarity_dependence.png")

progress("d7: done -> ", outputs_dir())