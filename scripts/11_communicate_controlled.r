# ===========================================================================
# 11_communicate_controlled.r  -  forest charts showing each funding-dependence
#   odds ratio BEFORE and AFTER controlling for financial confidence. Each
#   predictor in its own logistic model (+ course/cohort FE), both specs on the
#   same confidence-available sample. Visualises the 08 result.
# ===========================================================================
library(dplyr); library(readr); library(tidyr); library(purrr); library(ggplot2)
purrr::walk(list.files("functions", full.names = TRUE), source)   # codebook + theme_dhsc

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
sample <- build_funding_leaving_sample(long, traj) |> define_leaving_outcomes()
conf_cw <- long |> filter(first_year == FALSE, !is.na(confidence)) |>
  arrange(UniqueID, year) |> group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |> select(UniqueID, confidence)
sample <- left_join(sample, conf_cw, by = "UniqueID")

# both specs fit on the confidence-available sample, so only the regressor moves
base_cc <- sample |> filter(!is.na(confidence), !is.na(course), !is.na(entry_year))
fe <- "factor(course) + factor(entry_year)"

preds <- tibble::tribble(
  ~var,                ~term,
  "fund_availability", "fund_availability",
  "grant_influence",   "grant_influence",
  "funding_imp_crse",  "I(funding_imp_crse >= 4)",
  "funding_imp_uni",   "I(funding_imp_uni >= 4)",
  "grant_helps_stay",  "grant_helps_stay")
outs <- tibble::tribble(
  ~var,                   ~label,
  "considered_leaving",   "Considered leaving",
  "left_after_year1",     "Dropped after year 1",
  "left_2y_plus_early",   "Left 2+ years early",
  "left_final_year_only", "Stopped in final year",
  "left_before_finish",   "Left before finish")
# NB one_wave_only / considered_first dropped: no variation among year-2 survivors (the confidence sample)

fit_focal <- function(out, term, adjust) {
  rhs <- paste(term, "+", fe); if (adjust) rhs <- paste(rhs, "+ factor(confidence)")
  m  <- tryCatch(glm(as.formula(paste(out, "~", rhs)), family = binomial, data = base_cc), error = function(e) NULL)
  if (is.null(m)) return(tibble(OR = NA, lo = NA, hi = NA))
  co <- coef(summary(m)); focal <- paste0(term, "TRUE")
  if (!focal %in% rownames(co)) return(tibble(OR = NA, lo = NA, hi = NA))
  e <- co[focal, "Estimate"]; s <- co[focal, "Std. Error"]
  tibble(OR = exp(e), lo = exp(e - 1.96 * s), hi = exp(e + 1.96 * s))
}

progress("fitting unadjusted + adjusted models ...")
res <- crossing(out = outs$var, var = preds$var, adjust = c(FALSE, TRUE)) |>
  left_join(preds, by = "var") |>
  mutate(fit = pmap(list(out, term, adjust), \(o, t, a) { progress("  ", o, " / ", t, if (a) " +conf" else ""); fit_focal(o, t, a) })) |>
  unnest(fit) |>
  filter(!is.na(OR)) |>
  mutate(model      = factor(if_else(adjust, "+ financial confidence", "Unadjusted (+ FE)"),
                             levels = c("Unadjusted (+ FE)", "+ financial confidence")),
         pred_label = factor(lsf_lab(var), levels = rev(lsf_lab(preds$var))),
         out_label  = factor(outs$label[match(out, outs$var)], levels = outs$label))
write_csv(res, file.path(outputs_dir(), "lsf_grid_controlled_long.csv"))

or_breaks <- c(0.7, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5)
mcols <- c("Unadjusted (+ FE)" = dhsc_cols[["midgrey"]], "+ financial confidence" = dhsc_cols[["dhsc_blue"]])
dodge <- position_dodge(width = 0.55)

# (a) HEADLINE: considered leaving, before vs after ------------------------
hd <- res |> filter(out == "considered_leaving")
p_head <- ggplot(hd, aes(OR, pred_label, colour = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dhsc_cols[["midgrey"]]) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .18, linewidth = .5, position = dodge) +
  geom_point(size = 3, position = dodge) +
  geom_text(aes(label = sprintf("%.2f", OR)), position = dodge, hjust = -0.25, size = 2.8, show.legend = FALSE) +
  scale_x_log10(breaks = or_breaks, expand = expansion(mult = c(0.05, 0.12))) +
  scale_colour_manual(values = mcols) +
  labs(title = "Funding dependence and considering leaving, before and after controlling for financial confidence",
       subtitle = "Each predictor in its own logistic model with course and cohort fixed effects, on the confidence-answering sample.",
       x = "Odds ratio  (log scale, right of 1 = more likely)", y = NULL, colour = NULL,
       caption = "Source: NHS Learning Support Fund longitudinal panel 2020-2026. DHSC analysis.") +
  theme_dhsc()
save_dhsc(p_head, file.path(outputs_dir(), "lsf_controlled_headline.png"), width = 10, height = 5.5)

# (b) GRID: every outcome, before vs after ---------------------------------
xr <- range(c(res$lo, res$hi), na.rm = TRUE)
p_grid <- ggplot(res, aes(OR, pred_label, colour = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dhsc_cols[["midgrey"]]) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .45, position = dodge) +
  geom_point(size = 2, position = dodge) +
  scale_x_log10(limits = xr, breaks = or_breaks) +
  scale_colour_manual(values = mcols) +
  facet_wrap(~out_label, ncol = 3, scales = "free_x") +
  labs(title = "Does controlling for financial confidence shrink the funding-dependence effect?",
       subtitle = "Odds ratios before (grey) and after (blue) adding financial confidence. Each predictor its own model, course and cohort FE.",
       x = "Odds ratio  (log scale, right of 1 = more likely)", y = NULL, colour = NULL,
       caption = "Source: NHS Learning Support Fund longitudinal panel 2020-2026. DHSC analysis.") +
  theme_dhsc() + theme(panel.spacing = grid::unit(1, "lines"))
save_dhsc(p_grid, file.path(outputs_dir(), "lsf_controlled_grid.png"), width = 13, height = 8)
progress("done -> ", outputs_dir())