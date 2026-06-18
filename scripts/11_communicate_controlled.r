# ===========================================================================
# 11_communicate_controlled.r  -  funding-dependence odds ratios across THREE
#   specs: (1) all sample unadjusted, (2) year-2 sample unadjusted,
#   (3) year-2 sample + financial confidence. Gap 1->2 = survivorship,
#   gap 2->3 = confidence control. Each predictor in its own logistic model.
# ===========================================================================
library(dplyr); library(readr); library(tidyr); library(purrr); library(ggplot2)
purrr::walk(list.files("functions", full.names = TRUE), source)

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
sample <- build_funding_leaving_sample(long, traj) |> define_leaving_outcomes()
conf_cw <- long |> filter(first_year == FALSE, !is.na(confidence)) |>
  arrange(UniqueID, year) |> group_by(UniqueID) |> slice_head(n = 1) |> ungroup() |> select(UniqueID, confidence)
sample  <- left_join(sample, conf_cw, by = "UniqueID")
base_cc <- sample |> filter(!is.na(confidence), !is.na(course), !is.na(entry_year))   # year-2 (survivor) sample
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
combos <- crossing(out = outs$var, var = preds$var) |> left_join(preds, by = "var")

fit_focal <- function(data, out, term, adjust) {
  rhs <- paste(term, "+", fe); if (adjust) rhs <- paste(rhs, "+ factor(confidence)")
  m <- tryCatch(glm(as.formula(paste(out, "~", rhs)), family = binomial, data = data), error = function(e) NULL)
  if (is.null(m)) return(tibble(OR = NA_real_, lo = NA_real_, hi = NA_real_))
  co <- coef(summary(m)); fc <- paste0(term, "TRUE")
  if (!fc %in% rownames(co)) return(tibble(OR = NA_real_, lo = NA_real_, hi = NA_real_))
  e <- co[fc, "Estimate"]; s <- co[fc, "Std. Error"]
  tibble(OR = exp(e), lo = exp(e - 1.96 * s), hi = exp(e + 1.96 * s))
}
run_spec <- function(nm, data, adjust) combos |>
  mutate(fit = pmap(list(out, term), \(o, t) { progress("  ", nm, ": ", o, "/", t); fit_focal(data, o, t, adjust) })) |>
  unnest(fit) |> mutate(spec = nm)

spec_levels <- c("All sample, unadjusted", "Year-2 sample, unadjusted", "Year-2 sample, + fin. confidence")
progress("fitting three specs ...")
res <- bind_rows(
  run_spec(spec_levels[1], sample,  FALSE),
  run_spec(spec_levels[2], base_cc, FALSE),
  run_spec(spec_levels[3], base_cc, TRUE)) |>
  filter(!is.na(OR)) |>
  mutate(spec       = factor(spec, levels = spec_levels),
         pred_label = factor(lsf_lab(var), levels = rev(lsf_lab(preds$var))),
         out_label  = factor(outs$label[match(out, outs$var)], levels = outs$label))
write_csv(res, file.path(outputs_dir(), "lsf_grid_3spec_long.csv"))

mcols <- c("All sample, unadjusted"           = dhsc_cols[["midgrey"]],
           "Year-2 sample, unadjusted"        = dhsc_cols[["af_orange"]],
           "Year-2 sample, + fin. confidence" = dhsc_cols[["dhsc_blue"]])
or_breaks <- c(0.7, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5, 1.75)
dodge <- position_dodge(width = 0.7)

# (a) HEADLINE: left before finish, all three specs ------------------------
head_out <- "left_before_finish"                       # switch to "considered_leaving" if preferred
hd    <- res |> filter(out == head_out)
nf    <- nlevels(hd$pred_label)
xlim  <- c(min(hd$lo) * 0.95, max(hd$hi) * 1.30)
boxes <- tibble::tibble(y = seq_len(nf))

p_head <- ggplot(hd, aes(OR, pred_label, colour = spec)) +
  geom_rect(data = boxes, inherit.aes = FALSE,
            aes(ymin = y - 0.45, ymax = y + 0.45, xmin = xlim[1], xmax = xlim[2]),
            fill = NA, colour = dhsc_cols[["midgrey"]], linetype = "dashed", linewidth = .3) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dhsc_cols[["midgrey"]]) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .16, linewidth = .45, position = dodge) +
  geom_point(size = 2.6, position = dodge) +
  geom_text(aes(x = hi, label = sprintf("%.2f", OR)), position = dodge, hjust = -0.35, size = 2.7, show.legend = FALSE) +
  scale_x_log10(breaks = or_breaks, limits = xlim) +
  scale_colour_manual(values = mcols) +
  labs(title = paste0("Funding dependence and ", tolower(outs$label[match(head_out, outs$var)]),
                      ": survivorship vs financial-confidence control"),
       subtitle = "Grey -> orange shows the effect of restricting to year-2 survivors; orange -> blue shows controlling for financial confidence.",
       x = "Odds ratio  (log scale, right of 1 = more likely)", y = NULL, colour = NULL,
       caption = "Source: NHS Learning Support Fund longitudinal panel 2020-2026. DHSC analysis.") +
  theme_dhsc()
save_dhsc(p_head, file.path(outputs_dir(), "lsf_3spec_headline.png"), width = 10.5, height = 6)

# (b) GRID: every outcome, all three specs ---------------------------------
xr <- range(c(res$lo, res$hi), na.rm = TRUE)
p_grid <- ggplot(res, aes(OR, pred_label, colour = spec)) +
  geom_rect(data = boxes, inherit.aes = FALSE,
            aes(ymin = y - 0.45, ymax = y + 0.45, xmin = xr[1], xmax = xr[2]),
            fill = NA, colour = dhsc_cols[["gridgrey"]], linetype = "dashed", linewidth = .25) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = dhsc_cols[["midgrey"]]) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, linewidth = .4, position = dodge) +
  geom_point(size = 1.9, position = dodge) +
  scale_x_log10(limits = xr, breaks = or_breaks) +
  scale_colour_manual(values = mcols) +
  facet_wrap(~out_label, ncol = 3, scales = "free_x") +
  labs(title = "Funding dependence and leaving: survivorship and financial-confidence control, across definitions",
       subtitle = "All-sample (grey), year-2 survivors (orange), year-2 + financial confidence (blue). Each predictor its own model, course and cohort FE.",
       x = "Odds ratio  (log scale, right of 1 = more likely)", y = NULL, colour = NULL,
       caption = "Source: NHS Learning Support Fund longitudinal panel 2020-2026. DHSC analysis.") +
  theme_dhsc() + theme(panel.spacing = grid::unit(1, "lines"))
save_dhsc(p_grid, file.path(outputs_dir(), "lsf_3spec_grid.png"), width = 13, height = 8.5)
progress("done -> ", outputs_dir())