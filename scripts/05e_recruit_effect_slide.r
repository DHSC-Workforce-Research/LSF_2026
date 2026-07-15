# ===========================================================================
# scripts/05e_recruit_effect_slide.r
#
# A clear graphic for the Arm 3 (recruitment) result - replaces the dire FE
# table. For each method, shows the % change in first-year LSF claimants if a
# place's real grant value were £1,000 LOWER, with 95% CI and a zero line, in
# plain English (no "S0/S1/S2", no "cells"). The story: the raw comparison looks
# big and negative, but it is confounded by big expensive cities vs small cheap
# towns; comparing each provider with itself over time, the effect crosses zero.
#
# ASCII ONLY (£ safe). Reads tbl_recruit_fe_results.csv (written by 08).
#   source("scripts/05e_recruit_effect_slide.r", encoding = "UTF-8")
# Outputs -> outputs_dir()/slide_rv_recruit_effect.png
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({ library(dplyr); library(readr); library(ggplot2); library(stringr) })
update_geom_defaults("text", list(family = "Arial"))

teal   <- dcol("dhsc_teal", "#01A188")
orange <- dcol("af_orange", "#F46A25")
grey   <- dcol("midgrey", "#6F777B")
ink    <- dcol("ink", "#0B0C0C")
wrap_title <- function(x, w = 56)  str_wrap(x, width = w)
wrap_sub   <- function(x, w = 100) str_wrap(x, width = w)
wrapcap    <- function(x, w = 128) str_wrap(x, width = w)

rdir <- file.path(outputs_dir(), "real_value_hazard_recruit")
fe   <- read_csv(file.path(rdir, "tbl_recruit_fe_results.csv"), show_col_types = FALSE)
cells <- tryCatch(read_csv(file.path(rdir, "tbl_recruit_provider_year.csv"), show_col_types = FALSE),
                  error = function(e) NULL)
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

p <- ggplot(d, aes(pct, lab, colour = crosses0)) +
  geom_vline(xintercept = 0, colour = grey, linewidth = 0.6) +
  geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = 0.16, linewidth = 1.1) +
  geom_point(size = 5) +
  geom_text(aes(label = sprintf("%+.0f%%", pct)), vjust = -1.2, size = 4.2, colour = ink) +
  scale_colour_manual(values = c(`TRUE` = grey, `FALSE` = orange), guide = "none") +
  scale_x_continuous(limits = c(xr[1] - pad, xr[2] + pad),
                     labels = function(z) paste0(z, "%")) +
  labs(
    title = wrap_title("Recruitment: no reliable link once you compare like with like"),
    subtitle = wrap_sub(paste0(
      "Estimated change in the number of first-year LSF claimants if a place's real grant value were ",
      "£1,000 lower. The raw comparison (orange) looks large and negative, but it just reflects big ",
      "expensive cities vs small cheap towns. Comparing each provider with itself over time, the range ",
      "crosses zero, so there is no reliable effect.")),
    x = "Change in first-year claimants per £1,000 lower real LSF  (dot = estimate, bar = 95% CI)",
    y = NULL,
    caption = wrapcap(paste0(
      "Source: NHS LSF panel",
      if (!is.na(n_py)) paste0(", ", format(n_py, big.mark = ","),
                               " provider-years across ", n_prov, " providers") else "",
      ". Grey = 95% range includes zero (no reliable effect); orange = raw, unadjusted (confounded by place)."))
  ) +
  theme_dhsc_slide(15) +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = margin(16, 22, 12, 14))

save_slide(p, file.path(outputs_dir(), "slide_rv_recruit_effect.png"))

cat("\nRecruitment effect (% change in first-year claimants per £1,000 less real LSF):\n")
print(as.data.frame(d |> transmute(method = spec, pct_change = round(pct, 1),
                                   ci_lo = round(pct_lo, 1), ci_hi = round(pct_hi, 1),
                                   includes_zero = crosses0)), row.names = FALSE)
