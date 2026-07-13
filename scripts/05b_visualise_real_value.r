# ===========================================================================
# scripts/05b_visualise_real_value.r
# Tables from 05_real_value_controlled -> DHSC widescreen slides.
# Run after 05. Finding text is plain (no slide_labels.json dependency).
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(stringr); library(tidyr)
})

out <- outputs_dir()
rd  <- function(f) read_csv(file.path(out, f), show_col_types = FALSE)
src <- "Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis. Associational models; course and entry-year fixed effects."
wrapcap <- function(x, w = 130) str_wrap(x, w)
teal <- dcol("dhsc_teal", "#01A188")
blue <- dcol("dhsc_blue", "#0063BE")
orange <- dcol("af_orange", "#F46A25")
grey <- dcol("midgrey", "#6F777B")
ink  <- dcol("ink", "#0B0C0C")
risk <- dcol("risk", "#D4351C")

# --- slide A: spec ladder forest (primary outcome, per 1 SD) ---------------
lad <- rd("tbl_rv_spec_ladder.csv") |>
  filter(outcome_var == "left_before_finish", scale == "per 1 SD",
         spec %in% c("S0", "S1", "S2", "S3")) |>
  mutate(
    spec_label = factor(spec_label, levels = rev(unique(spec_label[order(spec)]))),
    direction = ifelse(OR < 1, "Higher real value -> less leaving", "Higher real value -> more leaving")
  )

pA <- ggplot(lad, aes(OR, spec_label, colour = direction)) +
  geom_vline(xintercept = 1, colour = grey, linewidth = 0.35, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.55) +
  geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("%.3f", OR)), vjust = -1.05, size = 3.6, colour = ink, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Higher real value -> less leaving" = teal,
    "Higher real value -> more leaving" = risk
  )) +
  labs(
    title = "Does real LSF value still predict leaving once we control for survey answers?",
    subtitle = paste0(
      "Odds ratio per 1 SD of rent-adjusted (TTWA) real grant value. ",
      "S0 = real value only; S1 = + funding survey items; S2 = + grant components; ",
      "S3 = S1 + financial confidence (year-2 survivors)."
    ),
    x = "Odds ratio of leaving before finishing (1.0 = no association)",
    y = NULL, colour = NULL,
    caption = wrapcap(src)
  ) +
  theme_dhsc_slide(base = 14) +
  theme(legend.position = "top")
save_slide(pA, file.path(out, "slide_rv_spec_ladder.png"))

# --- slide B: joint horse race (S1 terms) ----------------------------------
jt <- rd("tbl_rv_joint_terms.csv") |>
  mutate(
    direction = ifelse(OR >= 1, "More likely to leave", "Less likely to leave"),
    label = factor(label, levels = label[order(OR)])
  )

pB <- ggplot(jt, aes(OR, label, colour = direction)) +
  geom_vline(xintercept = 1, colour = grey, linewidth = 0.35, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.55) +
  geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1.05, size = 3.6, colour = ink, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "More likely to leave" = risk,
    "Less likely to leave" = teal
  )) +
  labs(
    title = "Real value vs survey funding factors (same model)",
    subtitle = paste0(
      "All terms from one logistic model of leaving before finishing. ",
      "Real value is per 1 SD (continuous); survey items are yes/no. ",
      "Compare directions and whether real value remains after the survey controls."
    ),
    x = "Odds ratio (1.0 = no difference)", y = NULL, colour = NULL,
    caption = wrapcap(paste(
      "Note: continuous (per SD) and binary predictors are not on the same unit scale;",
      "use this chart for direction and whether effects survive jointly, not for ranking magnitude naively.",
      src
    ))
  ) +
  theme_dhsc_slide(base = 14) +
  theme(legend.position = "top")
save_slide(pB, file.path(out, "slide_rv_joint_terms.png"))

# --- slide C: measure sensitivity S0 vs S1 ---------------------------------
ms <- rd("tbl_rv_measure_sensitivity.csv") |>
  mutate(
    measure = factor(measure, levels = rev(c(
      "Inflation-only (CPIH)", "Rent-adjusted (LAD)", "House-price-adjusted (LAD)",
      "Rent-adjusted (TTWA)", "House-price-adjusted (TTWA)"
    ))),
    controls = factor(controls, levels = c("S0", "S1"),
                      labels = c("S0: real value only", "S1: + survey funding"))
  )

pC <- ggplot(ms, aes(OR, measure, colour = controls)) +
  geom_vline(xintercept = 1, colour = grey, linewidth = 0.35, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.5,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 2.8, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c("S0: real value only" = grey, "S1: + survey funding" = blue)) +
  labs(
    title = "Real-value measures: uncontrolled vs survey-controlled",
    subtitle = "Odds ratio of leaving per 1 SD. If S1 sits on S0, survey answers do not absorb the geography/inflation signal.",
    x = "Odds ratio of leaving (per SD)", y = NULL, colour = NULL,
    caption = wrapcap(src)
  ) +
  theme_dhsc_slide(base = 14) +
  theme(legend.position = "top")
save_slide(pC, file.path(out, "slide_rv_measure_sensitivity.png"))

# --- slide D: interactions (does it matter MORE for parents / specialists?) -
ix <- rd("tbl_rv_interactions.csv") |>
  filter(controls == "S1 survey + interaction",
         grepl("matter MORE|rv x", role)) |>
  mutate(
    group = recode(group, parental = "Parental support recipients",
                   specialist = "Specialist subject recipients"),
    group = factor(group, levels = c("Parental support recipients", "Specialist subject recipients"))
  )

if (nrow(ix) > 0) {
  pD <- ggplot(ix, aes(OR, group)) +
    geom_vline(xintercept = 1, colour = grey, linewidth = 0.35, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.55, colour = orange) +
    geom_point(size = 3.4, colour = orange) +
    geom_text(aes(label = sprintf("%.3f", OR)), vjust = -1.1, size = 4, colour = ink) +
    labs(
      title = "Does real LSF value matter more for parents or specialist recipients?",
      subtitle = paste0(
        "Interaction odds ratio (rv x group) from models that also include the main effects ",
        "and survey funding controls. OR > 1: real value's link to leaving is stronger (more risk) ",
        "in that group; OR < 1: stronger protective link; OR ~ 1: no differential."
      ),
      x = "Interaction odds ratio (1.0 = no differential)", y = NULL,
      caption = wrapcap(src)
    ) +
    theme_dhsc_slide(base = 14)
  save_slide(pD, file.path(out, "slide_rv_interactions.png"))
} else {
  message("NOTE: no interaction rows for slide D; check tbl_rv_interactions.csv")
}

# --- slide E: multi-outcome ladder (S0 vs S1 only) -------------------------
mo <- rd("tbl_rv_spec_ladder.csv") |>
  filter(scale == "per 1 SD", spec %in% c("S0", "S1")) |>
  mutate(
    spec = factor(spec, levels = c("S0", "S1"),
                  labels = c("S0: real value only", "S1: + survey")),
    outcome = factor(outcome, levels = c(
      "Left before finishing", "Claimed once only",
      "Left 2+ years early", "Considered leaving"
    ))
  )

pE <- ggplot(mo, aes(OR, outcome, colour = spec)) +
  geom_vline(xintercept = 1, colour = grey, linewidth = 0.35, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.5,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 2.8, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c("S0: real value only" = grey, "S1: + survey" = teal)) +
  labs(
    title = "Robustness across leaving definitions",
    subtitle = "Rent-adjusted (TTWA) real value, per 1 SD. Behavioural outcomes vs self-reported intention.",
    x = "Odds ratio", y = NULL, colour = NULL,
    caption = wrapcap(src)
  ) +
  theme_dhsc_slide(base = 14) +
  theme(legend.position = "top")
save_slide(pE, file.path(out, "slide_rv_outcomes.png"))

progress("05b done. slides written to ", out)
