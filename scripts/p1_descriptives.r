# ===========================================================================
# scripts/p1_descriptives.r
#
# U3 of the placement-hours arm. The honest unadjusted picture: outcomes by
# hours band, published NEXT TO what each band is actually made of.
#
# The composition table is not an appendix. Of the 23 programmes carrying an
# hours figure, everything above 660 is a nursing field, midwifery, ODP or
# Dental Therapist. "High placement hours" is close to a synonym for "is a
# nurse". If the high band turns out to be overwhelmingly nursing, the outcome
# difference below it is a nursing-versus-everyone comparison and every slide
# downstream has to say so. That number therefore appears in the same file, on
# the same page, as the outcome it qualifies.
#
# Composition is written and printed FIRST, before the outcome comparison, so
# that fact is known before anyone reads a difference.
#
# Outputs (AGGREGATE only -> outputs_dir()/placement_hours_pack_YYYYMMDD/)
#   00_README.txt
#   P1_band_composition_family.csv
#   P1_band_composition_programme.csv
#   P1_outcomes_by_band.csv
#   placement_numbers.txt
#
# Run from repo root (after 01, and after the crosswalk is completed):
#   source("scripts/p1_descriptives.r", encoding = "UTF-8")
#
# ASSOCIATIONAL only. No student-level data is written.
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(tibble)
})

# ---- CONFIG ----------------------------------------------------------------
MIN_N <- 10L   # disclosure floor. Cells below this are marked "suppressed",
               # never blanked - a gap reads as a zero and a zero is a claim.
OUTCOMES <- tibble::tribble(
  ~var,                 ~label,                       ~kind,
  "left_before_finish", "Left before finishing",      "binary",
  "considered_leaving", "Considered leaving",         "binary",
  "fund_availability",  "Funding was available",      "binary",
  "grant_influence",    "Grant influenced decision",  "binary",
  "grant_helps_stay",   "Grant helps me stay",        "binary",
  "confidence",         "Financial confidence (1-5)", "scale"
)
# ---------------------------------------------------------------------------

stamp <- format(Sys.Date(), "%Y%m%d")
pack  <- file.path(outputs_dir(), paste0("placement_hours_pack_", stamp))
dir.create(pack, showWarnings = FALSE, recursive = TRUE)

numbers_file <- file.path(pack, "placement_numbers.txt")
writeLines(character(0), numbers_file)
cat_both <- function(...) {
  msg <- paste0(...)
  message(msg)
  cat(msg, "\n", file = numbers_file, append = TRUE)
}

# ---- load and attach -------------------------------------------------------
progress("p1: loading sample and attaching placement hours ...")
stud <- placement_sample(quiet = TRUE)
rep  <- attr(stud, "match_report")

cat_both("=== PLACEMENT HOURS: descriptives (U3) ===")
cat_both("Source: DHSC programme-level average placement hours per year, FY26/27,")
cat_both("applied retrospectively to entry cohorts 2020-2026. Associational only.")
cat_both("")
placement_match_report(stud, emit = cat_both)
cat_both("")

# Gate: if the crosswalk loses most of the sample there is nothing honest to
# describe. Report the rate and stop rather than describing a mangled variable.
if (is.na(rep$hours_rate) || rep$hours_rate < 0.50) {
  cat_both(sprintf("STOP: only %.1f%% of students carry an hours value.",
                   100 * rep$hours_rate))
  cat_both("Complete reference/course_crosswalk.csv before interpreting anything.")
  stop("placement hours: match rate below 50% - see ", numbers_file, call. = FALSE)
}

# Coerce the binary outcomes the same way 06_findings_pack.r does, so the arms
# speak the same language as the existing pack.
stud <- stud |>
  mutate(across(any_of(c("fund_availability", "grant_influence", "grant_helps_stay",
                         "left_before_finish", "considered_leaving")), to_01),
         confidence = suppressWarnings(as.integer(confidence)),
         band = if_else(is.na(hours_band), "unmatched", as.character(hours_band)),
         band = factor(band, levels = c("low", "medium", "high", "unmatched")))

# suppression helper: below MIN_N the value is withheld and SAID to be withheld
supp <- function(value, n, digits = 1) {
  ifelse(n < MIN_N, "suppressed", formatC(value, format = "f", digits = digits))
}

# ===========================================================================
# COMPOSITION FIRST (R4). Read this before the outcome table below.
# ===========================================================================
progress("p1: band composition ...")

comp_family <- stud |>
  count(band, course_family, name = "n") |>
  group_by(band) |>
  mutate(share_pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(family = if_else(is.na(course_family), "unmatched",
                          as.character(course_family)),
         n_pub = if_else(n < MIN_N, NA_integer_, n),
         share_pub = supp(share_pct, n)) |>
  select(band, family, n = n_pub, share_pct = share_pub) |>
  arrange(band, desc(share_pct))

write_csv(comp_family, file.path(pack, "P1_band_composition_family.csv"))

comp_prog <- stud |>
  count(band, programme_name, name = "n") |>
  group_by(band) |>
  mutate(share_pct = 100 * n / sum(n)) |>
  ungroup() |>
  filter(n >= MIN_N) |>
  arrange(band, desc(n)) |>
  group_by(band) |>
  slice_head(n = 8) |>
  ungroup() |>
  mutate(programme = if_else(is.na(programme_name), "unmatched", programme_name),
         share_pct = round(share_pct, 1)) |>
  select(band, programme, n, share_pct)

write_csv(comp_prog, file.path(pack, "P1_band_composition_programme.csv"))

cat_both("--- BAND COMPOSITION BY COURSE FAMILY ---")
cat_both("Read this before the outcome table. If one family dominates a band,")
cat_both("the band difference is that family's difference wearing a new label.")
for (b in levels(stud$band)) {
  rows <- filter(comp_family, band == b)
  if (!nrow(rows)) next
  cat_both(sprintf("  %s band (n = %s):", b,
                   format(sum(stud$band == b), big.mark = ",")))
  for (i in seq_len(nrow(rows)))
    cat_both(sprintf("    %-12s %6s%%", rows$family[i], rows$share_pct[i]))
}
top_share <- comp_family |>
  filter(band == "high", share_pct != "suppressed") |>
  mutate(s = as.numeric(share_pct)) |>
  slice_max(s, n = 1, with_ties = FALSE)
if (nrow(top_share) && top_share$s >= 75) {
  cat_both("")
  cat_both(sprintf("NOTE: the high band is %.0f%% %s. Every comparison below, and every",
                   top_share$s, top_share$family))
  cat_both(sprintf("slide built from it, is a %s-versus-everyone comparison. Label it as one.",
                   top_share$family))
}
cat_both("")

# ===========================================================================
# OUTCOMES BY BAND (R4), paired with the composition above.
# ===========================================================================
progress("p1: outcomes by band ...")

out_rows <- list()
for (i in seq_len(nrow(OUTCOMES))) {
  v <- OUTCOMES$var[i]
  if (!v %in% names(stud)) {
    cat_both(sprintf("(outcome %s not on the sample - skipped)", v))
    next
  }
  d <- stud[!is.na(stud[[v]]), , drop = FALSE]
  g <- d |>
    group_by(band) |>
    summarise(n = n(),
              value = mean(.data[[v]], na.rm = TRUE),
              .groups = "drop") |>
    mutate(outcome = v,
           outcome_label = OUTCOMES$label[i],
           kind = OUTCOMES$kind[i],
           value_pub = if_else(kind == "binary",
                               supp(100 * value, n),
                               supp(value, n, digits = 2)),
           unit = if_else(kind == "binary", "%", "mean (1-5)"),
           n_pub = if_else(n < MIN_N, NA_integer_, n)) |>
    select(outcome, outcome_label, band, n = n_pub, value = value_pub, unit)
  out_rows[[length(out_rows) + 1L]] <- g
}
outcomes_tbl <- bind_rows(out_rows)
write_csv(outcomes_tbl, file.path(pack, "P1_outcomes_by_band.csv"))

cat_both("--- OUTCOMES BY HOURS BAND (unadjusted) ---")
cat_both("No controls. Band is close to subject. See composition above.")
for (v in unique(outcomes_tbl$outcome)) {
  rows <- filter(outcomes_tbl, outcome == v)
  cat_both(sprintf("  %s (%s):", rows$outcome_label[1], rows$unit[1]))
  for (i in seq_len(nrow(rows)))
    cat_both(sprintf("    %-10s n=%-8s %s",
                     as.character(rows$band[i]),
                     ifelse(is.na(rows$n[i]), "supp", format(rows$n[i], big.mark = ",")),
                     rows$value[i]))
}
cat_both("")

# ---- README ----------------------------------------------------------------
readme <- c(
  "placement hours pack - U3 descriptives",
  paste0("built ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "P1_band_composition_family.csv     what each hours band is made of, by family",
  "P1_band_composition_programme.csv  top programmes per band (n >= 10 only)",
  "P1_outcomes_by_band.csv            outcomes by band, unadjusted",
  "placement_numbers.txt              the same numbers in plain English",
  "",
  sprintf("disclosure floor: cells below n=%d are marked 'suppressed', not blanked.", MIN_N),
  sprintf("match rate: %.1f%% of students carry an hours value.", 100 * rep$hours_rate),
  if (!is.null(rep$band_cuts))
    sprintf("band cut points (hours per 100): %s",
            paste(sprintf("%.2f", rep$band_cuts), collapse = " | ")) else "",
  "",
  "These are UNADJUSTED comparisons. Placement hours are a programme-level",
  "attribute, so an hours band is close to a subject grouping. Nothing here",
  "controls for subject, provider, place, or year. The adjusted arms are",
  "scripts/p2_hours_interaction.r and scripts/p3_hours_family_fe.r.",
  "",
  "No student-level data is written by this script."
)
writeLines(readme[nzchar(readme)], file.path(pack, "00_README.txt"))

progress("p1: done -> ", pack)
