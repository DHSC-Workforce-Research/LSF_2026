# =====================================================================
# 14_extra_factors.r  -  who leaves, beyond the five funding items:
#   (1) grant component applied for   (2) profession/subject
#   (3) spread across universities    (4) awareness -> confidence
#   (5) confidence -> leaving
# Prints five small tables and leaves each as a named object.
# Run:  source("scripts/14_extra_factors.r")
# =====================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(stringr)
  purrr::walk(list.files("functions", full.names = TRUE), source)
}))

# ---- cached student-level sample (course, entry_year, confidence, outcomes) ----
smp <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_3spec_sample.rds")))
smp$leave <- as.integer(as.logical(smp$left_before_finish))

# ---- pull university + grant component from the panel (entry wave only) ----
progress("reading college + grant components from panel (column-select) ...")
lm <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
               col_select = c("UniqueID", "year", "first_year", "college", "grants_applied"),
               show_col_types = FALSE)
entry <- lm |> filter(first_year == TRUE) |>
  group_by(UniqueID) |> slice_min(year, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(UniqueID,
            college       = as.character(college),
            grants_applied = coalesce(as.character(grants_applied), ""))
smp <- left_join(smp, entry, by = "UniqueID")

gc   <- smp[!is.na(smp$leave), ]               # rows with an observed leave/stay
base <- round(100 * mean(gc$leave), 1)
cat(sprintf("\nOverall leaving rate (left before finish): %.1f%%   n = %s\n\n",
            base, format(nrow(gc), big.mark = ",")))

# ---- (1) grant component (substring match; robust to delimiter) ----
grp <- function(d, cond, label) data.frame(
  factor = label, n_flagged = sum(cond),
  leave_flagged = round(100 * mean(d$leave[cond]), 1),
  leave_others  = round(100 * mean(d$leave[!cond]), 1),
  lift = round(mean(d$leave[cond]) / mean(d$leave[!cond]), 2))
component <- bind_rows(
  grp(gc, str_detect(gc$grants_applied, regex("parent", ignore_case = TRUE)),               "Parental support"),
  grp(gc, str_detect(gc$grants_applied, regex("exception|hardship", ignore_case = TRUE)),   "Exceptional / hardship"),
  grp(gc, str_detect(gc$grants_applied, regex("travel|dual|accommod", ignore_case = TRUE)), "Travel / dual accommodation"),
  grp(gc, str_detect(gc$grants_applied, regex("disab|specialist", ignore_case = TRUE)),     "Disability / specialist"))
cat("=== (1) Leaving by grant component applied for ===\n"); print(component, row.names = FALSE); cat("\n")

# ---- (2) profession / subject (regex on course name) ----
prof_of <- function(x) { x <- tolower(coalesce(x, ""))
  dplyr::case_when(
    str_detect(x, "midwif")                          ~ "Midwifery",
    str_detect(x, "mental health")                   ~ "Nursing - Mental Health",
    str_detect(x, "learning disab")                  ~ "Nursing - Learning Disability",
    str_detect(x, "child")                           ~ "Nursing - Children's",
    str_detect(x, "adult") & str_detect(x, "nurs")   ~ "Nursing - Adult",
    str_detect(x, "nurs")                            ~ "Nursing - other/dual",
    str_detect(x, "physio")                          ~ "Physiotherapy",
    str_detect(x, "occupational")                    ~ "Occupational Therapy",
    str_detect(x, "paramedic")                       ~ "Paramedic",
    str_detect(x, "radiograph|radiother")            ~ "Radiography",
    str_detect(x, "speech|language")                 ~ "Speech & Language Therapy",
    str_detect(x, "diet")                            ~ "Dietetics",
    str_detect(x, "podiat|chiropod")                 ~ "Podiatry",
    str_detect(x, "operating department|\\bodp\\b")  ~ "ODP",
    TRUE                                             ~ "Other AHP")
}
gc$prof <- prof_of(gc$course)
profession <- gc |> group_by(prof) |>
  summarise(n = n(), leave = round(100 * mean(leave), 1), .groups = "drop") |>
  arrange(desc(leave))
cat("=== (2) Leaving by profession / subject ===\n"); print(profession, row.names = FALSE); cat("\n")

# ---- (3) spread across universities (no naming; just the spread) ----
uni <- gc |> group_by(college) |>
  summarise(n = n(), leave = 100 * mean(leave), .groups = "drop") |>
  filter(n >= 200)
university <- data.frame(
  n_universities = nrow(uni),
  students_covered = sum(uni$n),
  overall_leave  = base,
  p10_leave = round(quantile(uni$leave, .10), 1),
  median_leave = round(quantile(uni$leave, .50), 1),
  p90_leave = round(quantile(uni$leave, .90), 1),
  sd_between_unis = round(sd(uni$leave), 1))
cat("=== (3) Variation across universities (n>=200 each) ===\n"); print(university, row.names = FALSE); cat("\n")

# ---- (4) awareness -> financial confidence (survivors, y2+) ----
numconf <- function(x) as.numeric(str_extract(as.character(x), "[0-9]"))
cc <- smp[!is.na(smp$confidence), ]
cc$conf_n <- numconf(cc$confidence)
aware_confidence <- cc |> group_by(fund_availability) |>
  summarise(n = n(), mean_confidence = round(mean(conf_n, na.rm = TRUE), 2),
            pct_confident_4plus = round(100 * mean(conf_n >= 4, na.rm = TRUE), 1), .groups = "drop")
cat("=== (4) Financial confidence by whether aware of grant at entry ===\n")
print(aware_confidence, row.names = FALSE); cat("\n")

# ---- (5) financial confidence -> leaving ----
cl <- smp[!is.na(smp$confidence) & !is.na(smp$leave), ]
cl$conf_n <- numconf(cl$confidence)
confidence_leaving <- cl |> group_by(conf_n) |>
  summarise(n = n(), leave = round(100 * mean(leave), 1), .groups = "drop") |> arrange(conf_n)
or_per_point <- round(exp(coef(glm(leave ~ conf_n, data = cl, family = binomial))["conf_n"]), 3)
cat("=== (5) Leaving by financial-confidence score (1 low - 5 high) ===\n")
print(confidence_leaving, row.names = FALSE)
cat(sprintf("Odds ratio per +1 confidence point: %.3f  (below 1 = more confident, less likely to leave)\n\n", or_per_point))

message("done. objects: component, profession, university, aware_confidence, confidence_leaving")