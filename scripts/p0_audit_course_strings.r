# ===========================================================================
# scripts/p0_audit_course_strings.r
#
# U1 of the placement-hours arm. NOTHING downstream can be written until this
# has run on the work machine, because `course` in the analysis sample is free
# text and there is no canonical course list anywhere in this repo. This script
# prints what students actually typed, with counts, and writes a crosswalk
# template with programme_code left blank for hand-completion.
#
# It computes no results and states none. It is a look-before-you-map step.
#
# Outputs (AGGREGATE only -> outputs_dir()/placement_hours/)
#   tbl_course_string_audit.csv   course string, n, share, current match status
#   course_crosswalk_TEMPLATE.csv copy into reference/course_crosswalk.csv and
#                                 fill programme_code by hand
#
# Run from repo root (after 01):
#   source("scripts/p0_audit_course_strings.r", encoding = "UTF-8")
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({ library(dplyr); library(readr); library(stringr); library(tibble) })

REF_DIR  <- "reference"
MIN_N    <- 5L    # disclosure floor: strings below this are reported as a
                  # single pooled row, never named individually

outdir <- file.path(outputs_dir(), "placement_hours")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ---- load ------------------------------------------------------------------
progress("p0: loading analysis sample ...")
sample <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
if (!"course" %in% names(sample)) stop("no `course` column on lsf_analysis_sample.rds", call. = FALSE)

hours <- read_csv(file.path(REF_DIR, "placement_hours.csv"),
                  show_col_types = FALSE, progress = FALSE)

# ---- the audit -------------------------------------------------------------
# NA course is its own row rather than an error: a student with no course
# recorded is a real, countable thing and the crosswalk has to decide about it.
progress("p0: counting distinct course strings ...")
audit <- sample |>
  transmute(course_raw = if_else(is.na(course), "(NA - no course recorded)",
                                 as.character(course))) |>
  count(course_raw, name = "n", sort = TRUE) |>
  mutate(
    course_key = str_squish(tolower(course_raw)),
    share_pct  = round(100 * n / sum(n), 2),
    cum_pct    = round(cumsum(share_pct), 2)
  )

stopifnot(sum(audit$n) == nrow(sample))

# Provisional exact match against the programme names we hold, purely to show
# how far a naive join gets. This is NOT the crosswalk - it is the baseline the
# hand-authored crosswalk has to beat.
naive <- hours |>
  transmute(programme_code, programme_name,
            course_key = str_squish(tolower(programme_name)))

audit <- audit |>
  left_join(select(naive, course_key, naive_code = programme_code), by = "course_key") |>
  mutate(naive_match = !is.na(naive_code))

naive_rate <- 100 * sum(audit$n[audit$naive_match]) / sum(audit$n)

# ---- disclosure control ----------------------------------------------------
# Strings held by fewer than MIN_N students are pooled into one row. A rare
# free-text course value is exactly the kind of thing that identifies someone.
named  <- filter(audit, n >= MIN_N)
rare   <- filter(audit, n <  MIN_N)
audit_pub <- named |>
  select(course_raw, n, share_pct, cum_pct, naive_code, naive_match)
if (nrow(rare)) {
  audit_pub <- bind_rows(audit_pub, tibble(
    course_raw  = sprintf("(%d distinct strings each held by <%d students)",
                          nrow(rare), MIN_N),
    n           = sum(rare$n),
    share_pct   = round(100 * sum(rare$n) / sum(audit$n), 2),
    cum_pct     = 100,
    naive_code  = NA_character_,
    naive_match = NA))
}

write_csv(audit_pub, file.path(outdir, "tbl_course_string_audit.csv"))

# ---- crosswalk template ----------------------------------------------------
# Every distinct string gets a row, including the rare ones - the template
# stays on the secure machine, so it is allowed to name them. Only the
# published audit table above is pooled.
template <- audit |>
  transmute(course_raw, course_key, n,
            programme_code = naive_code,   # pre-filled where a naive exact hit
            note = if_else(naive_match, "auto: exact name match", ""))

write_csv(template, file.path(outdir, "course_crosswalk_TEMPLATE.csv"))

# ---- report ----------------------------------------------------------------
cat("\n=== p0: course string audit ===\n")
cat(sprintf("students          : %s\n", format(nrow(sample), big.mark = ",")))
cat(sprintf("distinct strings  : %d  (%d named, %d pooled below n=%d)\n",
            nrow(audit), nrow(named), nrow(rare), MIN_N))
cat(sprintf("naive exact match : %.1f%% of students\n", naive_rate))
cat(sprintf("programmes unhit  : %d of %d\n",
            sum(!hours$programme_code %in% audit$naive_code), nrow(hours)))
cat("\nTop 40 course strings by student count:\n")
print(as.data.frame(head(select(named, course_raw, n, share_pct, cum_pct), 40)),
      row.names = FALSE)

cat("\nProgramme names with NO exact hit in the survey text (crosswalk work list):\n")
print(as.data.frame(hours |>
        filter(!programme_code %in% audit$naive_code) |>
        select(programme_code, programme_name, hours_per_year)), row.names = FALSE)

cat("\nNext: copy ", file.path(outdir, "course_crosswalk_TEMPLATE.csv"),
    "\n  -> reference/course_crosswalk.csv, fill programme_code by hand,",
    "\n  leave it blank where genuinely unknown (blank is honest, a guess is not).\n", sep = "")
progress("p0: done -> ", outdir)
