# ===========================================================================
# scripts/01_data.r
#
# STAGE 1 of the pipeline: raw sources -> analysis-ready tables. No models, no
# slides, no findings. Everything written here goes to the secure derived
# folder; nothing individual-level ever touches the repo.
#
#   source("scripts/01_data.r")
#
# Produces, in the derived folder:
#   lsf_panel_long_2020_2026.csv            one row per student-year
#   lsf_trajectories_classified_2020_2026.csv  one row per student, exit classified
#   lsf_analysis_sample.rds                 the student-level model table
#   lsf_real_value_sample.rds               + every cost-of-living real-value measure
#   financial_confidence_long.csv           BSA confidence cross-tab, long
#   funding_triangle_long.csv               BSA funding-triangle cross-tab, long
#
# The two BSA cross-tab workbooks come from David Taylor and are a separate
# data source from the LSF panel; if they are absent this script says so and
# carries on, since the panel work does not depend on them.
#
# Reference CSVs in reference/ are inputs here, not outputs. Rebuild them only
# when the external ONS / Land Registry data is refreshed: 90_build_reference.r.
# ===========================================================================

source("scripts/00_config.r")

suppressMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(purrr); library(readxl); library(tibble)
})

# ---- 1. the LSF panel ------------------------------------------------------
build_analysis_sample()

# ---- 2. real value ---------------------------------------------------------
build_real_value_sample()

# ---- 3. BSA demographic cross-tabs (optional inputs) -----------------------
try_parse <- function(what, f) {
  tryCatch(f(), error = function(e) {
    message("SKIPPED ", what, ": ", conditionMessage(e))
    invisible(NULL)
  })
}
try_parse("financial confidence workbook", parse_confidence_workbook)
try_parse("funding triangle workbook",     parse_funding_triangle)

progress("01_data.r complete -> ", derived_dir())
