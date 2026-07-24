# ===========================================================================
# scripts/02_analysis.r
#
# STAGE 2 of the pipeline: analysis-ready tables -> tidy result tables. Every
# model in the project is fitted from here, and every result it produces is
# persisted as a tbl_*.csv in outputs_dir(). Stage 3 draws from those CSVs and
# fits nothing.
#
#   source("scripts/02_analysis.r")
#
# Run after 01_data.r. Verify with tests/check_numbers.r.
#
# MIGRATION IN PROGRESS (task 5 of the RAP refactor). Landed so far:
#   analysis_panel()               <- scripts/02_analyse.r
#   analysis_real_value()          <- scripts/05_real_value_controlled.r
#   analysis_findings_pack()       <- scripts/06_findings_pack.r
#   analysis_hazard_recruitment()  <- scripts/08_hazard_and_recruitment.r (analysis half)
# All four harness-pinned tables are now produced from here: B_panel_descriptives.csv,
# tbl_rv_spec_ladder.csv, tbl_hazard_1k.csv, tbl_recruit_fe_results.csv.
# Still to move, and still to be run from their own scripts until they do:
#   07 predicted-probability curves
#   05d place / package tables (currently computed inside the slide script)
#   p1-p3 placement hours, d5/d7 equity triangle
# The six slide blocks inside 08 stay in that script until task 6 moves them
# into the deck layer; every number they draw is already persisted as a CSV.
# ===========================================================================

source("scripts/00_config.r")

suppressMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(purrr); library(fixest); library(tibble)
})

progress("02_analysis: panel ...")
analysis_panel()

progress("02_analysis: real value (Arm 1) ...")
analysis_real_value()

progress("02_analysis: findings pack ...")
analysis_findings_pack()

progress("02_analysis: hazard (Arm 2) + recruitment (Arm 3) ...")
analysis_hazard_recruitment()

progress("02_analysis.r complete -> ", outputs_dir())
