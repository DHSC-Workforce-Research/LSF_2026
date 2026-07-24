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
#   analysis_panel()       <- scripts/02_analyse.r
#   analysis_real_value()  <- scripts/05_real_value_controlled.r
# Still to move, and still to be run from their own scripts until they do:
#   06 findings pack (B_panel_descriptives, lag/mechanism tables)
#   07 predicted-probability curves
#   08 Arm 2 hazard + Arm 3 recruitment
#   05d place / package tables (currently computed inside the slide script)
#   p1-p3 placement hours, d5/d7 equity triangle
# Until that is complete this script does NOT reproduce the full harness set;
# the old scripts remain in place and runnable.
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

progress("02_analysis.r complete -> ", outputs_dir())
