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
#   analysis_rv_curves()           <- scripts/07_real_value_comms.r (analysis half)
#   analysis_framing()             <- scripts/05d_framing_slides.r (extracted + persisted)
#   analysis_placement_*()         <- scripts/p1, p1b, p2, p3
#   analysis_triangle_rates()      <- scripts/d5_funding_triangle_rates.r
#   analysis_confidence_bands()    <- scripts/d2_analysis.r (table half)
#   analysis_triangle_cross()      <- scripts/d7_triangle_cross_question.r (analysis half)
#
# TASK 5 COMPLETE: every model in the project is now fitted from this script,
# and every table the deck manifest names is produced here. All four
# harness-pinned tables come from here (B_panel_descriptives.csv,
# tbl_rv_spec_ladder.csv, tbl_hazard_1k.csv, tbl_recruit_fe_results.csv).
#
# The slide blocks inside 03, 05b-e, 07, 08, p4, d3, d6 and d7 stay in those
# scripts until task 6 moves them into the deck layer; every number they draw
# is now persisted as a CSV, which is what makes that move possible.
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

progress("02_analysis: predicted-probability curves ...")
analysis_rv_curves()

progress("02_analysis: framing tables (place, package) ...")
analysis_framing()

# ---- annexes ---------------------------------------------------------------
# Both read data sources the panel work does not depend on (placement hours,
# and the BSA cross-tab workbooks parsed in 01), so a missing input skips the
# annex rather than stopping the main chain.
try_analysis <- function(what, f) {
  tryCatch(f(), error = function(e) {
    message("SKIPPED ", what, ": ", conditionMessage(e))
    invisible(NULL)
  })
}

progress("02_analysis: placement hours annex ...")
try_analysis("placement descriptives", analysis_placement_descriptives)
try_analysis("placement interaction",  analysis_placement_interaction)
try_analysis("placement family FE",    analysis_placement_family_fe)

progress("02_analysis: equity triangle annex ...")
try_analysis("confidence bands",  analysis_confidence_bands)
try_analysis("triangle rates",    analysis_triangle_rates)
try_analysis("triangle cross-question", analysis_triangle_cross)

progress("02_analysis.r complete -> ", outputs_dir())
