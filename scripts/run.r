# ===========================================================================
# run.r  -  master controller for the LSF_2026 pipeline.
#   Open this, set the flags at the top, run top to bottom (or run one section).
#   BUILD is slow and only needed when the RAW data changes (a new wave). ANALYSE
#   and COMMUNICATE are cheap: they read the derived CSVs from disk, so re-run
#   them freely. define_leaving_outcomes() runs at ANALYSE time, so a change
#   there (e.g. the one-wave gate) needs only an ANALYSE re-run, NOT a rebuild.
# ===========================================================================

# Working directory must be the LSF_2026 repo root.
purrr::walk(list.files("functions", full.names = TRUE), source)

# ---- flags: set these, then run the whole file ----
REBUILD     <- FALSE   # TRUE only when the RAW data changed (a new wave)
VALIDATE    <- FALSE   # TRUE to run the post-build sanity checks
COMMUNICATE <- TRUE    # TRUE to regenerate the plots + deck

run <- function(path) {
  cat("\n==================== RUN:", path, "====================\n")
  source(path, echo = FALSE)
}

# ---- TIER 1: BUILD (slow; raw -> derived CSVs). Skipped unless REBUILD. ----
if (REBUILD) {
  run("scripts/01_build_panel.r")         # raw blocks   -> panel_long + panel_wide
  run("scripts/02_build_trajectories.r")  # panel_long   -> trajectories
  run("scripts/03_classify_outcomes.r")   # trajectories -> trajectories_classified
}

# ---- VALIDATE (only after a rebuild; optional) ----
if (VALIDATE) {
  run("scripts/04_validate_outcomes.r")
  run("scripts/97_validate_course_length.r")
}

# ---- TIER 2: ANALYSE (cheap; reads the derived CSVs) ----
run("scripts/05_transition_test.r")
run("scripts/06_funding_retention_link.r")
run("scripts/07_confidence_eda.r")
run("scripts/08_leaving_regression.r")
run("scripts/09_robustness_grid.r")

# ---- TIER 2: COMMUNICATE (plots + the deck) ----
if (COMMUNICATE) {
  run("scripts/10_communication.r")
  run("scripts/11_communicate_controlled.r")
  run("scripts/12_build_deck.r")
}

cat("\n==================== pipeline complete ====================\n")