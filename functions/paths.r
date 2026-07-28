# ---------------------------------------------------------------------------
# Project paths
#
# Data never lives in this repo. Individual-level data sits in the secure share;
# aggregate outputs go to the NW025 project area. Both are built from the
# Windows user profile, so no username is hard-coded and it works for any user.
# Set LSF_DATA_DIR / LSF_OUTPUT_DIR to override.
#
# Paths are built here with file.path(), NOT via ${USERPROFILE} in .Renviron,
# because .Renviron mangles the backslashes in USERPROFILE (C:\Users\... becomes
# C:Users...). Sys.getenv() here returns the value intact.
# ---------------------------------------------------------------------------

dhsc_root <- function() {
  up <- Sys.getenv("USERPROFILE")
  if (up == "") up <- Sys.getenv("HOME")
  if (up == "") stop("Neither USERPROFILE nor HOME is set.", call. = FALSE)
  file.path(up, "Department of Health and Social Care")
}

data_dir <- function() {
  override <- Sys.getenv("LSF_DATA_DIR")
  if (override != "") return(override)
  file.path(dhsc_root(),
            "GOV-Workforce_Secure_Data - BSA LSF - BSA LSF",
            "BSA (LSF) Survey", "2026 Data Share")
}

derived_dir <- function() {
  path <- file.path(data_dir(), "_derived")
  if (!dir.exists(path)) dir.create(path)
  path
}

outputs_dir <- function() {
  override <- Sys.getenv("LSF_OUTPUT_DIR")
  base <- if (override != "") override else
    file.path(dhsc_root(), "NW025 - Research", "1. Projects",
              "Learning Support Fund Evaluation", "LSF Review 2026", "outputs")
  if (!dir.exists(base)) dir.create(base, recursive = TRUE)
  base
}

panel_files <- function() {
  data_dir() |>
    list.files(pattern = "Yearly Comparison.*\\.xlsx$", full.names = TRUE) |>
    sort()
}

# ---------------------------------------------------------------------------
# Output sub-areas (task 11, 2026-07-28). Everything the analysis writes goes
# into a named subfolder of outputs_dir(), so the root holds folders, not a
# heap of loose files. The harness and the deck both find tables by recursive
# search under outputs_dir(), so nothing downstream depends on WHICH subfolder
# a table lands in; these just decide where writes go.
#
#   tables_dir()         the standalone analysis tables (02's panel + real value
#                        + framing) that used to sit loose in the root
#   findings_pack_dir()  the emailable numbers pack; a FIXED name, overwritten
#                        each run, so it stops accumulating one dated copy per run
#   placement_pack_dir() the placement-hours pack; likewise fixed and overwritten
# ---------------------------------------------------------------------------
.out_sub <- function(name) {
  p <- file.path(outputs_dir(), name)
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
  p
}
tables_dir         <- function() .out_sub("tables")
findings_pack_dir  <- function() .out_sub("findings_pack")
placement_pack_dir <- function() .out_sub("placement_hours")