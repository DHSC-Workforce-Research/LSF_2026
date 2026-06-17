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