# ---------------------------------------------------------------------------
# Project paths
#
# The data never lives in this repo. It sits in the secure share, and we reach
# it through one environment variable (LSF_DATA_DIR) set in .Renviron, so the
# secure path is never committed to git. If it isn't set, stop now with a clear
# message rather than failing obscurely three functions later.
# ---------------------------------------------------------------------------

data_dir <- function() {
  dir <- Sys.getenv("LSF_DATA_DIR")
  if (dir == "") {
    stop("LSF_DATA_DIR is not set. Add it to .Renviron and restart R.", call. = FALSE)
  }
  dir
}

derived_dir <- function() {
  path <- file.path(data_dir(), "_derived")
  if (!dir.exists(path)) dir.create(path)
  path
}

panel_files <- function() {
  data_dir() |>
    list.files(pattern = "Yearly Comparison.*\\.xlsx$", full.names = TRUE) |>
    sort()
}

fs <- list.files("functions", full.names = TRUE)
fs[sapply(fs, function(f)
  any(grepl("walk\\(|source\\(|list\\.files\\(", readLines(f, warn = FALSE))))]
