# ---------------------------------------------------------------------------
# outputs_dir(): path to the summarised-outputs folder (aggregate findings)
#
# Aggregate, non-individual-level artefacts (tables, figures, model HTML) go
# here: the NW025 research project area, set via LSF_OUTPUT_DIR in .Renviron so
# the path stays out of the code and out of git. NOT the secure microdata share
# and NOT the repo. Individual-level data still goes to derived_dir().
# ---------------------------------------------------------------------------
outputs_dir <- function() {
  d <- Sys.getenv("LSF_OUTPUT_DIR")
  if (d == "") stop("LSF_OUTPUT_DIR is not set. Add it to .Renviron and restart R.", call. = FALSE)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}