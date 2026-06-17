# ---------------------------------------------------------------------------
# course_length(): map a course name to its length in years
#
# Default 3, which is correct for the standard pre-reg UG healthcare courses
# that make up ~90% of the data (the nursing fields, midwifery, physio,
# paramedic, OT, radiography, SLT, ODP). Pass `overrides` (a named vector) to
# correct the handful that differ once we've eyeballed the full 72. NB the data
# does not record study level, so PG pre-reg routes (often 2yr) cannot be told
# apart from their 3yr UG namesakes here; that's a known, bounded error.
# ---------------------------------------------------------------------------
course_length <- function(course, default = 3L, overrides = NULL) {
  out <- rep(as.integer(default), length(course))
  if (!is.null(overrides)) {
    hit <- match(course, names(overrides))
    out[!is.na(hit)] <- as.integer(overrides[hit[!is.na(hit)]])
  }
  out
}