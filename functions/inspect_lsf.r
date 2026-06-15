# ---------------------------------------------------------------------------
# inspect_lsf(): a one-row structural summary of a workbook
#
# The look-before-you-leap check. Per file it reports the size, whether the
# first row is the real header (UniqueID) or already data, and which block of
# Student IDs it covers. Mapped across the files, the four blocks should run
# end to end with no gaps and no overlaps. If they don't, we stop and look.
# ---------------------------------------------------------------------------

inspect_lsf <- function(file) {
  raw <- read_lsf(file, has_header = FALSE)

  first_col <- as.character(raw[[1]])              # column one, however it's named
  ids       <- as.integer(stringr::str_extract(first_col, "\\d+"))
  has_ids   <- any(!is.na(ids))

  tibble::tibble(
    file       = basename(file),
    n_row      = nrow(raw),
    n_col      = ncol(raw),
    has_header = isTRUE(first_col[1] == "UniqueID"),
    id_min     = if (has_ids) min(ids, na.rm = TRUE) else NA_integer_,
    id_max     = if (has_ids) max(ids, na.rm = TRUE) else NA_integer_
  )
}