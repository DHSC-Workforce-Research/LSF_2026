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

  first_col      <- raw |> dplyr::pull(1)
  header_present <- dplyr::first(first_col) == "UniqueID"

  ids <- first_col |>
    stringr::str_extract("(?<=Student)\\d+") |>
    readr::parse_integer()

  tibble::tibble(
    file       = basename(file),
    n_row      = nrow(raw),
    n_col      = ncol(raw),
    has_header = header_present,
    id_min     = min(ids, na.rm = TRUE),
    id_max     = max(ids, na.rm = TRUE)
  )
}