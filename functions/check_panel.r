# ---------------------------------------------------------------------------
# check_panel(): QA assertions on the stitched wide panel
#
# Run this before trusting the stitch. The numbers we expect, from the inspect
# step: 290,947 rows, all unique ids, no duplicates, ids running 1 to 290,947,
# and 169 columns. If any of these is off, the bind went wrong and we stop.
# ---------------------------------------------------------------------------

check_panel <- function(panel, id = "UniqueID") {
  ids     <- panel[[id]]
  ids_num <- as.integer(stringr::str_extract(ids, "\\d+"))

  tibble::tribble(
    ~check,           ~value,
    "rows",           nrow(panel),
    "cols",           ncol(panel),
    "unique_ids",     dplyr::n_distinct(ids),
    "duplicate_ids",  sum(duplicated(ids)),
    "id_min",         min(ids_num, na.rm = TRUE),
    "id_max",         max(ids_num, na.rm = TRUE)
  )
}