# ---------------------------------------------------------------------------
# stitch_panel(): bind the four wide blocks into one panel
#
# The blocks are one table that got split into 75k-row chunks for sharing.
# Block 2 lost its header in the export, blocks 1, 3 and 4 kept theirs. So I
# read every block positionally (no header), stack them, lift the real header
# off any row that still says "UniqueID", drop those embedded header rows, and
# name the columns properly. Order of the blocks doesn't matter.
# ---------------------------------------------------------------------------

stitch_panel <- function(files) {
  stacked <- files |>
    purrr::map(read_lsf, has_header = FALSE) |>
    purrr::list_rbind()

  id_col <- names(stacked)[1]                     # first column, whatever it's called

  header <- stacked |>
    dplyr::filter(.data[[id_col]] == "UniqueID") |>
    dplyr::slice(1) |>
    unlist(use.names = FALSE) |>
    as.character()

  stacked |>
    dplyr::filter(.data[[id_col]] != "UniqueID") |>   # drop the embedded header rows
    rlang::set_names(header)
}