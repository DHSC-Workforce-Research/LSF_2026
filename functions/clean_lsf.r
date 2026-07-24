# ---------------------------------------------------------------------------
# clean_lsf(): tidy the assembled panel
#
# Three jobs, kept deliberately light. Repair the encoding mess where a pound
# sign arrives as the two-character cp1252-read-as-UTF-8 sequence (someone
# saved cp1252 as UTF-8 upstream), squish stray
# whitespace, and turn empty strings into proper NA so a blank reads as "did
# not answer". I leave everything as text on purpose. Typing individual fields
# is an analysis decision, not a cleaning one, so it happens later where it
# matters. If we spot more mojibake, extend the lookup.
#
# The lookup below is written with \u escapes, not literal characters. It has
# to contain the exact bytes it repairs, and a literal copy would be the one
# thing this project cannot have in a .r file: non-ASCII that Windows
# source() truncates the file at. The escapes are identical to R at runtime.
# ---------------------------------------------------------------------------

clean_lsf <- function(df) {
  mojibake <- c("\u00c2\u00a3" = "\u00a3", "\u00e2\u20ac\u2122" = "\u2019", "\u00e2\u20ac\u201c" = "\u2013", "\u00e2\u20ac\u0153" = "\u201c", "\u00e2\u20ac" = "\u201d", "\u00c2" = "")

  df |>
    dplyr::mutate(dplyr::across(
      where(is.character),
      \(x) x |>
        stringr::str_replace_all(mojibake) |>
        stringr::str_squish() |>
        dplyr::na_if("")
    ))
}