# ---------------------------------------------------------------------------
# clean_lsf(): tidy the assembled panel
#
# Three jobs, kept deliberately light. Repair the encoding mess where £ came
# through as "Â£" (someone saved cp1252 as UTF-8 upstream), squish stray
# whitespace, and turn empty strings into proper NA so a blank reads as "did
# not answer". I leave everything as text on purpose. Typing individual fields
# is an analysis decision, not a cleaning one, so it happens later where it
# matters. If we spot more mojibake, extend the lookup.
# ---------------------------------------------------------------------------

clean_lsf <- function(df) {
  mojibake <- c("Â£" = "£", "â€™" = "’", "â€“" = "–", "â€œ" = "“", "â€" = "”", "Â" = "")

  df |>
    dplyr::mutate(dplyr::across(
      where(is.character),
      \(x) x |>
        stringr::str_replace_all(mojibake) |>
        stringr::str_squish() |>
        dplyr::na_if("")
    ))
}