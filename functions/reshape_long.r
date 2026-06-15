# ---------------------------------------------------------------------------
# reshape_long(): wide panel -> one row per student-year
#
# The panel holds each student once, with the 24 questions repeated across a
# "YYYY-" prefix for every year 2020 to 2026. For analysis we want it long:
# one row per student per year they actually answered. I build it a year at a
# time and stack the results, rather than melting all 168 columns at once,
# because the all-at-once melt blows up to ~49 million rows and chews memory
# for no benefit. A student-year with nothing in it (all questions NA) means
# they didn't respond that year, so it gets dropped.
# ---------------------------------------------------------------------------

reshape_long <- function(wide, id = "UniqueID") {
  years <- wide |>
    dplyr::select(-tidyselect::all_of(id)) |>
    names() |>
    stringr::str_extract("^\\d{4}") |>
    unique()

  pull_year <- function(yr) {
    wide |>
      dplyr::select(tidyselect::all_of(id), tidyselect::starts_with(paste0(yr, "-"))) |>
      dplyr::rename_with(
        \(nm) stringr::str_remove(nm, paste0("^", yr, "-")),
        -tidyselect::all_of(id)
      ) |>
      dplyr::mutate(year = as.integer(yr), .after = tidyselect::all_of(id)) |>
      dplyr::filter(dplyr::if_any(
        -tidyselect::all_of(c(id, "year")),
        \(x) !is.na(x)
      ))
  }

  years |>
    purrr::map(pull_year) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data[[id]], year)
}