# Collapse a 1..5 Likert (output of extract_stacked_tables) into named bands
# and express each as a % of the group total.
#
# `bands` maps a band name to the rating values it covers. Default is the
# confidence banding: Unconfident = 1-2, Neutral = 3, Confident = 4-5.
# Reuse for any 5-point question by passing a different `bands` list, e.g.
#   band_pct(long, list(Bottom2 = 1:2, Mid = 3, Top2 = 4:5))

band_pct <- function(long, bands = list(Unconfident = 1:2,
                                        Neutral     = 3,
                                        Confident   = 4:5)) {
  map <- do.call(rbind, lapply(names(bands), function(nm)
    data.frame(rating = bands[[nm]], band = nm, stringsAsFactors = FALSE)))

  w <- long |>
    dplyr::inner_join(map, by = "rating") |>
    dplyr::group_by(Demographic, Group, n, band) |>
    dplyr::summarise(count = sum(count), .groups = "drop") |>
    tidyr::pivot_wider(names_from = band, values_from = count, values_fill = 0)

  for (nm in names(bands)) if (!nm %in% names(w)) w[[nm]] <- 0
  for (nm in names(bands)) w[[paste0(nm, "_pct")]] <- round(100 * w[[nm]] / w$n, 1)

  w$is_total <- grepl("grand total|^total$", tolower(w$Group))
  dplyr::arrange(w, Demographic, Group)
}

# Convenience wrapper for the financial-confidence banding
confidence_by_group <- function(long) band_pct(long)