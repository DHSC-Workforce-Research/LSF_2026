# ANALYSIS STAGE. Collapse response options into named bands and express each
# as a % of the group total. Banding is a decision made HERE, not in the parse.
#
# `bands` maps a band name to the option_index values it covers. Keyed on
# option_index (stable regardless of whether the sheet codes are 1..5 or text).
# Reuse for any question by passing a different `bands` list.

band_pct <- function(long, bands, key = "option_index") {
  map <- do.call(rbind, lapply(names(bands), function(nm)
    data.frame(k = bands[[nm]], band = nm, stringsAsFactors = FALSE)))
  names(map)[1] <- key

  w <- long |>
    dplyr::inner_join(map, by = key) |>
    dplyr::group_by(Demographic, Group, n, band) |>
    dplyr::summarise(count = sum(count), .groups = "drop") |>
    tidyr::pivot_wider(names_from = band, values_from = count, values_fill = 0)

  for (nm in names(bands)) if (!nm %in% names(w)) w[[nm]] <- 0
  for (nm in names(bands)) w[[paste0(nm, "_pct")]] <- round(100 * w[[nm]] / w$n, 1)
  w$is_total <- grepl("grand total|^total$", tolower(w$Group))
  dplyr::arrange(w, Demographic, Group)
}

# The confidence banding for this survey: 1-2 = Unconfident, 3 = Neutral,
# 4-5 = Confident (by option position). Change here, nowhere else.
confidence_by_group <- function(long)
  band_pct(long, bands = list(Unconfident = 1:2, Neutral = 3, Confident = 4:5),
           key = "option_index")