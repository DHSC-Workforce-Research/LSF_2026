# ===========================================================================
# functions/analysis_triangle_rates.r
#
# Funding-decision triangle: rates by demographic group from the BSA cross-tab,
# with the disclosure floor applied. Moved verbatim from
# scripts/d5_funding_triangle_rates.r; the only changes are the function
# wrapper, indentation, MIN_N bound to MIN_CELL_N and TOP_BOX taken from
# scripts/00_config.r (same values).
# ===========================================================================

analysis_triangle_rates <- function() {
  MIN_N <- MIN_CELL_N   # disclosure floor, from 00_config.r


  long <- read_csv(file.path(derived_dir(), "funding_triangle_long.csv"),
                   show_col_types = FALSE)

  # labels.r: drop (blank)/NULL/Grand Total, shorten Demographic + Group.
  # Keep PNTS (flag it below); do our own n-suppression so cells are marked, not
  # dropped.
  long <- tidy_groups(long, drop_pnts = FALSE, min_n = 0)

  rates <- long |>
    filter(!is.na(count)) |>
    mutate(is_pos   = (response_type == "binary" & response == "TRUE") |
                      (response_type == "scale"  & response %in% TOP_BOX),
           resp_num = suppressWarnings(as.numeric(response))) |>
    group_by(question_slug, question, response_type, Demographic, Group) |>
    summarise(denom   = sum(count),
              pos     = sum(count[is_pos]),
              num_sum = sum(resp_num * count, na.rm = TRUE),
              num_den = sum(count[!is.na(resp_num)]),
              .groups = "drop") |>
    mutate(is_pref    = str_detect(str_to_lower(Group), "prefer not to say"),
           suppressed = denom < MIN_N,
           pct        = if_else(suppressed, NA_real_, 100 * pos / denom),
           mean_score = if_else(response_type == "scale" & !suppressed & num_den > 0,
                                num_sum / num_den, NA_real_),
           metric     = if_else(response_type == "binary",
                                "Felt may have to leave course (% yes)",
                                "Funding important (% rating 4-5)")) |>
    select(question_slug, question, metric, response_type,
           demog = Demographic, Demographic, Group, is_pref,
           n = denom, pos, pct, mean_score, suppressed) |>
    arrange(question_slug, demog, desc(pct))

  out_csv <- file.path(derived_dir(), "funding_triangle_rates.csv")
  write_csv(rates, out_csv)

  # ---- console sanity: overall rate per question ------------------------------
  message(sprintf("Written: %s  (%d group-rates)", out_csv, nrow(rates)))
  overall <- rates |>
    filter(!is_pref, !suppressed) |>
    group_by(question_slug, metric) |>
    summarise(groups = n(),
              wtd_avg_pct = round(weighted.mean(pct, n), 1),
              n_students  = sum(n), .groups = "drop")
  message("\n-- n-weighted average rate per question (non-response & PNTS excluded) --")
  print(as.data.frame(overall), row.names = FALSE)
  message("\n-- head of rates --")

  invisible(TRUE)
}
