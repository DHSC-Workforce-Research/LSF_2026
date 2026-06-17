# ---------------------------------------------------------------------------
# rate_by(): mean of a binary outcome by a grouping column, with relative risk
#
# Summarises an outcome (default the considered-leaving flag) as a rate within
# each level of `g`, alongside the cell count and a relative risk against the
# reference level. The reference is the first non-NA group, which is FALSE for
# a logical predictor and 1 for the 1-5 funding-importance scales, so the
# reference row reads rr = 1 and the rest are multiples of it. Pass `outcome`
# to point it at a different binary column (e.g. a gone-for-good flag) without
# touching the body.
# ---------------------------------------------------------------------------
rate_by <- function(df, g, outcome = "considered_leaving") {
  out <- df |>
    group_by(across(all_of(g))) |>
    summarise(n = n(), rate = mean(.data[[outcome]]), .groups = "drop")
  ref <- out[["rate"]][!is.na(out[[g]])][1]
  out |>
    mutate(rr   = round(rate / ref, 2),
           rate = round(rate, 3)) |>
    relocate(rr, .after = rate)
}