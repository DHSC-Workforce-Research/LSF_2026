library(dplyr); library(purrr); library(tibble); library(readr)

profile_cols <- function(df, top = 6) {
  n <- nrow(df)
  imap(df, \(x, nm) {
    vals     <- x[!is.na(x)]
    top_vals <- sort(table(vals), decreasing = TRUE) |> head(top)
    tibble(
      column     = nm,
      type       = class(x)[1],
      fill_pct   = round(100 * length(vals) / n, 1),
      n_distinct = dplyr::n_distinct(vals),
      top_values = paste(sprintf("%s (%s)", names(top_vals), top_vals), collapse = "  |  ")
    )
  }) |> list_rbind()
}

prof <- profile_cols(long)            # or profile_cols(sample)
View(prof)                            # sortable grid, now your session is attached
write_csv(prof, file.path(derived_dir(), "lsf_column_profile.csv"))