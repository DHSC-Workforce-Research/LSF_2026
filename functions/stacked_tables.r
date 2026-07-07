# Parse a sheet of stacked demographic cross-tabs into tidy long form.
#
# Layout assumed:
#   * Each block starts with a LABEL row: text in column A, B..n_col empty
#     (e.g. "Age", "Gender", "Region").
#   * A DATA row has text in column A (the group, e.g. "16-24") and a number
#     in the last column (Grand Total).
#   * Columns B..(n_col-1) are the rating scale 1..k; the last column is the
#     group total. Columns beyond n_col are dropped by read_grid().

# Rows that are a lone demographic label (text in A, nothing in B..n_col)
find_label_rows <- function(grid, n_col) {
  Filter(function(r) {
    !blank(grid[r, 1]) &&
      all(vapply(2:n_col, function(j) blank(grid[r, j]), logical(1)))
  }, seq_len(nrow(grid)))
}

# Long data.frame: Demographic, Group, rating (1..k), count, n (group total)
extract_stacked_tables <- function(grid, n_col) {
  labels    <- find_label_rows(grid, n_col)
  ratings   <- 2:(n_col - 1)   # columns holding the 1..k scale
  total_col <- n_col           # last column = Grand Total
  rows <- list()

  for (i in seq_along(labels)) {
    lr    <- labels[i]
    demog <- trimws(as.character(grid[lr, 1]))
    end   <- if (i < length(labels)) labels[i + 1] - 1 else nrow(grid)

    for (r in (lr + 1):end) {
      if (blank(grid[r, 1])) next
      n <- num(grid[r, total_col])
      if (is.na(n) || n <= 0) next  # skip header ("Grand Total" text) / blank rows

      rows[[length(rows) + 1]] <- data.frame(
        Demographic = demog,
        Group       = trimws(as.character(grid[r, 1])),
        rating      = seq_along(ratings),
        count       = vapply(ratings, function(j) num(grid[r, j]), numeric(1)),
        n           = n,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Warn if a group's ratings do not sum to its stated Grand Total. Quickest way
# to catch a mis-mapped column. Prints; returns the offending rows invisibly.
reconcile_check <- function(long) {
  agg <- stats::aggregate(count ~ Demographic + Group + n, long, sum)
  bad <- agg[abs(agg$count - agg$n) > 0.5, ]
  if (nrow(bad) == 0) {
    cat("All groups reconcile: rating counts sum to the Grand Total.\n")
  } else {
    cat("!! RECONCILE WARNINGS (check column mapping):\n")
    for (k in seq_len(nrow(bad)))
      cat(sprintf("  %s / %s: sum %s vs total %s\n",
                  bad$Demographic[k], bad$Group[k], bad$count[k], bad$n[k]))
  }
  invisible(bad)
}