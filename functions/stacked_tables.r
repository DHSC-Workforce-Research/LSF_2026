# Parse a sheet of stacked demographic cross-tabs into FAITHFUL long form.
# No interpretation of the response codes happens here. It records the
# response labels exactly as the sheet has them, plus their column position.
#
# Layout assumed:
#   * Each block starts with a LABEL row: text in column A, B..n_col empty
#     (e.g. "Age", "Gender", "Region").
#   * A DATA row has text in column A (the group, e.g. "16-24") and a number
#     in the last column (Grand Total).
#   * Columns B..(n_col-1) are the response options; last column is the total.

find_label_rows <- function(grid, n_col) {
  Filter(function(r) {
    !blank(grid[r, 1]) &&
      all(vapply(2:n_col, function(j) blank(grid[r, j]), logical(1)))
  }, seq_len(nrow(grid)))
}

# Long data.frame: Demographic, Group, option_index, response, count, n
#   option_index = 1..k position of the option column (stable key for analysis)
#   response     = the option label read verbatim from the sheet header
extract_stacked_tables <- function(grid, n_col) {
  labels    <- find_label_rows(grid, n_col)
  opt_cols  <- 2:(n_col - 1)   # response option columns
  total_col <- n_col           # Grand Total column
  rows <- list()

  for (i in seq_along(labels)) {
    lr    <- labels[i]
    demog <- trimws(as.character(grid[lr, 1]))
    end   <- if (i < length(labels)) labels[i + 1] - 1 else nrow(grid)

    # data rows in this block = rows with a number in the total column
    data_rows <- Filter(function(r) {
      !blank(grid[r, 1]) &&
        !is.na(num(grid[r, total_col])) && num(grid[r, total_col]) > 0
    }, (lr + 1):end)
    if (length(data_rows) == 0) next

    # header row = nearest non-blank row above the first data row; take the
    # response labels straight from it (walk up if the row above is a spacer)
    hdr <- data_rows[1] - 1
    while (hdr > lr && all(vapply(opt_cols, function(j) blank(grid[hdr, j]), logical(1))))
      hdr <- hdr - 1
    responses <- vapply(opt_cols, function(j) trimws(as.character(grid[hdr, j])), character(1))
    missing   <- is.na(responses) | responses == ""
    responses[missing] <- paste0("opt", which(missing))   # fallback if header blank

    for (r in data_rows) {
      rows[[length(rows) + 1]] <- data.frame(
        Demographic  = demog,
        Group        = trimws(as.character(grid[r, 1])),
        option_index = seq_along(opt_cols),
        response     = responses,
        count        = vapply(opt_cols, function(j) num(grid[r, j]), numeric(1)),
        n            = num(grid[r, total_col]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Warn if a group's option counts do not sum to its stated Grand Total.
reconcile_check <- function(long) {
  agg <- stats::aggregate(count ~ Demographic + Group + n, long, sum)
  bad <- agg[abs(agg$count - agg$n) > 0.5, ]
  if (nrow(bad) == 0) {
    cat("All groups reconcile: option counts sum to the Grand Total.\n")
  } else {
    cat("!! RECONCILE WARNINGS (check column mapping):\n")
    for (k in seq_len(nrow(bad)))
      cat(sprintf("  %s / %s: sum %s vs total %s\n",
                  bad$Demographic[k], bad$Group[k], bad$count[k], bad$n[k]))
  }
  invisible(bad)
}