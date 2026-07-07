# ---------------------------------------------------------------------------
# Questionnaire confidence extractor  (LSF 2026 project)
#
# Reads the "Questionnaire Analysis" workbook from the 2026 Data Share folder
# (data_dir(), top level, NOT _derived), auto-detects every stacked
# demographic table, and computes for each demographic GROUP the
# % Unconfident (1-2), % Neutral (3) and % Confident (4-5), using that
# group's own total as the denominator.
#
# Output: confidence_by_demographic.csv in outputs_dir(), so you can read it
# back with  rd("confidence_by_demographic.csv").
#
# Run from the project root (same as the panel scripts).
#
# FORMAT ASSUMED (from the sheet you described)
#   - Data is in columns A:G only. Column H onwards (pre-programmed %s) ignored.
#   - Each block = a LABEL cell (e.g. "Age") with an empty row to its right;
#     the count table starts a few rows below it.
#   - Table columns: A = group, B..F = ratings 1..5, G = Grand Total.
#   - A data row = any row with text in A and a NUMBER in G.
# ---------------------------------------------------------------------------

purrr::walk(list.files("functions", full.names = TRUE), source)
library(readr); library(readxl); library(stringr)

# ---- CONFIG ---------------------------------------------------------
MAX_ROW  <- 151   # last row with table data (Grand Total of the region table)
N_COL    <- 7     # columns A:G
OUT_FILE <- "confidence_by_demographic.csv"
# ---------------------------------------------------------------------

# Locate the questionnaire workbook at the top level of the data share.
# list.files() is non-recursive, so _derived is never searched.
questionnaire_file <- function() {
  f <- list.files(data_dir(), pattern = "Questionnaire Analysis.*\\.xlsx$",
                  full.names = TRUE)
  if (length(f) == 0)
    stop("No 'Questionnaire Analysis*.xlsx' found in ", data_dir(), call. = FALSE)
  sort(f)[length(f)]   # newest by filename if there is more than one
}

FILE_PATH <- questionnaire_file()
message("Reading: ", FILE_PATH)

# Read A1:G{MAX_ROW} as a plain grid, no headers, everything as text
grid <- read_excel(
  FILE_PATH, sheet = 1, col_names = FALSE,
  range = paste0("A1:G", MAX_ROW), col_types = "text",
  .name_repair = "minimal"
)
grid <- as.data.frame(grid, stringsAsFactors = FALSE)

blank <- function(x) is.na(x) || trimws(as.character(x)) == ""
num   <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))

# A demographic LABEL row: text in A, everything else in B:G empty
is_label <- function(r) {
  !blank(grid[r, 1]) && all(vapply(2:N_COL, function(j) blank(grid[r, j]), logical(1)))
}
label_rows <- Filter(is_label, seq_len(nrow(grid)))

out       <- list()
warn_rows <- character(0)

for (i in seq_along(label_rows)) {
  lr    <- label_rows[i]
  demog <- trimws(as.character(grid[lr, 1]))
  end   <- if (i < length(label_rows)) label_rows[i + 1] - 1 else nrow(grid)

  for (r in (lr + 1):end) {
    if (blank(grid[r, 1])) next
    g <- num(grid[r, 7])            # Grand Total column
    if (is.na(g) || g <= 0) next    # skips header rows ("Grand Total" text) and blanks

    group <- trimws(as.character(grid[r, 1]))
    b  <- num(grid[r, 2]); c2 <- num(grid[r, 3]); d <- num(grid[r, 4])
    e  <- num(grid[r, 5]); f  <- num(grid[r, 6])

    unconf <- sum(c(b, c2), na.rm = TRUE)
    neut   <- sum(c(d),     na.rm = TRUE)
    conf   <- sum(c(e, f),  na.rm = TRUE)
    check  <- sum(c(b, c2, d, e, f), na.rm = TRUE)

    if (abs(check - g) > 0.5)
      warn_rows <- c(warn_rows,
        sprintf("  %s / %s: ratings sum to %s but Grand Total = %s", demog, group, check, g))

    out[[length(out) + 1]] <- data.frame(
      Demographic     = demog,
      Group           = group,
      n               = g,
      Unconfident_1_2 = unconf,
      Neutral_3       = neut,
      Confident_4_5   = conf,
      Unconfident_pct = round(100 * unconf / g, 1),
      Neutral_pct     = round(100 * neut   / g, 1),
      Confident_pct   = round(100 * conf   / g, 1),
      is_total        = grepl("grand total|^total$", tolower(group)),
      stringsAsFactors = FALSE
    )
  }
}

result <- do.call(rbind, out)

cat("\n==== Confidence by demographic ====\n\n")
print(result, row.names = FALSE)

if (length(warn_rows) > 0) {
  cat("\n!! RECONCILE WARNINGS (check the column mapping on these rows):\n")
  cat(paste(warn_rows, collapse = "\n"), "\n")
} else {
  cat("\nAll rows reconcile (ratings 1-5 sum to the Grand Total). Column mapping looks right.\n")
}

write_csv(result, file.path(outputs_dir(), OUT_FILE))
cat("\nWritten:", file.path(outputs_dir(), OUT_FILE), "\n")