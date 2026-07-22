# ===========================================================================
# scripts/d4_parse_funding_triangle.r
#
# Parse the demographic cross-tab workbook for the funding-decision triangle:
#   LEAVE_COURSE             - retention risk   (Yes/No)
#   FUNDING_INFLUENCE_COURSE - dependence, what (1-5)
#   FUNDING_INFLUENCE_HEI    - dependence, where (1-5)
# into ONE faithful long table. Response labels are read verbatim; no
# interpretation of codes happens here.
#
# Reuses functions/stacked_tables.r + grid_utils.r (same parser as the
# financial-confidence work, d1). Differences handled here:
#   - THREE sheets, one question each (sheet name -> question via a map).
#   - Sheets differ in WIDTH (yes/no = 4 cols; 1-5 scale = ~7). Width is
#     detected per sheet, not hardcoded.
#   - Each demographic block ends in a Grand Total ROW. That row is asserted
#     against the sum of its groups (catches a missing or double-counted
#     group) and then DROPPED so it never enters analysis.
#   - reconcile_check runs PER SHEET. Run on the combined table it merges any
#     demographic group that shares a denominator across two questions and
#     reports a false ~2x mismatch, so it stays inside parse_sheet.
#
#   source("scripts/d4_parse_funding_triangle.r", encoding = "UTF-8")
# Output -> derived_dir()/funding_triangle_long.csv   (secure, not the repo)
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(readr); library(readxl); library(dplyr)
  library(purrr);  library(stringr); library(tibble)
})

# ---- sheet -> question map -------------------------------------------------
# sheet_key is the sheet tab name, normalised (upper, non-alnum -> "_").
QUESTION_MAP <- tribble(
  ~sheet_key,                 ~question,                                                                      ~slug,                      ~response_type,
  "LEAVE_COURSE",             "Over the last year, did you ever feel that you may have to leave your course?", "leave_course",             "binary",
  "FUNDING_INFLUENCE_COURSE", "How important was funding in your decision on what to study?",                 "funding_influence_course", "scale",
  "FUNDING_INFLUENCE_HEI",    "How important was funding to your decision on where to study?",                "funding_influence_hei",    "scale"
)

norm_key <- function(x) {
  x |> str_to_upper() |> str_trim() |>
    str_replace_all("[^A-Z0-9]+", "_") |> str_replace_all("^_|_$", "")
}
is_total_row <- function(x) str_detect(str_to_lower(x), "grand *total|^total$")

# rightmost column carrying any content (so the Grand Total column is found
# whatever the sheet's width)
detect_ncol <- function(g) {
  hit <- vapply(seq_len(ncol(g)),
                function(j) any(!vapply(g[[j]], blank, logical(1))), logical(1))
  if (!any(hit)) NA_integer_ else max(which(hit))
}

# ---- locate workbook -------------------------------------------------------
# Specific pattern: the folder also holds the confidence workbook, and a loose
# "Questionnaire Analysis" pattern would match both. Keep this tight.
path   <- find_in_data("DHSC Questionnaire Analysis_v1.*\\.xlsx$")
sheets <- readxl::excel_sheets(path)
message("Reading: ", path)
message("Sheets:  ", paste(sheets, collapse = " | "))

MAX_ROW  <- 200L   # generous; blank trailing rows are ignored by the parser
SCAN_COL <- 8L     # read wide, then trim to the detected width per sheet

# ---- parse one sheet -------------------------------------------------------
parse_sheet <- function(sheet) {
  wide  <- read_grid(path, sheet = sheet, max_row = MAX_ROW, n_col = SCAN_COL)
  n_col <- detect_ncol(wide)
  if (is.na(n_col) || n_col < 3) {
    warning("Sheet '", sheet, "': <3 columns of content; skipped.", call. = FALSE)
    return(NULL)
  }
  grid <- wide[, seq_len(n_col), drop = FALSE]
  long <- extract_stacked_tables(grid, n_col = n_col)
  if (is.null(long) || !nrow(long)) {
    warning("Sheet '", sheet, "': no tables parsed; skipped.", call. = FALSE)
    return(NULL)
  }

  key  <- norm_key(sheet)
  meta <- filter(QUESTION_MAP, sheet_key == key)
  if (!nrow(meta)) {
    warning("Sheet '", sheet, "' (key ", key, ") not in QUESTION_MAP; ",
            "kept with question = sheet name.", call. = FALSE)
    meta <- tibble(sheet_key = key, question = sheet,
                   slug = str_to_lower(key), response_type = NA_character_)
  }

  long <- long |>
    mutate(sheet         = sheet,
           question      = meta[["question"]][1],
           question_slug = meta[["slug"]][1],
           response_type = meta[["response_type"]][1],
           .before = 1)

  # ---- grand-total ROW: assert, then drop (the double-count trap) ----------
  # One n per group lives on option_index == 1. Sum the groups in each block
  # and compare to the block's Grand Total row.
  check <- long |>
    mutate(.tot = is_total_row(Group)) |>
    filter(option_index == 1) |>
    group_by(Demographic) |>
    summarise(total_row = sum(n[.tot], na.rm = TRUE),
              groups    = sum(n[!.tot], na.rm = TRUE),
              .groups   = "drop") |>
    mutate(gap = groups - total_row)
  bad <- filter(check, total_row > 0, abs(gap) > 0.5)
  if (nrow(bad)) {
    message("!! GRAND-TOTAL MISMATCH on '", sheet,
            "' (a group may be missing or double-counted):")
    walk(seq_len(nrow(bad)), function(i)
      message(sprintf("     %s: groups sum %s vs grand total %s",
                      bad[["Demographic"]][i], bad[["groups"]][i], bad[["total_row"]][i])))
  } else {
    message("Grand-total rows reconcile on '", sheet, "'.")
  }

  data_long <- filter(long, !is_total_row(Group))

  # ---- faithful column reconcile, PER SHEET --------------------------------
  # Each row's option counts should sum to its own total. Run here, not on the
  # combined table, so shared-denominator groups across questions do not
  # produce spurious ~2x mismatches.
  message("-- column reconcile for '", sheet, "':")
  reconcile_check(data_long)

  data_long
}

# ---- run over the three sheets --------------------------------------------
long_all <- map(sheets, parse_sheet) |> compact() |> bind_rows()
if (!nrow(long_all)) stop("No sheets parsed. Check sheet names and layout.", call. = FALSE)

# ---- write (secure derived, not the repo) ---------------------------------
out_csv <- file.path(derived_dir(), "funding_triangle_long.csv")
write_csv(long_all, out_csv)
message(sprintf("Written: %s  (%d rows | %d questions | %d demographics)",
                out_csv, nrow(long_all),
                n_distinct(long_all[["question_slug"]]),
                n_distinct(long_all[["Demographic"]])))
print(head(long_all, 12))