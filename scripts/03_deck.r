# ===========================================================================
# scripts/03_deck.r
#
# STAGE 3 of the pipeline: tidy result tables -> the presented deck. This
# script fits nothing. It walks functions/deck_manifest.r in order, reads only
# the CSVs each row names, calls that row's builder, and saves the result as
#
#   outputs_dir()/deck/slide_NN_slug.png
#
# NN comes from the manifest and nowhere else. Reordering the deck is one edit
# to the manifest plus a re-run of this script; no plotting code changes.
#
#   source("scripts/03_deck.r")
#
# Run after 02_analysis.r. A row whose builder does not exist yet, or whose
# source table is missing, is reported and skipped: the deck still builds from
# whatever is ready, and the summary at the end says exactly what was left out.
# Nothing is silently dropped.
# ===========================================================================

source("scripts/00_config.r")

suppressMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(purrr); library(ggplot2); library(tibble)
})

man <- deck_manifest()
progress("03_deck: ", nrow(man), " slides in the manifest -> ", deck_dir())

built <- character(0)
skipped <- tibble(number = integer(0), slug = character(0), reason = character(0))

for (i in seq_len(nrow(man))) {
  row <- man[i, ]
  tag <- sprintf("%02d %s", row$number, row$slug)

  if (!exists(row$builder, mode = "function")) {
    skipped <- bind_rows(skipped, tibble(number = row$number, slug = row$slug,
                                         reason = paste0("no builder ", row$builder, "()")))
    next
  }

  res <- tryCatch({
    tables <- deck_tables(row$source_tables)
    p <- get(row$builder, mode = "function")(tables)
    f <- slide_file(row$number, row$slug)
    save_slide(p, f)
    basename(f)
  }, error = function(e) {
    skipped <<- bind_rows(skipped, tibble(number = row$number, slug = row$slug,
                                          reason = conditionMessage(e)))
    NULL
  })

  if (!is.null(res)) built <- c(built, res)
}

cat("\n=== DECK ===\n")
cat(sprintf("built %d of %d slides -> %s\n", length(built), nrow(man), deck_dir()))
if (length(built)) cat(paste0("  ", built, collapse = "\n"), "\n", sep = "")
if (nrow(skipped)) {
  cat(sprintf("\nNOT BUILT (%d):\n", nrow(skipped)))
  for (i in seq_len(nrow(skipped)))
    cat(sprintf("  %02d %-34s %s\n", skipped$number[i], skipped$slug[i], skipped$reason[i]))
} else {
  cat("\nevery manifest slide built.\n")
}
