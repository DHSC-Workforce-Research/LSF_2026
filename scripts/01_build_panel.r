# ===========================================================================
# 01_build_panel.r  -  stitch the four blocks into one panel, QA it, then
#                      reshape to a tidy student-year table and save both
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)

library(dplyr)
library(readr)

# --- bring the four blocks together, then clean -------------------------
wide <- stitch_panel(panel_files()) |> clean_lsf()

# --- QA before we trust it ----------------------------------------------
# Expect 290,947 rows and unique ids, 0 duplicates, ids 1 to 290,947, 169 cols.
print(check_panel(wide))

# --- reshape to one row per student-year --------------------------------
long <- reshape_long(wide)
cat("\nlong panel:", nrow(long), "student-year rows\n")
print(count(long, year))

# --- write to the secure derived folder, never the repo -----------------
write_csv(wide, file.path(derived_dir(), "lsf_panel_wide_2020_2026.csv"))
write_csv(long, file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"))
cat("\nwritten to", derived_dir(), "\n")
