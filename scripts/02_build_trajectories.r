# ===========================================================================
# 02_build_trajectories.r  -  collapse the long student-year panel into one
#                             trajectory row per student, QA it, then save
# ===========================================================================
purrr::walk(list.files("functions", full.names = TRUE), source)
library(dplyr)
library(readr)

# rebuild the long panel from source (or read the derived CSV if you prefer)
wide <- stitch_panel(panel_files()) |> clean_lsf()
long <- reshape_long(wide)

# --- collapse to trajectories -------------------------------------------
traj <- build_trajectories(long)

# --- QA: one row per student, ids intact --------------------------------
# Expect 290,947 rows and 290,947 distinct ids (one row per student).
cat("trajectory rows:", nrow(traj),
    "| distinct ids:", n_distinct(traj$UniqueID), "\n\n")

# the headline signals
cat("waves answered per student:\n");        print(count(traj, n_waves))
cat("\nentry (first wave observed):\n");      print(count(traj, first_wave))
cat("\never considered leaving:\n");          print(count(traj, ever_considered_leaving))

# --- write to the secure derived folder ---------------------------------
write_csv(traj, file.path(derived_dir(), "lsf_trajectories_2020_2026.csv"))
cat("\nwritten to", derived_dir(), "\n")