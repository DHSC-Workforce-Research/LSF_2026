# ===========================================================================
# 00_inspect.R  -  look at the panel files before trusting any of them
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)

# Structural summary of every block, one row each. Read this before stitching:
# the id_min / id_max should tile cleanly from 1 to 290,947.
panel_summary <- panel_files() |>
  purrr::map(inspect_lsf) |>
  purrr::list_rbind()

print(panel_summary)