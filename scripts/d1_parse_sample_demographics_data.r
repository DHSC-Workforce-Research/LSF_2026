purrr::walk(list.files("functions", full.names = TRUE), source)
library(readr); library(readxl)

path <- find_in_data("Questionnaire Analysis.*\\.xlsx$")
message("Reading: ", path)

grid <- read_grid(path, max_row = 151, n_col = 7)
long <- extract_stacked_tables(grid, n_col = 7)
reconcile_check(long)
print(head(long, 12))

write_csv(long, file.path(derived_dir(), "financial_confidence_long.csv"))
message("Written: ", file.path(derived_dir(), "financial_confidence_long.csv"))