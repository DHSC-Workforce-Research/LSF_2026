# Utilities for reading and cleaning messy Excel grids.

# TRUE if a cell is missing or whitespace-only
blank <- function(x) is.na(x) || trimws(as.character(x)) == ""

# Robust numeric coercion: strips commas, %, stray text; "" -> NA
num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))

# Read an .xlsx range as a plain text grid (no headers, all character), so
# downstream parsing is not fooled by readxl's per-column type guessing.
read_grid <- function(path, sheet = 1, max_row, n_col) {
  g <- readxl::read_excel(
    path, sheet = sheet, col_names = FALSE,
    range = paste0("A1:", LETTERS[n_col], max_row),
    col_types = "text", .name_repair = "minimal"
  )
  as.data.frame(g, stringsAsFactors = FALSE)
}

# Newest data file in the secure share matching a regex. Non-recursive, so it
# never descends into _derived. Complements panel_files().
find_in_data <- function(pattern) {
  f <- list.files(data_dir(), pattern = pattern, full.names = TRUE)
  if (length(f) == 0) stop("No file matching '", pattern, "' in ", data_dir(), call. = FALSE)
  sort(f)[length(f)]
}