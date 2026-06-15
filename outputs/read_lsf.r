# ---------------------------------------------------------------------------
# read_lsf(): read a single LSF workbook, sheet 1, as plain text
#
# Everything comes back as character on purpose. The four blocks disagree on
# headers, blanks and "True"/"False" flags, and if readxl guesses types it will
# guess differently across the files and the bind will break. So I read it all
# as text now and decide types once, later, in clean_lsf().
# ---------------------------------------------------------------------------

read_lsf <- function(file, has_header = FALSE) {
  readxl::read_excel(
    path         = file,
    sheet        = 1,
    col_names    = has_header,
    col_types    = "text",
    .name_repair = "minimal"
  )
}