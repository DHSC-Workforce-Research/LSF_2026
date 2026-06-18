# 98_extract_questionnaire.r  -  scrape the questionnaire, NO extra packages
library(dplyr); library(stringr); library(purrr); library(tibble); library(readr)
purrr::walk(list.files("functions", full.names = TRUE), source)

# --- auto-pick the Word doc ---------------------------------------------
docx_files <- list.files(Sys.getenv("LSF_DATA_DIR"), pattern = "\\.docx$",
                         recursive = TRUE, full.names = TRUE)
docx_files <- docx_files[!grepl("~\\$", docx_files)]
stopifnot("No .docx found under LSF_DATA_DIR" = length(docx_files) > 0)
pick     <- docx_files[grepl("question|survey|instrument", docx_files, ignore.case = TRUE)]
doc_path <- if (length(pick) >= 1) pick[1] else docx_files[1]
message("Using questionnaire: ", basename(doc_path))

# --- copy local first (forces OneDrive to hydrate the file), then unzip --
local_copy <- file.path(tempdir(), "questionnaire.docx")
ok <- file.copy(doc_path, local_copy, overwrite = TRUE)
stopifnot("Could not copy the .docx locally" = isTRUE(ok),
          "Copied file looks empty (OneDrive placeholder?)" = file.size(local_copy) > 1000)

xml_dir <- file.path(tempdir(), "docx_xml")
unzip(local_copy, files = "word/document.xml", exdir = xml_dir)
xml <- paste(readLines(file.path(xml_dir, "word", "document.xml"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "")

# --- rebuild one string per paragraph (<w:p>) from its <w:t> text runs ---
unescape <- function(x) x |>
  str_replace_all("&amp;", "&")  |> str_replace_all("&lt;", "<") |>
  str_replace_all("&gt;", ">")   |> str_replace_all("&quot;", '"') |>
  str_replace_all("&apos;", "'")

para_text <- str_split(xml, "</w:p>")[[1]] |>
  map_chr(\(p) {
    runs <- str_extract_all(p, "<w:t[^>]*>[^<]*</w:t>")[[1]]
    runs |> str_replace_all("<w:t[^>]*>|</w:t>", "") |> paste(collapse = "") |> unescape()
  }) |>
  str_squish()
para_text <- para_text[nzchar(para_text)]

# --- segment by the two section headings, carry the section down --------
fill_down <- function(x) Reduce(\(a, b) if (is.na(b)) a else b, x, accumulate = TRUE)

qs <- tibble(text = para_text) |>
  mutate(section = case_when(
    str_detect(text, regex("first year", ignore_case = TRUE)) ~ "first_year",
    str_detect(text, regex("continuing", ignore_case = TRUE)) ~ "continuing",
    TRUE                                                      ~ NA_character_),
    section = fill_down(section)) |>
  filter(!is.na(section),
         !str_detect(text, regex("first year|continuing", ignore_case = TRUE))) |>
  group_by(section) |>
  mutate(q_no = row_number()) |>
  ungroup() |>
  select(section, q_no, question_text = text)

print(qs, n = Inf)
write_csv(qs, file.path(derived_dir(), "lsf_questionnaire_extracted.csv"))

