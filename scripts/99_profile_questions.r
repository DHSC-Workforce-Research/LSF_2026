# ===========================================================================
# 99_profile_questions.r  -  empirically profile every question column so we
#   can SEE what each question is from its answers, and emit a data-dictionary
#   scaffold to fill in from the questionnaire.
# ===========================================================================
library(dplyr); library(readr); library(purrr); library(tibble)
purrr::walk(list.files("functions", full.names = TRUE), source)

long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"),
                 show_col_types = FALSE)

question_cols <- setdiff(names(long), c("UniqueID", "year"))

profile <- question_cols |>
  map(\(nm) {
    col  <- long[[nm]]
    vals <- col[!is.na(col)]
    top  <- head(sort(table(vals), decreasing = TRUE), 6)
    tibble(
      column       = nm,
      n_answered   = length(vals),
      pct_answered = round(length(vals) / length(col), 3),
      n_distinct   = n_distinct(vals),
      top_values   = paste0(names(top), " (", as.integer(top), ")", collapse = " | ")
    )
  }) |>
  list_rbind()

print(profile, n = Inf, width = Inf)

# full empirical profile -> secure _derived (top_values may hold free text, keep secure)
write_csv(profile, file.path(derived_dir(), "lsf_question_profile.csv"))

# a dictionary scaffold: one row per column, blank fields to fill from the questionnaire
profile |>
  transmute(column, pct_answered, n_distinct,
            question_text  = NA_character_,   # paste verbatim from questionnaire
            answer_options = NA_character_,
            routing        = NA_character_,   # e.g. "continuing students only"
            years_asked    = NA_character_,
            analyst_notes  = NA_character_) |>
  write_csv(file.path(derived_dir(), "lsf_data_dictionary_SCAFFOLD.csv"))

cat("\nwritten profile + scaffold to", derived_dir(), "\n")