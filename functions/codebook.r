# ===========================================================================
# codebook.r  -  single source of truth for variable -> question mapping.
#   One row per column. label_short drives chart/table labels; question_text
#   is the verbatim survey wording. Helpers: lsf_lab(), lsf_q(),
#   codebook_coverage(). Edit HERE, never hardcode a label in a plot again.
# ===========================================================================
lsf_codebook <- tibble::tribble(
  ~raw_name,                   ~analysis_name,              ~population,  ~type,    ~label_short,                                ~question_text,
  "UniqueID",                  "UniqueID",                  "all",        "id",     "Student ID",                                 "Anonymised student identifier (admin)",
  "year",                      "year",                      "all",        "admin",  "Survey year",                                "Survey / claim year (admin)",
  "college",                   "college",                   "all",        "admin",  "University",                                 "University attended (admin)",
  "course",                    "course",                    "all",        "admin",  "Course",                                     "Course studied (admin)",
  "first_year",                "first_year",                "all",        "flag",   "First year",                                 "In first year of course? (drives question routing)",
  "fund_availability",         "fund_availability",         "first_year", "yesno",  "Aware of grant before applying",             "Were you aware of the availability of a grant prior to applying for a Nursing, Midwifery or Allied Health professional?",
  "grant_influence",           "grant_influence",           "first_year", "yesno",  "Grant influenced enrolment",                 "Did the availability of the grant influence your decision to apply and enter training?",
  "elements_awareness",        "elements_awareness",        "first_year", "multi",  "Elements aware of",                          "Which elements were you aware of?",
  "elements_awareness_other",  "elements_awareness_other",  "first_year", "empty",  "Elements aware of (other text)",             "Free-text 'Other' for elements awareness (empty in data)",
  "funding_influence_uni",     "funding_imp_uni",           "first_year", "scale5", "Funding critical to WHERE to study",         "How important was funding to your decision on where to study? (1-5)",
  "funding_influence_course",  "funding_imp_crse",          "first_year", "scale5", "Funding critical to WHAT to study",          "How important was funding in your decision on what to study? (1-5)",
  "alternative_course",        "alternative_course",        "first_year", "text",   "Alternative subjects considered",            "If you considered alternative subjects, please list them below.",
  "grant_difference",          "grant_difference",          "first_year", "multi",  "How the grant makes a difference",           "How will the Training Grant make a difference?",
  "grant_difference_other",    "grant_difference_other",    "first_year", "text",   "Grant difference (other text)",              "Free-text 'Other' for how the grant makes a difference",
  "concerns",                  "concerns",                  "first_year", "text",   "Other completion concerns",                  "If you have any other concerns that may affect your ability to complete the course, provide details here.",
  "grants_applied",            "grants_applied",            "first_year", "multi",  "Grant components applied for",               "Which grant component are you applying for?",
  "leave_course",              "considered_leaving",        "continuing", "yesno",  "Considered leaving",                         "Over the last year, did you ever feel that you may have to leave your course?",
  "leave_factors",             "leave_factors",             "continuing", "multi",  "Factors in considering leaving",             "What were the key factors involved? (branches off considered leaving == yes)",
  "leave_factors_other",       "leave_factors_other",       "continuing", "text",   "Leave factors (other text)",                 "Free-text 'Other' for key factors in considering leaving",
  "finish_on_time",            "finish_on_time",            "continuing", "yesno",  "Expects to finish on time",                  "Do you expect to complete your final year on time?",
  "extra_time_reason",         "extra_time_reason",         "continuing", "text",   "Reason for extra time",                      "What is the reason for taking extra time to complete your course?",
  "confidence",                "confidence",                "continuing", "scale5", "Financial confidence (cover living costs)",  "How confident are you about being able to cover living expenses in the next year? (1 = not confident at all, 5 = very confident)",
  "work_intensions",           "work_intensions",           "continuing", "multi",  "Intended sector after graduation",           "Where do you intend to work following graduation? (sic: source misspelling)",
  "work_intensions_other",     "work_intensions_other",     "continuing", "text",   "Work intentions (other text)",               "Free-text 'Other' for intended sector",
  "start_work_date",           "start_work_date",           "continuing", "year",   "Intended start-work year",                   "What year do you intend to start employment? (post-graduation, not course finish)",
  "grants_applied_continuing", "grants_applied_continuing", "continuing", "multi",  "Grant components (continuing)",              "Which grant component are you applying for? (continuing-student version)",
  # derived analysis variables (constructed in the pipeline, no raw column)
  NA_character_,               "grant_helps_stay",          "first_year", "derived","Grant helps me stay",                        "Derived from grant_difference: selected 'help me to stay on the course and complete my year'",
  NA_character_,               "left_early",                "derived",    "derived","Left the course early",                      "Derived from the trajectory classification (left before expected finish)"
) |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# --- helpers ---------------------------------------------------------------
.lsf_lab_map <- with(lsf_codebook, c(stats::setNames(label_short,   analysis_name),
                                     stats::setNames(label_short,   raw_name)))
.lsf_q_map   <- with(lsf_codebook, c(stats::setNames(question_text, analysis_name),
                                     stats::setNames(question_text, raw_name)))
lsf_lab <- function(x) { o <- unname(.lsf_lab_map[x]); ifelse(is.na(o), x, o) }  # short label, falls back to the name
lsf_q   <- function(x) unname(.lsf_q_map[x])                                     # verbatim question

codebook_coverage <- function(df) {
  known <- unique(stats::na.omit(c(lsf_codebook$raw_name, lsf_codebook$analysis_name)))
  miss  <- setdiff(names(df), known)
  if (length(miss)) message("NOT in codebook: ", paste(miss, collapse = ", "))
  else               message("All ", ncol(df), " columns mapped.")
  invisible(miss)
}