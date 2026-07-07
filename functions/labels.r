# ANALYSIS-STAGE relabelling. Turns the verbatim survey questions and long
# category labels into short, chart-ready names, and drops non-response rows.
# The _derived long table stays untouched; this is applied downstream.

# Full survey question -> short demographic name (keyword match, order matters)
short_demographic <- function(x) {
  dplyr::case_when(
    grepl("ethnic",                         x, ignore.case = TRUE) ~ "Ethnicity",
    grepl("religion",                       x, ignore.case = TRUE) ~ "Religion",
    grepl("marital",                        x, ignore.case = TRUE) ~ "Marital status",
    grepl("sexual",                         x, ignore.case = TRUE) ~ "Sexual orientation",
    grepl("gender",                         x, ignore.case = TRUE) ~ "Gender",
    grepl("trans person",                   x, ignore.case = TRUE) ~ "Trans",
    grepl("disab|day-to-day|health problem",x, ignore.case = TRUE) ~ "Disability",
    grepl("pregnan|maternity",              x, ignore.case = TRUE) ~ "Pregnancy/maternity",
    grepl("age",                            x, ignore.case = TRUE) ~ "Age",
    TRUE ~ x
  )
}

# Exact long-label -> short-label map. Anything not listed passes through.
group_map <- c(
  # Age
  "16-24 years" = "16-24", "25-34 years" = "25-34", "35-44 years" = "35-44",
  "45-54 years" = "45-54", "55-64 years" = "55-64",
  # Ethnicity
  "White: English, Scottish, Welsh, Northern Irish, British" = "White British",
  "White: Other" = "White other", "White: Irish" = "White Irish",
  "Black/Black British: African"   = "Black African",
  "Black/Black British: Caribbean" = "Black Caribbean",
  "Black/Black British: Other"     = "Black other",
  "Asian/Asian British: Indian"      = "Indian",
  "Asian/Asian British: Pakistani"   = "Pakistani",
  "Asian/Asian British: Bangladeshi" = "Bangladeshi",
  "Asian/Asian British: Chinese"     = "Chinese",
  "Asian/Asian British: Other"       = "Asian other",
  "Mixed: White and Black Caribbean" = "Mixed White/Black Caribbean",
  "Mixed: White and Black African"   = "Mixed White/Black African",
  "Mixed: White and Asian"           = "Mixed White/Asian",
  "Mixed: Other"                     = "Mixed other",
  "Any other ethnic group"           = "Other ethnic group",
  # Religion
  "Christianity (including Church of england, Catholic, Protestant and all other Christian denominations" = "Christian",
  "Atheism/no religion" = "No religion", "Islam" = "Muslim",
  "Hinduism" = "Hindu", "Sikhism" = "Sikh", "Buddhism" = "Buddhist",
  "Judaism" = "Jewish", "Any other religion" = "Other religion",
  # Disability
  "Not at all" = "Not limited", "Yes, a little" = "Limited a little",
  "Yes, a lot" = "Limited a lot",
  # Gender / Sexual orientation
  "I prefer to use another term" = "Other term",
  "Heterosexual/straight" = "Heterosexual", "Bi/bisexual" = "Bisexual"
)

relabel <- function(x, map) {
  hit <- map[x]
  ifelse(is.na(hit), x, unname(hit))
}

# Apply the whole tidy: drop non-response rows, shorten labels.
#   drop_nonresponse : remove "(blank)" and "NULL" (item non-response)
#   drop_total       : remove per-demographic "Grand Total"
#   drop_pnts        : remove "Prefer not to say"
#   min_n            : suppress groups below this size (disclosure control)
tidy_groups <- function(long,
                        drop_nonresponse = TRUE,
                        drop_total       = TRUE,
                        drop_pnts        = TRUE,
                        min_n            = 0) {
  drop <- character(0)
  if (drop_nonresponse) drop <- c(drop, "(blank)", "NULL")
  if (drop_total)       drop <- c(drop, "Grand Total")
  if (drop_pnts)        drop <- c(drop, "Prefer not to say")

  long <- long[!(long$Group %in% drop), , drop = FALSE]
  if (min_n > 0) long <- long[long$n >= min_n, , drop = FALSE]

  long$Demographic <- short_demographic(long$Demographic)
  long$Group       <- relabel(long$Group, group_map)
  long
}