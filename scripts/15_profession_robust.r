# =====================================================================
# 15_profession_robust.r  -  is the profession leaving gap real, or a
# course-length artefact? Compare three measures side by side.
# Run:  source("scripts/15_profession_robust.r")
# =====================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(stringr)
  purrr::walk(list.files("functions", full.names = TRUE), source)
}))
smp <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_3spec_sample.rds")))

prof_of <- function(x) { x <- tolower(dplyr::coalesce(x, ""))
  dplyr::case_when(
    str_detect(x, "midwif")                          ~ "Midwifery",
    str_detect(x, "mental health")                   ~ "Nursing - Mental Health",
    str_detect(x, "learning disab")                  ~ "Nursing - Learning Disability",
    str_detect(x, "child")                           ~ "Nursing - Children's",
    str_detect(x, "adult") & str_detect(x, "nurs")   ~ "Nursing - Adult",
    str_detect(x, "nurs")                            ~ "Nursing - other/dual",
    str_detect(x, "physio")                          ~ "Physiotherapy",
    str_detect(x, "occupational")                    ~ "Occupational Therapy",
    str_detect(x, "paramedic")                       ~ "Paramedic",
    str_detect(x, "radiograph|radiother")            ~ "Radiography",
    str_detect(x, "speech|language")                 ~ "Speech & Language Therapy",
    str_detect(x, "diet")                            ~ "Dietetics",
    str_detect(x, "podiat|chiropod")                 ~ "Podiatry",
    str_detect(x, "operating department|\\bodp\\b")  ~ "ODP",
    TRUE                                             ~ "Other AHP")
}
smp$prof <- prof_of(smp$course)

profession_robust <- smp |> group_by(prof) |>
  summarise(
    n                  = n(),
    left_before_finish = round(100 * mean(left_before_finish, na.rm = TRUE), 1), # length-dependent
    one_wave_only      = round(100 * mean(one_wave_only,      na.rm = TRUE), 1),  # anchor-free
    considered_leaving = round(100 * mean(considered_leaving, na.rm = TRUE), 1),  # self-report
    .groups = "drop") |>
  arrange(desc(left_before_finish))

print(as.data.frame(profession_robust), row.names = FALSE)
message("object: profession_robust")