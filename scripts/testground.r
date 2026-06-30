# ===========================================================================
# 12_reconcile.r  -  one-shot grid for findings review
#   Mirrors 10_communicate.r's load (functions + derived_dir + build sample),
#   then runs EVERY funding predictor x EVERY leaving definition as a logistic
#   with course + cohort fixed effects. Prints: predictor coding, the full
#   OR grid (OR, 95% CI, p, n, event rate), an OR matrix, and a schema dump of
#   `sample` so column names are visible. Writes one CSV. Reads only.
# ===========================================================================
library(dplyr); library(readr); library(stringr); library(purrr)
purrr::walk(list.files("functions", full.names = TRUE), source)

long   <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
traj   <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
sample <- build_funding_leaving_sample(long, traj) |> define_leaving_outcomes()
sample <- as.data.frame(sample)   # plain data frame for base glm

# ---- config ----
out_path   <- file.path(derived_dir(), "lsf_findings_for_review.csv")
likert_cut <- 4   # dependence Likerts binarised at >= 4 to match the recorded grid

predictor_candidates <- c("fund_availability","grant_influence",
                          "funding_imp_crse","funding_imp_uni","grant_helps_stay")
outcome_candidates   <- c("reached_finish","left_before_finish","left_final_year_only",
                          "left_2y_plus_early","left_3y_plus_early","left_after_year1",
                          "one_wave_only","considered_first","considered_ever",
                          "considered_and_left","considered_but_stayed")
course_candidates    <- c("course","course_name","programme","programme_name")
cohort_candidates    <- c("cohort","cohort_year","entry_year",
                          "course_first_year_wave","first_year_wave")

pick_first <- function(cands, df) { h <- intersect(cands, names(df)); if (length(h)) h[1] else NA }
fe_course  <- pick_first(course_candidates, sample)
fe_cohort  <- pick_first(cohort_candidates, sample)

# ---- schema dump of `sample` (so the real column names are visible) ----
cat("\n=====================================================\n")
cat("SCHEMA: sample  rows:", nrow(sample), " cols:", ncol(sample), "\n")
cat("=====================================================\nAll columns:\n")
print(names(sample))
cat("\nFE picked -> course:", fe_course, " | cohort:", fe_cohort, "\n")
hits <- grep("confid|surviv|year2|wave|n_waves|finish|consider|fund|grant|cohort|course",
             names(sample), ignore.case = TRUE, value = TRUE)
for (col in hits) {
  v <- sample[[col]]
  cat("\n--", col, "-- (class:", class(v)[1], ", NA:", sum(is.na(v)), ")\n")
  if (is.numeric(v) && length(unique(v[!is.na(v)])) > 12) print(summary(v)) else print(table(v, useNA = "ifany"))
}

# ---- coerce predictors to a 0/1 exposure, recording the coding ----
preds <- intersect(predictor_candidates, names(sample))
outs  <- intersect(outcome_candidates,  names(sample))
cat("\nPredictors found:", paste(preds, collapse = ", "), "\n")
cat("Outcomes found:  ", paste(outs,  collapse = ", "), "\n")

coding <- data.frame(predictor = character(), rule = character())
to_binary <- function(v, nm) {
  u <- sort(unique(v[!is.na(v)]))
  if (is.logical(v))    { coding[nrow(coding)+1,] <<- c(nm, "TRUE vs FALSE");            return(as.integer(v)) }
  if (length(u) <= 2)   { coding[nrow(coding)+1,] <<- c(nm, paste("==", max(u), "vs else")); return(as.integer(v == max(u))) }
  if (max(u) <= 5)      { coding[nrow(coding)+1,] <<- c(nm, paste(">=", likert_cut, "vs <", likert_cut)); return(as.integer(v >= likert_cut)) }
  coding[nrow(coding)+1,] <<- c(nm, "above median vs at/below"); as.integer(v > median(v, na.rm = TRUE))
}
for (p in preds) sample[[paste0(p, "__bin")]] <- to_binary(sample[[p]], p)

# ---- the grid: one logistic per (outcome, predictor) with course + cohort FE ----
fe_terms <- c(if (!is.na(fe_course)) paste0("factor(`", fe_course, "`)"),
              if (!is.na(fe_cohort)) paste0("factor(`", fe_cohort, "`)"))
fe_rhs <- if (length(fe_terms)) paste("+", paste(fe_terms, collapse = " + ")) else ""

rows <- list()
for (o in outs) for (p in preds) {
  pb <- paste0(p, "__bin")
  keep <- c(o, pb, if (!is.na(fe_course)) fe_course, if (!is.na(fe_cohort)) fe_cohort)
  d <- sample[stats::complete.cases(sample[, keep, drop = FALSE]), keep, drop = FALSE]
  d[[o]] <- as.integer(as.logical(d[[o]]))
  res <- list(outcome = o, predictor = p, n = nrow(d),
              event_rate = round(mean(d[[o]], na.rm = TRUE), 3),
              OR = NA, CI_low = NA, CI_high = NA, p = NA, note = "")
  if (length(unique(d[[pb]])) != 2 || length(unique(d[[o]])) != 2 || nrow(d) < 50) {
    res$note <- "skipped: no variation / too few"; rows[[length(rows)+1]] <- res; next
  }
  fit <- tryCatch(glm(as.formula(paste0("`", o, "` ~ ", pb, fe_rhs)), family = binomial, data = d),
                  error = function(e) NULL)
  if (is.null(fit)) { res$note <- "glm failed"; rows[[length(rows)+1]] <- res; next }
  sm <- summary(fit)$coefficients
  if (pb %in% rownames(sm)) {
    est <- sm[pb, "Estimate"]; se <- sm[pb, "Std. Error"]
    res$OR     <- round(exp(est), 3)
    res$CI_low <- round(exp(est - 1.96 * se), 3)
    res$CI_high<- round(exp(est + 1.96 * se), 3)
    res$p      <- signif(sm[pb, "Pr(>|z|)"], 3)
  }
  rows[[length(rows)+1]] <- res
}
grid <- do.call(rbind, lapply(rows, as.data.frame))

# ---- output ----
cat("\n=====================================================\nPREDICTOR CODING USED\n")
cat("=====================================================\n"); print(coding)

cat("\n=====================================================\n")
cat("ODDS-RATIO GRID (OR > 1 = more likely to have that outcome)\n")
cat("=====================================================\n")
print(grid[order(grid$predictor, grid$outcome), ], row.names = FALSE)

or_wide <- tryCatch(reshape(grid[, c("predictor","outcome","OR")], idvar = "predictor",
                            timevar = "outcome", direction = "wide"), error = function(e) NULL)
if (!is.null(or_wide)) {
  names(or_wide) <- sub("^OR\\.", "", names(or_wide))
  cat("\nOR matrix (predictor x outcome):\n"); print(or_wide, row.names = FALSE)
}

write.csv(grid, out_path, row.names = FALSE)
cat("\nWritten:", out_path, "\n")
cat("Paste me: the SCHEMA column list, the PREDICTOR CODING table, and the OR matrix.\n")