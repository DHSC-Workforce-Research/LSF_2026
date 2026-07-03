# =====================================================================
# testground.r  -  entry-time survey answers as a predictor of leaving
# Cross-validated AUC + decile lift. Numeric-safe AUC (no overflow, no pROC).
# Loads script-11's cached sample. Writes nothing.
# Run:  source("scripts/testground.r")   ->  prints ONE 3-row table `res`
# =====================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(readr)
  purrr::walk(list.files("functions", full.names = TRUE), source)
}))
options(width = 200); set.seed(1); K <- 5

cache_path <- file.path(derived_dir(), "lsf_3spec_sample.rds")
if (file.exists(cache_path)) {
  sample <- as.data.frame(readRDS(cache_path))
} else {
  long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
  traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
  sample <- as.data.frame(build_funding_leaving_sample(long, traj) |> define_leaving_outcomes())
}

# numeric-safe AUC (as.numeric prevents the integer overflow -> NA)
auc <- function(score, y) {
  ok <- !is.na(score) & !is.na(y); score <- score[ok]; y <- as.integer(y[ok])
  r <- rank(score); n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

term_survey <- c("fund_availability", "grant_influence",
                 "I(funding_imp_crse >= 4)", "I(funding_imp_uni >= 4)", "grant_helps_stay")
raw_cols    <- c("fund_availability", "grant_influence",
                 "funding_imp_crse", "funding_imp_uni", "grant_helps_stay", "entry_year")
outcomes    <- c("left_before_finish", "one_wave_only", "left_2y_plus_early")

cv_scores <- function(d, form) {
  n <- nrow(d); fold <- sample(rep(1:K, length.out = n)); oos <- rep(NA_real_, n)
  for (k in 1:K) {
    te <- which(fold == k)
    m  <- suppressWarnings(glm(as.formula(form), data = d[-te, ], family = binomial))
    oos[te] <- tryCatch(predict(m, newdata = d[te, ], type = "response"), error = function(e) NA_real_)
  }
  oos
}

rows <- lapply(outcomes, function(out) {
  d <- sample[, c(out, raw_cols), drop = FALSE]
  d[[out]] <- as.integer(as.logical(d[[out]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  f_svy <- paste(out, "~", paste(term_survey, collapse = " + "))
  f_yr  <- paste(f_svy, "+ factor(entry_year)")
  s_svy <- cv_scores(d, f_svy); s_yr <- cv_scores(d, f_yr)
  base  <- mean(d[[out]]); flag <- s_yr >= quantile(s_yr, 0.90, na.rm = TRUE)
  data.frame(
    outcome    = out,
    n          = nrow(d),
    base_pc    = round(100 * base, 1),
    AUC_svy    = round(auc(s_svy, d[[out]]), 3),
    AUC_plusYr = round(auc(s_yr,  d[[out]]), 3),
    top10_pc   = round(100 * mean(d[[out]][flag], na.rm = TRUE), 1),
    lift       = round(mean(d[[out]][flag], na.rm = TRUE) / base, 2),
    capture_pc = round(100 * sum(d[[out]][flag], na.rm = TRUE) / sum(d[[out]]), 0))
})
res <- do.call(rbind, rows)
print(res, row.names = FALSE)