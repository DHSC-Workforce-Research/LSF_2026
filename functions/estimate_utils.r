# ---------------------------------------------------------------------------
# estimate_utils.r  -  shared logistic OR helpers (fixest::feglm).
#
# Used by 02_analyse and the real-value controlled arm (05). Odds ratios from
# coefficient + SE (Wald), never broom profile CIs (those re-fit large FE
# models and appear to hang on the work machine).
# ---------------------------------------------------------------------------

# Pull one coefficient as exp(b) with 95% Wald CI. Tolerates logical TRUE
# suffixes from fixest (e.g. "crit_courseTRUE") and interaction names.
pull_or <- function(m, term) {
  na <- tibble::tibble(term = term, OR = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_)
  if (is.null(m)) return(na)
  ct <- as.data.frame(fixest::coeftable(m))
  rn <- rownames(ct)
  cand <- c(paste0(term, "TRUE"), term, paste0(term, "TRUE:"), paste0("`", term, "`"))
  row <- cand[cand %in% rn][1]
  if (is.na(row)) {
    hit <- which(rn == term | startsWith(rn, paste0(term, "TRUE")) |
                   rn == paste0(term, "TRUE") | grepl(paste0("(^|:)", term, "(TRUE)?$"), rn))
    if (length(hit)) row <- rn[hit[1]]
  }
  # interaction: try common fixest patterns rv:parentalTRUE, rv:specialistTRUE
  if (is.na(row) && grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    alts <- c(
      term,
      paste0(parts[1], ":", parts[2], "TRUE"),
      paste0(parts[1], "TRUE:", parts[2]),
      paste0(parts[1], "TRUE:", parts[2], "TRUE")
    )
    row <- alts[alts %in% rn][1]
    if (is.na(row)) {
      hit <- which(grepl(parts[1], rn, fixed = TRUE) & grepl(parts[2], rn, fixed = TRUE))
      if (length(hit)) row <- rn[hit[1]]
    }
  }
  if (is.na(row)) return(na)
  e <- ct[row, "Estimate"]
  s <- ct[row, "Std. Error"]
  p <- if ("Pr(>|z|)" %in% names(ct)) ct[row, "Pr(>|z|)"] else
    if ("Pr(>|t|)" %in% names(ct)) ct[row, "Pr(>|t|)"] else NA_real_
  tibble::tibble(
    term = term,
    OR = exp(e),
    lo = exp(e - 1.96 * s),
    hi = exp(e + 1.96 * s),
    p  = p
  )
}

# Fit binomial FE glm; return model or NULL on failure.
fit_feglm <- function(data, outcome, rhs, fe = "course + entry_year") {
  f <- stats::as.formula(paste0(outcome, " ~ ", rhs, " | ", fe))
  tryCatch(
    fixest::feglm(f, family = binomial, data = data, warn = FALSE, notes = FALSE),
    error = function(e) {
      message("fit_feglm failed [", outcome, " ~ ", rhs, "]: ", conditionMessage(e))
      NULL
    }
  )
}

# Convenience: fit + pull one term.
fit_feglm_or <- function(data, outcome, term, rhs = term, fe = "course + entry_year") {
  m <- fit_feglm(data, outcome, rhs, fe)
  pull_or(m, term)
}

# Mann-Whitney AUC for a score vs binary y (matches 02_analyse).
auc_score <- function(score, y) {
  ok <- !is.na(score) & !is.na(y)
  score <- score[ok]
  y <- as.integer(y[ok])
  r <- rank(score)
  n1 <- as.numeric(sum(y == 1L))
  n0 <- as.numeric(sum(y == 0L))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
