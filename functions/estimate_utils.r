# ---------------------------------------------------------------------------
# estimate_utils.r  -  shared logistic OR helpers (fixest::feglm).
#
# Odds ratios from coefficient + SE (Wald), never broom profile CIs.
# ---------------------------------------------------------------------------

# Coerce survey / flag columns to integer 0/1 (fixest is happier than bare logical).
# Accepts logical, 0/1 numeric, and common yes/no / true/false strings.
to_01 <- function(x) {
  if (is.null(x)) return(integer(0))
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) {
    out <- as.integer(x != 0)
    out[is.na(x)] <- NA_integer_
    return(out)
  }
  s <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(s))
  out[s %in% c("1", "true", "t", "yes", "y")] <- 1L
  out[s %in% c("0", "false", "f", "no", "n")] <- 0L
  # numeric strings e.g. "1"/"0" already handled; leave other as NA
  out
}

# Pull one coefficient as exp(b) with 95% Wald CI.
pull_or <- function(m, term) {
  na <- tibble::tibble(term = as.character(term), OR = NA_real_, lo = NA_real_,
                       hi = NA_real_, p = NA_real_)
  if (is.null(m)) return(na)
  ct <- tryCatch(as.data.frame(fixest::coeftable(m)), error = function(e) NULL)
  if (is.null(ct) || nrow(ct) == 0) return(na)
  rn <- rownames(ct)
  if (is.null(rn) || !length(rn)) return(na)

  # exact and common fixest suffixes first (no regex)
  cand <- unique(c(
    term,
    paste0(term, "TRUE"),
    paste0(term, "TRUE1"),
    paste0("`", term, "`")
  ))
  row <- cand[cand %in% rn][1]

  # interaction: rv:parental / rv:parentalTRUE / parentalTRUE:rv etc.
  if (is.na(row) && grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    if (length(parts) == 2L) {
      a <- parts[1]; b <- parts[2]
      alts <- c(
        paste0(a, ":", b),
        paste0(a, ":", b, "TRUE"),
        paste0(a, "TRUE:", b),
        paste0(a, "TRUE:", b, "TRUE"),
        paste0(b, ":", a),
        paste0(b, "TRUE:", a),
        paste0(b, ":", a, "TRUE")
      )
      row <- alts[alts %in% rn][1]
      if (is.na(row)) {
        hit <- which(grepl(a, rn, fixed = TRUE) & grepl(b, rn, fixed = TRUE))
        if (length(hit)) row <- rn[hit[1]]
      }
    }
  }

  if (is.na(row) || !nzchar(row)) {
    # last resort: startsWith
    hit <- which(startsWith(rn, term) | rn == paste0(term, "TRUE"))
    if (length(hit)) row <- rn[hit[1]] else return(na)
  }

  e <- ct[row, "Estimate"]
  s <- ct[row, "Std. Error"]
  pcol <- intersect(c("Pr(>|z|)", "Pr(>|t|)"), names(ct))
  p <- if (length(pcol)) ct[row, pcol[1]] else NA_real_
  tibble::tibble(
    term = as.character(term),
    OR = as.numeric(exp(e)),
    lo = as.numeric(exp(e - 1.96 * s)),
    hi = as.numeric(exp(e + 1.96 * s)),
    p  = as.numeric(p)
  )
}

# Fit binomial FE glm; return model or NULL on failure.
fit_feglm <- function(data, outcome, rhs, fe = "course + entry_year") {
  if (is.null(data) || !nrow(data)) {
    message("fit_feglm: empty data for ", outcome, " ~ ", rhs)
    return(NULL)
  }
  # fixest wants a plain data.frame; drop unused factor levels on FE cols
  data <- as.data.frame(data)
  for (v in c("course", "entry_year")) {
    if (v %in% names(data)) data[[v]] <- factor(data[[v]])
  }
  fml <- tryCatch(
    stats::as.formula(paste0(outcome, " ~ ", rhs, " | ", fe)),
    error = function(e) {
      message("fit_feglm: bad formula [", outcome, " ~ ", rhs, " | ", fe, "]: ",
              conditionMessage(e))
      NULL
    }
  )
  if (is.null(fml)) return(NULL)
  tryCatch(
    fixest::feglm(fml, family = "binomial", data = data, warn = FALSE, notes = FALSE),
    error = function(e) {
      message("fit_feglm failed [", outcome, " ~ ", rhs, "]: ", conditionMessage(e))
      NULL
    }
  )
}

fit_feglm_or <- function(data, outcome, term, rhs = term, fe = "course + entry_year") {
  pull_or(fit_feglm(data, outcome, rhs, fe), term)
}

# Mann-Whitney AUC (matches 02_analyse).
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
