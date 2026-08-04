# ===========================================================================
# tests/check_numbers.r
#
# Numbers + ASCII harness for the LSF 2026 RAP refactor.
# It reproduces the frozen acceptance numbers from the analysis outputs and
# fails loudly if any drift. The refactor moves code verbatim; this is the
# tripwire that proves the statistics did not change.
#
# HOW TO RUN (work machine, repo root as working directory, after the analysis
# chain has written its outputs):
#
#   MODE=capture  ->  read the CURRENT outputs and freeze every observed value
#                     into tests/expected_values.csv. Do this ONCE at baseline,
#                     before anything moves. It also verifies the hard-coded
#                     frozen numbers and shouts if any is mislocated.
#   MODE=check    ->  (default) recompute the same probes and compare to
#                     tests/expected_values.csv. This is the gate at every
#                     later checkpoint.
#
# Set the mode with an env var:  Sys.setenv(LSF_HARNESS_MODE = "capture")
# then  source("tests/check_numbers.r")   ... then unset and source again to
# run the check. Or just edit MODE below.
#
# Outputs are located by recursive filename search under outputs_dir() and
# derived_dir() (functions/paths.r), newest file wins, so it does not care
# which sub-folder a table lives in. Honours LSF_OUTPUT_DIR.
#
# ASCII rule: no non-ASCII bytes in any .r file except the UTF-8 pound sign
# (C2 A3). Windows source() truncates at the first exotic byte. The lint runs
# every time; STRICT_ASCII (below) decides whether a violation is fatal. It is
# non-fatal at baseline because two function files legitimately carry non-ASCII
# that gets converted to \u escapes when they move; it becomes fatal at the
# post-refactor checkpoint (success criterion 5).
# ===========================================================================

MODE        <- Sys.getenv("LSF_HARNESS_MODE", "check")   # "check" or "capture"
STRICT_ASCII <- identical(Sys.getenv("LSF_HARNESS_STRICT_ASCII"), "1")
EXPECTED_CSV <- file.path("tests", "expected_values.csv")

stopifnot(MODE %in% c("check", "capture"))
source(file.path("functions", "paths.r"))

# ---- locate output tables -------------------------------------------------

find_newest <- function(fname) {
  roots <- unique(c(outputs_dir(), derived_dir()))
  hits <- character(0)
  for (r in roots) {
    if (dir.exists(r))
      hits <- c(hits, list.files(r, pattern = paste0("^", fname, "$"),
                                 recursive = TRUE, full.names = TRUE))
  }
  if (!length(hits))
    stop("harness: could not find ", fname, " under outputs/derived areas.",
         call. = FALSE)
  hits[order(file.info(hits)$mtime, decreasing = TRUE)][1]
}

read_tbl <- function(fname)
  read.csv(find_newest(fname), stringsAsFactors = FALSE, check.names = FALSE)

# pick the single row matching a logical condition and return one column, numeric
pick <- function(df, cond, col) {
  r <- df[cond, , drop = FALSE]
  if (nrow(r) != 1L)
    stop("harness: probe matched ", nrow(r), " rows (wanted 1) for column '",
         col, "'.", call. = FALSE)
  suppressWarnings(as.numeric(r[[col]][1]))
}

# erosion is fully deterministic from the committed CPI reference (no secure
# data), so we recompute it exactly as 05c does rather than read a slide.
erosion <- function() {
  cpi <- read.csv(file.path("reference", "cpi_index.csv"), stringsAsFactors = FALSE)
  base <- cpi$cpi[cpi$year == 2020][1]
  end_year <- max(cpi$year)
  real_end <- 5000 * base / cpi$cpi[cpi$year == end_year][1]
  list(real_end    = round(real_end),
       pct_of_face = round(100 * real_end / 5000),
       loss_pct    = round(100 * (1 - real_end / 5000)))
}

# ---- probe definitions -----------------------------------------------------
# Each probe is id -> function() returning one number. Ids are matched to
# expected_values.csv. Row selection uses grepl so the UTF-8 pound sign in the
# data never has to appear in this (ASCII-only) source file.

PROBES <- list(
  # --- panel size (findings pack B) ---
  # panel_students counts DISTINCT STUDENTS APPEARING IN THE STUDENT-YEAR PANEL,
  # i.e. n_distinct(panel$UniqueID) in 06. It is not the analysis sample size
  # (sample_students below) and not the number of students in the extract. The
  # plan originally froze 290,947 here; that figure is not produced by this
  # table in any pack (13 Jul and 15 Jul both give 290,454 on an identical
  # 606,548 student-years) and was corrected on 2026-07-24.
  panel_students        = function() { d <- read_tbl("B_panel_descriptives.csv"); as.numeric(d$n_students[1]) },
  panel_student_years   = function() { d <- read_tbl("B_panel_descriptives.csv"); as.numeric(d$n_student_years[1]) },
  panel_pct_wave_rv     = function() { d <- read_tbl("B_panel_descriptives.csv"); as.numeric(d$pct_wave_rv[1]) },

  # --- stage 1 output size: the student-level analysis sample every model uses.
  # Pins 01_data.r directly, so a sample-construction change during the refactor
  # fails here rather than showing up as drift in a downstream odds ratio.
  sample_students = function() {
    nrow(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  },

  # --- Arm 1: spec ladder, primary outcome left_before_finish ---
  arm1_s1_or_sd = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S1" & d$outcome_var == "left_before_finish" & d$term == "rv", "OR") },
  arm1_s1_lo    = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S1" & d$outcome_var == "left_before_finish" & d$term == "rv", "lo") },
  arm1_s1_hi    = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S1" & d$outcome_var == "left_before_finish" & d$term == "rv", "hi") },
  arm1_s1_p     = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S1" & d$outcome_var == "left_before_finish" & d$term == "rv", "p") },
  arm1_s3_or_sd = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S3" & d$outcome_var == "left_before_finish" & d$term == "rv", "OR") },
  arm1_s3_lo    = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S3" & d$outcome_var == "left_before_finish" & d$term == "rv", "lo") },
  arm1_s3_hi    = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S3" & d$outcome_var == "left_before_finish" & d$term == "rv", "hi") },
  arm1_s3_p     = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S3" & d$outcome_var == "left_before_finish" & d$term == "rv", "p") },
  arm1_s0k_or   = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S0k" & d$outcome_var == "left_before_finish" & d$term == "rv_k", "OR") },
  arm1_s1k_or   = function() { d <- read_tbl("tbl_rv_spec_ladder.csv"); pick(d, d$spec == "S1k" & d$outcome_var == "left_before_finish" & d$term == "rv_k", "OR") },

  # --- Arm 2: hazard (left_next), tbl_hazard_1k ---
  arm2_hazard_s1_or = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S1", d$spec), "OR_1k_less") },
  arm2_hazard_s1_n  = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S1", d$spec), "n") },
  arm2_hazard_s1_p  = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S1", d$spec), "p") },
  arm2_hazard_s1_lo = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S1", d$spec), "lo") },
  arm2_hazard_s1_hi = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S1", d$spec), "hi") },
  arm2_hazard_s0_or = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S0", d$spec), "OR_1k_less") },
  arm2_hazard_s0_n  = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S0", d$spec), "n") },
  arm2_hazard_s0_p  = function() { d <- read_tbl("tbl_hazard_1k.csv"); pick(d, grepl("^S0", d$spec), "p") },

  # --- Arm 3: recruitment FE, tbl_recruit_fe_results ---
  arm3_prov_pct    = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("provider", d$spec), "pct_change_starters_if_1k_less") },
  arm3_prov_p      = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("provider", d$spec), "p") },
  arm3_prov_ncells = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("provider", d$spec), "n_cells") },
  arm3_reg_pct     = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("region",   d$spec), "pct_change_starters_if_1k_less") },
  arm3_reg_p       = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("region",   d$spec), "p") },
  arm3_reg_ncells  = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("region",   d$spec), "n_cells") },
  arm3_s0_pct      = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("no FE",    d$spec), "pct_change_starters_if_1k_less") },
  arm3_s0_p        = function() { d <- read_tbl("tbl_recruit_fe_results.csv"); pick(d, grepl("no FE",    d$spec), "p") },

  # --- erosion (recomputed from reference/cpi_index.csv) ---
  erosion_real_end    = function() erosion()$real_end,
  erosion_pct_of_face = function() erosion()$pct_of_face,
  erosion_loss_pct    = function() erosion()$loss_pct,

  # --- relative importance (Shapley), region geography ---
  imp_n               = function() { d <- read_tbl("tbl_importance_summary.csv"); pick(d, d$geography == "region", "n") },
  imp_r2_full_region  = function() { d <- read_tbl("tbl_importance_summary.csv"); pick(d, d$geography == "region", "r2_mcfadden_full") },
  imp_auc_full_region = function() { d <- read_tbl("tbl_importance_summary.csv"); pick(d, d$geography == "region", "auc_full") },
  imp_share_top_region = function() { d <- read_tbl("tbl_importance_shapley.csv"); pick(d, d$geography == "region" & d$rank == 1, "share_pct") }
)

# ---- ASCII lint ------------------------------------------------------------

ascii_lint <- function() {
  files <- list.files(c("scripts", "functions", "tests"),
                      pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  bad <- integer(0)
  for (f in files) {
    sz <- file.info(f)$size
    if (is.na(sz) || sz == 0) next
    ints <- as.integer(readBin(f, "raw", sz))
    n <- length(ints)
    offending <- 0L
    i <- 1L
    while (i <= n) {
      b <- ints[i]
      if (b > 127L) {
        if (b == 194L && i < n && ints[i + 1L] == 163L) {  # C2 A3 = pound
          i <- i + 2L
          next
        }
        offending <- offending + 1L
      }
      i <- i + 1L
    }
    if (offending > 0L) bad[[f]] <- offending
  }
  bad
}

# ---- compare helpers -------------------------------------------------------

agree <- function(obs, exp, kind, tol) {
  if (is.na(exp)) return(NA)                       # pending capture
  if (identical(kind, "count"))
    return(as.integer(round(obs)) == as.integer(round(exp)))
  abs(obs - exp) <= 0.5 * 10^(-tol) + 1e-9         # stat: agree at 'tol' dp
}

fmt <- function(x, kind, tol) {
  if (is.na(x)) return("NA")
  if (identical(kind, "count")) return(format(as.integer(round(x)), big.mark = ","))
  formatC(x, format = "f", digits = tol)
}

# ---- run -------------------------------------------------------------------

if (!file.exists(EXPECTED_CSV))
  stop("harness: ", EXPECTED_CSV, " not found. Run from the repo root.", call. = FALSE)

spec <- read.csv(EXPECTED_CSV, stringsAsFactors = FALSE, check.names = FALSE)
spec$expected_num <- suppressWarnings(as.numeric(spec$expected))

missing_probe <- setdiff(spec$id, names(PROBES))
if (length(missing_probe))
  stop("harness: expected_values.csv has ids with no probe: ",
       paste(missing_probe, collapse = ", "), call. = FALSE)

cat("\n=== LSF numbers harness (mode = ", MODE, ") ===\n\n", sep = "")

results <- data.frame(id = spec$id, observed = NA_real_, expected = spec$expected_num,
                      kind = spec$kind, tol = spec$tol, status = NA_character_,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(spec))) {
  id  <- spec$id[i]
  obs <- tryCatch(PROBES[[id]](),
                  error = function(e) { message("  probe '", id, "' errored: ",
                                                 conditionMessage(e)); NA_real_ })
  results$observed[i] <- obs
  ok <- agree(obs, spec$expected_num[i], spec$kind[i], spec$tol[i])
  results$status[i] <-
    if (is.na(obs)) "ERROR"
    else if (is.na(ok)) "PENDING"
    else if (ok) "PASS" else "FAIL"
}

# capture mode: fill blank expected cells with the observed baseline; verify
# (never overwrite) the hard-coded frozen numbers and shout on mismatch.
if (MODE == "capture") {
  filled <- 0L; frozen_bad <- 0L
  for (i in seq_len(nrow(spec))) {
    obs <- results$observed[i]
    if (is.na(spec$expected_num[i])) {
      if (!is.na(obs)) {
        spec$expected[i] <- if (identical(spec$kind[i], "count"))
          as.character(as.integer(round(obs))) else formatC(obs, format = "f", digits = 6)
        filled <- filled + 1L
      }
    } else if (identical(results$status[i], "FAIL")) {
      frozen_bad <- frozen_bad + 1L
      cat(sprintf("  FROZEN MISMATCH  %-22s observed %s  expected %s\n",
                  spec$id[i], fmt(obs, spec$kind[i], spec$tol[i]),
                  fmt(spec$expected_num[i], spec$kind[i], spec$tol[i])))
    }
  }
  spec$expected_num <- NULL
  # quote = TRUE, not FALSE: a label or note containing a comma would otherwise
  # be written unquoted and split into extra columns, corrupting the baseline.
  write.csv(spec, EXPECTED_CSV, row.names = FALSE, quote = TRUE)
  cat(sprintf("\n  captured %d baseline value(s) into %s\n", filled, EXPECTED_CSV))
  if (frozen_bad > 0)
    cat(sprintf("  WARNING: %d hard-coded frozen number(s) did NOT match current outputs.\n",
                frozen_bad))
  else
    cat("  all hard-coded frozen numbers match current outputs.\n")
}

# print the table
w <- max(nchar(results$id))
cat(sprintf("  %-*s  %14s  %14s  %-8s\n", w, "probe", "observed", "expected", "status"))
cat("  ", strrep("-", w + 42), "\n", sep = "")
for (i in seq_len(nrow(results))) {
  cat(sprintf("  %-*s  %14s  %14s  %-8s\n", w, results$id[i],
              fmt(results$observed[i], results$kind[i], results$tol[i]),
              fmt(results$expected[i], results$kind[i], results$tol[i]),
              results$status[i]))
}

n_fail    <- sum(results$status == "FAIL")
n_err     <- sum(results$status == "ERROR")
n_pending <- sum(results$status == "PENDING")

# ASCII lint
bad <- ascii_lint()
cat("\n=== ASCII lint (pound sign excepted) ===\n")
if (!length(bad)) {
  cat("  clean: no non-ASCII bytes outside the pound sign.\n")
} else {
  for (f in names(bad))
    cat(sprintf("  %-45s  %d non-pound non-ASCII byte(s)\n", f, bad[[f]]))
  cat(sprintf("  %d file(s) flagged. STRICT_ASCII = %s.\n",
              length(bad), if (STRICT_ASCII) "TRUE (fatal)" else "FALSE (report only)"))
}
ascii_fatal <- STRICT_ASCII && length(bad) > 0

# ---- verdict ---------------------------------------------------------------

cat("\n=== VERDICT ===\n")
cat(sprintf("  numbers: %d pass, %d fail, %d error, %d pending capture\n",
            sum(results$status == "PASS"), n_fail, n_err, n_pending))

green <- n_fail == 0 && n_err == 0 && n_pending == 0 && !ascii_fatal
if (green) {
  cat("  ALL CHECKS PASSED\n\n")
} else {
  if (n_pending > 0 && MODE == "check")
    cat("  -> PENDING rows: run once with LSF_HARNESS_MODE=capture to freeze the baseline.\n")
  cat("  CHECKS NOT GREEN\n\n")
}

if (!interactive()) quit(status = if (green) 0L else 1L)
invisible(green)
