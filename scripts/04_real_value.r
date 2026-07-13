# ===========================================================================
# scripts/04_real_value.R   (thin runner; all logic lives in functions/real_value.r)
#
# Tests whether the REAL VALUE of the LSF (cost-of-living + inflation adjusted)
# predicts leaving, LAD vs TTWA geography, and the parent/carer interaction.
# ASSOCIATIONAL, consistent with the main analysis.
#
# Inputs:
#   * the three CSVs from build_all.R, copied into REF_DIR
#   * a student-year data frame that carries at least:
#       college (HEI), year (wave), course, and the leaving OUTCOME below
#     (the long tidied panel joined to the derived outcome; see note at foot)
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(ggplot2)
})

# ---- CONFIG ----------------------------------------------------------------
REF_DIR  <- "reference"                              # where the 3 CSVs live
SAMPLE   <- readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")) |> as.data.frame()
OUTCOME  <- "left_before_finish"                       # 0/1 leaving outcome
FE       <- c("entry_year", "course")                  # fixed effects
# provider/year columns are set at the top of functions/real_value.r (college/year)
# ---------------------------------------------------------------------------

ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

need <- c(RV_PROVIDER, RV_YEAR, OUTCOME)
miss <- setdiff(need, names(SAMPLE))
if (length(miss))
  stop("SAMPLE is missing: ", paste(miss, collapse = ", "),
       "\nJoin `college` + `year` from the long panel onto the analysis sample by UniqueID.")

cat("Building real-value measures...\n")
t0 <- Sys.time()
samp <- build_real_value(SAMPLE, ref, awards, cpih, base_year = 2020)
cat(sprintf("  done in %.1fs\n", as.numeric(Sys.time() - t0, units = "secs")))
have_parent <- any(samp$has_parent == 1L, na.rm = TRUE)

measures <- c(
  real_value_cpih           = "Inflation-only (CPIH)",
  real_value_rent           = "Rent only (LAD)",
  real_value_hp             = "House price only (LAD)",
  real_value_rent_ttwa      = "Rent only (TTWA)",
  real_value_hp_ttwa        = "House price only (TTWA)",
  real_value_rent_cpih      = "CPIH x rent (LAD)",
  real_value_hp_cpih        = "CPIH x house price (LAD)",
  real_value_rent_ttwa_cpih = "CPIH x rent (TTWA) [headline]",
  real_value_hp_ttwa_cpih   = "CPIH x house price (TTWA)"
)

# extract Wald odds ratios DIRECTLY from the fitted model - deliberately NOT
# using broom::tidy(conf.int=TRUE), which defaults to profile-likelihood CIs
# (MASS::confint.glm re-fits the model many times per coefficient). With
# ~75 factor(course)+factor(entry_year) dummy parameters that is minutes-to-
# much-longer with ZERO console output, which is what caused every earlier
# "it just hangs" run. confint.default() is base stats: computed directly
# from the already-fitted model's standard errors, no re-fitting, no
# profiling, no ambiguity about which method gets dispatched.
extract_or <- function(fit, terms) {
  co <- summary(fit)$coefficients
  ci <- confint.default(fit)
  keep <- intersect(terms, rownames(co))
  data.frame(
    term      = keep,
    estimate  = exp(co[keep, "Estimate"]),
    conf.low  = exp(ci[keep, 1]),
    conf.high = exp(ci[keep, 2]),
    p.value   = co[keep, "Pr(>|z|)"],
    row.names = NULL
  )
}

# fit one logistic model; OR is per 1 SD of the real-value measure -----------
fit_one <- function(m, interact = FALSE) {
  d <- samp |> filter(!is.na(.data[[m]]), !is.na(.data[[OUTCOME]]))
  d$rv <- as.numeric(scale(d[[m]]))
  rhs <- if (interact) "rv * has_parent" else "rv"
  fe  <- paste(sprintf("factor(%s)", FE), collapse = " + ")
  fit <- glm(as.formula(sprintf("%s ~ %s + %s", OUTCOME, rhs, fe)), data = d, family = binomial)
  extract_or(fit, c("rv", "rv:has_parent")) |>
    transmute(measure = measures[[m]], spec = if (interact) "with parent interaction" else "main",
              term = recode(term, rv = "real value (per SD)", `rv:has_parent` = "x parent/carer"),
              OR = estimate, lo = conf.low, hi = conf.high, p = p.value)
}

specs <- expand_grid(m = names(measures), interact = c(FALSE, have_parent)) |> distinct()
n_specs <- nrow(specs)

cat(sprintf("Fitting %d model(s)...\n", n_specs))
res_list <- vector("list", n_specs)
t_start <- Sys.time()
for (i in seq_len(n_specs)) {
  ti <- Sys.time()
  res_list[[i]] <- fit_one(specs$m[i], specs$interact[i])
  elapsed_i   <- as.numeric(Sys.time() - ti,      units = "secs")
  elapsed_tot <- as.numeric(Sys.time() - t_start, units = "secs")
  avg <- elapsed_tot / i
  eta <- avg * (n_specs - i)
  cat(sprintf("  [%d/%d] %-28s (%s) done in %.1fs  |  elapsed %.1fs  |  ETA %.1fs\n",
              i, n_specs, measures[[specs$m[i]]],
              if (specs$interact[i]) "with interaction" else "main",
              elapsed_i, elapsed_tot, eta))
}
res <- dplyr::bind_rows(res_list)

cat("\n=== Real value -> leaving (odds ratios, 95% CI) ===\n")
print(as.data.frame(res |> mutate(across(c(OR, lo, hi), ~round(.x, 3)))), row.names = FALSE)
write_csv(res, file.path(outputs_dir(), "real_value_odds.csv"))

# ---- DHSC forest plot (widescreen slide) -----------------------------------
p <- ggplot(filter(res, term == "real value (per SD)"),
            aes(x = OR, y = reorder(measure, OR))) +
  geom_vline(xintercept = 1, colour = "grey40", linewidth = 0.3) +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = "#00A499") +   # DHSC teal
  labs(title = "Does the real value of the LSF predict leaving?",
       subtitle = "Odds ratio of leaving per 1 SD of real grant value. Associational; entry-year and course fixed effects.",
       x = "Odds ratio of leaving (per SD)", y = NULL) +
  theme_dhsc()
save_dhsc(p, file.path(outputs_dir(), "real_value_slide.png"), width = 13.33, height = 7.5)

saveRDS(samp, file.path(derived_dir(), "lsf_real_value_sample.rds"))

cat("\nWritten: real_value_odds.csv, real_value_slide.png ->", outputs_dir(), "\n")
if (!have_parent) cat("NOTE: parent flag not set; interaction skipped (see functions/real_value.r RV_PARENT).\n")#
message("Real value coverage: ", round(100*mean(!is.na(samp$real_value_rent_ttwa)),1), "%")
