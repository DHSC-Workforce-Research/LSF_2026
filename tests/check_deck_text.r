# ===========================================================================
# tests/check_deck_text.r
#
# Guards the one-file rule for slide text. Run from the repo root:
#   source("tests/check_deck_text.r")
#
# Fails loudly if
#   1. a manifest slide has no entry in deck_text(), or an empty title
#   2. a builder named by the manifest does not exist
#   3. any rendered string still carries an unsubstituted {token}
#   4. the technical palette drifts from dhsc_theme.r
#   5. a string literal has crept back into a builder's labs() call
# ===========================================================================

source("scripts/00_config.r")

fails <- character(0)
note  <- function(...) fails <<- c(fails, paste0(...))

man <- deck_manifest()
txt <- deck_text()

# 1 + 2 ----------------------------------------------------------------------
for (i in seq_len(nrow(man))) {
  slug <- man$slug[i]
  if (!slug %in% names(txt)) { note("no deck_text entry for slide ", man$number[i], " ", slug); next }
  if (!nzchar(lbl_raw(slug, "title"))) note("empty title for ", slug)
  if (!exists(man$builder[i], mode = "function")) note("no builder ", man$builder[i], "() for ", slug)
}

# 3 --------------------------------------------------------------------------
# Fields whose braces are filled by the builder at draw time are exempt here;
# everything else must resolve with no arguments.
runtime_tokens <- c(
  rv_erosion = "loss_pct|core|real_end|end_year",
  rv_place = "core|year",
  rv_package = "core|parental|specialist|package_max|n_total",
  auc = "auc|aucpct",
  rv_leave_curve = "pp|spec|w_rent|w_cpi",
  rv_hazard_leave_next = "measure|pp",
  rv_three_arms = "measure",
  rv_recruit_effect = "cells",
  rv_spec_ladder = "measure",
  placement_family_fe = "not_estimated",
  confidence_confident = "ref",
  confidence_unconfident = "ref",
  triangle_scatter_risk_dependence = "rho|n_groups",
  triangle_rates = "ref",
  tech_realvalue = "w"
)
for (slug in names(txt)) {
  allowed <- if (slug %in% names(runtime_tokens)) runtime_tokens[[slug]] else "$^"
  for (f in setdiff(names(txt[[slug]]), "wrap")) {
    v <- lbl_raw(slug, f)
    hits <- regmatches(v, gregexpr("\\{[a-z_]+\\}", v))[[1]]
    hits <- hits[!grepl(paste0("^\\{(", allowed, ")\\}$"), hits)]
    if (length(hits)) note("unresolved token(s) in ", slug, "$", f, ": ", paste(hits, collapse = " "))
  }
}

# 4 --------------------------------------------------------------------------
pal_expect <- c(TECH_BLUE = "af_blue", TECH_TEAL = "af_teal",
                TECH_ORANGE = "af_orange", TECH_RED = "risk")
for (k in names(pal_expect))
  if (!identical(toupper(get(k)), toupper(unname(dhsc_cols[[pal_expect[[k]]]]))))
    note(k, " has drifted from dhsc_cols[['", pal_expect[[k]], "']]")

# 5 --------------------------------------------------------------------------
for (f in c("functions/deck_builders.r", "functions/deck_builders_annex.r",
            "functions/deck_builders_technical.r")) {
  ln <- readLines(f, warn = FALSE)
  bad <- grep('^\\s*(title|subtitle)\\s*=\\s*"', ln)
  if (length(bad)) note(f, ": literal title/subtitle at line(s) ", paste(bad, collapse = ", "))
}

cat("\n=== deck text check ===\n")
if (length(fails)) {
  cat(sprintf("FAILED, %d problem(s):\n", length(fails)))
  cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
  stop("deck text check failed", call. = FALSE)
}
cat(sprintf("passed: %d manifest slides, %d text entries, no unresolved tokens.\n",
            nrow(man), length(txt)))
