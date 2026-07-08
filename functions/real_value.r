# ===========================================================================
# real_value.r  ->  goes in functions/ of the LSF_2026 repo (auto-sourced)
#
# Attaches the "real value of the LSF" measures to the analysis sample, using
# the three reference CSVs built by build_all.R (Part A). Copy those CSVs into
# the repo (e.g. a reference/ or _derived/ folder) and point scripts/12 at them.
#
# THREE measures, all from a frozen nominal grant:
#   real_terms       = grant deflated by CPIH only            (inflation, time)
#   real_value_rent  = grant / local rent cost-index          (space + time via PIPR)
#   real_value_hp    = grant / local house-price cost-index   (space + time via UK HPI)
# ===========================================================================

# ---- COLUMN MAP (set to the long tidied df's columns) -----------------------
RV_PROVIDER <- "college"       # HEI/provider name field in the LSF long df
RV_YEAR     <- "year"          # survey wave year on each student-year row
# No direct parent/carer field in the survey. Proxy = received Parental Support,
# which requires a dependent child <15 to be eligible. Derive it from
# `grants_applied` once its coding is known, e.g.:
#   samp$has_parent <- as.integer(stringr::str_detect(samp$grants_applied, regex("parent", TRUE)))
# then set RV_PARENT <- "has_parent". Leave NA to disable the interaction.
RV_PARENT   <- NA_character_
# ---------------------------------------------------------------------------

# normalise a provider name for matching. Deletes apostrophes (so King's = Kings),
# turns other punctuation into spaces, drops "the" and normalises "&". Keeps
# "university"/"college" (removing them collides distinct HEIs, e.g. UCL).
rv_norm <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("['’`]", "", x)          # delete straight + curly apostrophes
  x <- gsub("&", " and ", x)
  x <- gsub("\\bthe\\b", " ", x)          # drop "the"
  x <- gsub("[^a-z0-9]+", " ", x)          # other punctuation -> space
  stringr::str_squish(x)
}

# match sample provider names to the cost reference; report unmatched
match_providers <- function(sample, ref, provider_col = RV_PROVIDER) {
  ref_u <- ref |>
    dplyr::distinct(provider, lad_code, region) |>
    dplyr::mutate(.key = rv_norm(provider)) |>
    dplyr::distinct(.key, .keep_all = TRUE)
  s <- sample |> dplyr::mutate(.key = rv_norm(.data[[provider_col]]))
  out <- dplyr::left_join(s, dplyr::select(ref_u, .key, lad_code, region), by = ".key")
  miss <- out |> dplyr::filter(is.na(lad_code)) |> dplyr::distinct(.data[[provider_col]])
  if (nrow(miss) > 0)
    message("match_providers: ", nrow(miss), " unmatched providers (add to a crosswalk):\n",
            paste(" -", miss[[1]], collapse = "\n"))
  dplyr::select(out, -.key)
}

# attach nominal grant, cost indices, CPIH and the three real-value measures
build_real_value <- function(sample, ref, awards, cpih, base_year = 2020,
                             provider_col = RV_PROVIDER, year_col = RV_YEAR,
                             parent_col = RV_PARENT) {
  training <- awards$amount[awards$component == "training_grant"][1]
  parental <- awards$amount[awards$component == "parental_support"][1]

  # the haircut factors (all <=1) come pre-computed in the reference (build_all.R)
  # LAD = campus location; TTWA = functional market students disperse across
  costs <- ref |>
    dplyr::distinct(lad_code, year, infl_factor,
                    rent_factor, hp_factor, rent_factor_ttwa, hp_factor_ttwa)

  has_parent_available <- !is.na(parent_col) && parent_col %in% names(sample)

  m <- match_providers(sample, ref, provider_col)
  m$.yr <- suppressWarnings(as.integer(m[[year_col]]))
  m <- dplyr::left_join(m, costs, by = c("lad_code" = "lad_code", ".yr" = "year"))
  m$has_parent <- if (has_parent_available)
    as.integer(m[[parent_col]] %in% c(1, TRUE, "Yes", "yes")) else 0L
  # nominal grant per student (training grant universal; add parental if flagged)
  m$nominal          <- training + parental * m$has_parent
  # real-value measures, haircut style (each <= nominal), capped at face value
  m$real_value_cpih      <- m$nominal * m$infl_factor          # inflation only
  m$real_value_rent      <- m$nominal * m$rent_factor          # local rent (LAD)
  m$real_value_hp        <- m$nominal * m$hp_factor            # local house price (LAD)
  m$real_value_rent_ttwa <- m$nominal * m$rent_factor_ttwa     # local rent (TTWA)
  m$real_value_hp_ttwa   <- m$nominal * m$hp_factor_ttwa       # local house price (TTWA)
  if (!has_parent_available)
    message("build_real_value: parent flag '", parent_col,
            "' not found; parental top-up set to 0 and interaction disabled.")
  dplyr::select(m, -.yr)
}
