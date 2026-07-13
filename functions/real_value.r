# ===========================================================================
# real_value.r  ->  goes in functions/ of the LSF_2026 repo (auto-sourced)
#
# Attaches the "real value of the LSF" measures to the analysis sample, using
# the three reference CSVs built by build_all.R (Part A). Copy those CSVs into
# the repo (e.g. a reference/ or _derived/ folder) and point scripts/12 at them.
#
# Real-value measures from a frozen nominal grant:
#   real_value_cpih              = nominal * infl_factor
#       pure general inflation (CPIH), time only
#   real_value_rent[_ttwa]       = nominal * rent_factor[_ttwa]
#       local housing haircut only (PIPR). Rents move over time so this
#       PARTLY captures time, but if the wider basket inflates while rents
#       are flat, purchasing power is still eroded -- that is MISSING here.
#   real_value_hp[_ttwa]         = nominal * hp_factor[_ttwa]
#       same idea with house prices (UK HPI)
#   real_value_rent[_ttwa]_cpih  = nominal * infl_factor * rent_factor[_ttwa]
#   real_value_hp[_ttwa]_cpih    = nominal * infl_factor * hp_factor[_ttwa]
#       HEADLINE family: general inflation AND local housing. Both channels.
#       (Some overlap: housing is in CPIH; still the right policy object for
#       "frozen nominal + national inflation + local housing costs".)
# ===========================================================================

# ---- COLUMN MAP (set to the analysis sample's columns) ----------------------
RV_PROVIDER <- "college"       # HEI/provider name field in the LSF analysis sample
RV_YEAR     <- "entry_year"    # SAMPLE is one row per student (from trajectories);
                                # "year" here is the cohort's entry year, which is
                                # also the anchor for base_year = 2020 below and the
                                # entry_year fixed effect used in 04_real_value.r.
# Component flags on the analysis sample (from grants_applied in 01_read_tidy):
#   parental   -> + parental_support (£2,000)
#   specialist -> + specialist_subject (£1,000)
# Training grant (£5,000) is universal. Leave NA to disable that top-up.
RV_PARENT     <- "parental"
RV_SPECIALIST <- "specialist"

# Known name variants in the LSF survey that don't string-match the reference
# register's names for the SAME institution (renames / legal-name differences,
# or the register using an older name). Verified against
# sort(unique(ref$provider)) directly, 2026-07-09.
PROVIDER_ALIASES <- c(
  "UNIVERSITY OF TEESSIDE"                   = "Teesside University",
  "UNIVERSITY OF LANCASHIRE"                 = "University of Central Lancashire",
  "UNIVERSITY OF THE WEST OF ENGLAND"        = "University of the West of England, Bristol",
  "NORTHUMBRIA UNIVERSITY"                   = "University of Northumbria at Newcastle",
  "CITY UNIVERSITY LONDON"                   = "The City University",
  "KEELE UNIVERSITY"                         = "University of Keele",
  "UNIVERSITY OF STAFFORDSHIRE"              = "Staffordshire University",
  "UNIVERSITY OF GREATER MANCHESTER"         = "The University of Bolton",
  "BUCKS NEW UNIVERSITY"                     = "Buckinghamshire New University",
  "LEEDS BECKETT UNIVERSITY"                 = "Leeds Metropolitan University",
  "UNIVERSITY OF SUFFOLK"                    = "University Campus Suffolk Ltd",
  "CITY ST GEORGE'S - UNIVERSITY OF LONDON"  = "The City University",
  "ST GEORGE'S - UNIVERSITY OF LONDON"       = "St George's Hospital Medical School",
  "BRUNEL UNIVERSITY LONDON"                 = "Brunel University",
  "UNIVERSITY OF ROEHAMPTON"                 = "Roehampton University",
  "NEWMAN UNIVERSITY - BIRMINGHAM"           = "Newman University College",
  "UNIVERSITY OF NEWCASTLE"                  = "University of Newcastle Upon Tyne",
  "SOLENT UNIVERSITY"                        = "Southampton Solent University",
  "ST MARY'S UNIVERSITY"                     = "St Mary's University College, Twickenham",
  "LEEDS TRINITY UNIVERSITY"                 = "Leeds Trinity University College",
  "QUEEN MARY UNIVERSITY OF LONDON"          = "Queen Mary and Westfield College, University of London",
  "OPEN UNIVERSITY - MILTON KEYNES"          = "Open University(The)",
  "OPEN UNIVERSITY - MIDDLESEX"              = "Open University(The)",
  "OPEN UNIVERSITY - UWE"                    = "Open University(The)",
  "OPEN UNIVERSITY - OXFORD"                 = "Open University(The)",
  "OPEN UNIVERSITY - TORBAY AND SOUTH DEVON" = "Open University(The)",
  "ROYAL HOLLOWAY UNIVERSITY OF LONDON"      = "Royal Holloway College and Bedford New College"
)
# unmatched on 2026-07 work-machine run (add to the c() above when register names confirmed):
# HEALTH SCIENCES UNIVERSITY, BPP UNIVERSITY LTD, NEW COLLEGE DURHAM,
# COLCHESTER INSTITUTE, UNIVERSITY CENTRE SOMERSET, BIRMINGHAM METROPOLITAN COLLEGE
# NB "CITY ST GEORGE'S" (the 2025 merger of City, University of London + St
# George's, University of London) is aliased to the City campus (Islington).
# St George's medical/nursing students are actually based at Tooting, a
# DIFFERENT LAD - this is an approximation. Fine for CPIH-only real value;
# imprecise for the LAD/TTWA rent/house-price measures (1,626 students affected).
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
  s <- sample |>
    dplyr::mutate(.resolved = dplyr::coalesce(
                    unname(PROVIDER_ALIASES[.data[[provider_col]]]), .data[[provider_col]]),
                  .key = rv_norm(.resolved))
  out <- dplyr::left_join(s, dplyr::select(ref_u, .key, lad_code, region), by = ".key")
  miss <- out |> dplyr::filter(is.na(lad_code)) |> dplyr::distinct(.data[[provider_col]])
  if (nrow(miss) > 0)
    message("match_providers: ", nrow(miss), " unmatched providers (add to a crosswalk):\n",
            paste(" -", miss[[1]], collapse = "\n"))
  dplyr::select(out, -.key, -.resolved)
}

# attach nominal grant, cost indices, CPIH and real-value measures
# Nominal package (England LSF schedule, non-means-tested core components):
#   training_grant £5,000 (all) + parental_support £2,000 + specialist_subject £1,000
# as flagged on the sample. Max face value = £8,000 if both top-ups.
build_real_value <- function(sample, ref, awards, cpih, base_year = 2020,
                             provider_col = RV_PROVIDER, year_col = RV_YEAR,
                             parent_col = RV_PARENT,
                             specialist_col = RV_SPECIALIST) {
  training   <- awards$amount[awards$component == "training_grant"][1]
  parental   <- awards$amount[awards$component == "parental_support"][1]
  specialist <- awards$amount[awards$component == "specialist_subject"][1]
  if (is.na(training))   stop("awards missing training_grant")
  if (is.na(parental))   parental <- 0
  if (is.na(specialist)) specialist <- 0

  # the haircut factors (all <=1) come pre-computed in the reference (g1)
  # LAD = campus location; TTWA = functional market students disperse across
  costs <- ref |>
    dplyr::distinct(lad_code, year, infl_factor,
                    rent_factor, hp_factor, rent_factor_ttwa, hp_factor_ttwa)

  flag01 <- function(df, col) {
    if (is.na(col) || !col %in% names(df)) return(rep(0L, nrow(df)))
    as.integer(df[[col]] %in% c(1, TRUE, "Yes", "yes", "TRUE"))
  }

  has_parent_available     <- !is.na(parent_col) && parent_col %in% names(sample)
  has_specialist_available <- !is.na(specialist_col) && specialist_col %in% names(sample)

  m <- match_providers(sample, ref, provider_col)
  m$.yr <- suppressWarnings(as.integer(m[[year_col]]))
  m <- dplyr::left_join(m, costs, by = c("lad_code" = "lad_code", ".yr" = "year"))
  m$has_parent     <- flag01(m, parent_col)
  m$has_specialist <- flag01(m, specialist_col)
  # nominal grant per student
  m$nominal <- training +
    parental   * m$has_parent +
    specialist * m$has_specialist
  # real-value measures (each <= nominal when factors <= 1)
  m$real_value_cpih      <- m$nominal * m$infl_factor          # inflation only
  m$real_value_rent      <- m$nominal * m$rent_factor          # local rent (LAD)
  m$real_value_hp        <- m$nominal * m$hp_factor            # local house price (LAD)
  m$real_value_rent_ttwa <- m$nominal * m$rent_factor_ttwa     # local rent (TTWA)
  m$real_value_hp_ttwa   <- m$nominal * m$hp_factor_ttwa       # local house price (TTWA)
  # CPIH x local housing (headline): inflation AND place
  m$real_value_rent_cpih      <- m$nominal * m$infl_factor * m$rent_factor
  m$real_value_hp_cpih        <- m$nominal * m$infl_factor * m$hp_factor
  m$real_value_rent_ttwa_cpih <- m$nominal * m$infl_factor * m$rent_factor_ttwa
  m$real_value_hp_ttwa_cpih   <- m$nominal * m$infl_factor * m$hp_factor_ttwa
  if (!has_parent_available)
    message("build_real_value: parent flag '", parent_col,
            "' not found; parental top-up set to 0.")
  if (!has_specialist_available)
    message("build_real_value: specialist flag '", specialist_col,
            "' not found; specialist top-up set to 0.")
  dplyr::select(m, -.yr)
}