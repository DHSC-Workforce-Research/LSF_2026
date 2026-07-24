# ===========================================================================
# real_value.r  ->  goes in functions/ of the LSF_2026 repo (auto-sourced)
#
# Attaches the "real value of the LSF" measures to the analysis sample, using
# the reference CSVs built by scripts/90_build_reference.r.
#
# WHAT CHANGED (2026-07-15, the "weighted cost-of-living index" rebuild):
#   OLD headline: nominal * CPIH_factor * rent_factor  (a PRODUCT of two
#   haircuts). Housing is ALREADY in CPIH, so that double-counts housing and
#   overstates erosion (a ~2% general + ~2% rent move compounds to ~4%/yr).
#
#   NEW headline: real value is a budget-weighted cost-of-living deflator. The
#   student's pound buys a fixed basket where housing is ONE part, counted once
#   at its real budget weight w:
#       cost_index = w * (local_rent / national_rent_2020)      # housing leg
#                  + (1 - w) * (CPI / CPI_2020)                  # non-housing leg (CPI excludes OOH)
#       real_value = nominal / cost_index
#   This carries all THREE channels of variation:
#       1. PLACE   -> local_rent differs across providers (cross-sectional)
#       2. TIME    -> both rent and CPI rise vs the frozen 2020 base
#       3. PACKAGE -> nominal differs by student (parental / specialist top-ups)
#   No cap: a genuinely cheap-2020 area can show real value slightly ABOVE
#   nominal (the grant stretches further there), which is honest.
#
#   COMPATIBILITY: the single-channel legacy columns (real_value_cpih,
#   real_value_rent, real_value_hp, real_value_rent_ttwa, real_value_hp_ttwa)
#   are KEPT UNCHANGED so the measure-sensitivity / robustness scripts still
#   run. The name `real_value_rent_ttwa_cpih` (which the analysis scripts read
#   via their PRIMARY config line) is REPOINTED to the NEW weighted w=HOUSING_
#   WEIGHT TTWA measure, so 04/05/06/07/08 need NO edits. The old product is
#   preserved beside it as real_value_rent_ttwa_cpih_product for comparison.
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

# ---- WEIGHTED COST-OF-LIVING INDEX -----------------------------------------
# real_value = nominal / [ w * rent_rel + (1 - w) * gen_rel ].
# w = the housing share of a student's budget. CPIH weights housing ~1/4, but a
# student living on the grant spends far more of it on rent, so w is higher.
# Take a defensible value from the DfE Student Income and Expenditure Survey
# (SIES); 0.5 is the sensible central default, 0.4 / 0.6 the sensitivity band.
# Change HOUSING_WEIGHT here (no re-download needed) to move the headline.
HOUSING_WEIGHT <- 0.5

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

# attach nominal grant, cost indices, and the real-value measures
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

  # cost building blocks come pre-computed in the reference (g1):
  #   LEGACY factors (all <=1): infl_factor, rent_factor*, hp_factor*
  #   NEW _rel blocks (~1 at cheap-2020, >1 in expensive places / later years):
  #     gen_rel (CPI ex OOH vs 2020), rent_rel* and hp_rel* (local vs national 2020)
  # LAD = campus location; TTWA = functional market students disperse across
  costs <- ref |>
    dplyr::distinct(lad_code, year, infl_factor,
                    rent_factor, hp_factor, rent_factor_ttwa, hp_factor_ttwa,
                    gen_rel, rent_rel, rent_rel_ttwa, hp_rel, hp_rel_ttwa)
  # GUARD: costs must be one row per (lad_code, year) or the join below fans out
  # and silently inflates every count. (Breaks only if a LAD maps to >1 TTWA.)
  stopifnot("costs is not unique by (lad_code, year) - join would fan out" =
              nrow(costs) == nrow(dplyr::distinct(costs, lad_code, year)))

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
  # nominal grant per student (channel 3: package variation)
  m$nominal <- training +
    parental   * m$has_parent +
    specialist * m$has_specialist

  # ---- LEGACY multiplicative measures (KEPT for downstream compatibility) ---
  # single-channel (each <= nominal, factors capped at 1 in g1):
  m$real_value_cpih      <- m$nominal * m$infl_factor          # inflation only (CPIH)
  m$real_value_rent      <- m$nominal * m$rent_factor          # local rent (LAD)
  m$real_value_hp        <- m$nominal * m$hp_factor            # local house price (LAD)
  m$real_value_rent_ttwa <- m$nominal * m$rent_factor_ttwa     # local rent (TTWA)
  m$real_value_hp_ttwa   <- m$nominal * m$hp_factor_ttwa       # local house price (TTWA)
  # OLD combined products (CPIH x local housing) - the double-counting family:
  m$real_value_rent_cpih              <- m$nominal * m$infl_factor * m$rent_factor
  m$real_value_hp_cpih                <- m$nominal * m$infl_factor * m$hp_factor
  m$real_value_rent_ttwa_cpih_product <- m$nominal * m$infl_factor * m$rent_factor_ttwa
  m$real_value_hp_ttwa_cpih           <- m$nominal * m$infl_factor * m$hp_factor_ttwa

  # ---- NEW weighted cost-of-living real value (the headline) ----------------
  # cost_index = w * housing_rel + (1 - w) * gen_rel ; real_value = nominal / cost_index
  ci <- function(rel_house, w) w * rel_house + (1 - w) * m$gen_rel
  w  <- HOUSING_WEIGHT
  m$cost_index_headline <- ci(m$rent_rel_ttwa, w)
  # GUARD: cost index must be strictly positive (a 0/negative _rel leg from a
  # data error would give Inf real value instead of degrading to 0 as the old
  # product did).
  stopifnot("cost_index_headline has non-positive values" =
              all(m$cost_index_headline > 0, na.rm = TRUE))
  # headline name repointed so 04/05/06/07/08 (PRIMARY = "real_value_rent_ttwa_cpih") use this:
  m$real_value_rent_ttwa_cpih <- m$nominal / m$cost_index_headline
  # GUARD: the new headline must have the SAME missing-rows as the old product,
  # or the CPI (D7BT) year coverage differs from CPIH and the sample silently
  # shifts off the known n. Hard-stop if the NA sets diverge.
  stopifnot("new headline NA set differs from legacy product - check CPI vs CPIH year coverage" =
              identical(is.na(m$real_value_rent_ttwa_cpih),
                        is.na(m$real_value_rent_ttwa_cpih_product)))
  # sensitivity band on the housing weight (TTWA geography):
  m$rv_w40_ttwa <- m$nominal / ci(m$rent_rel_ttwa, 0.40)
  m$rv_w50_ttwa <- m$nominal / ci(m$rent_rel_ttwa, 0.50)
  m$rv_w60_ttwa <- m$nominal / ci(m$rent_rel_ttwa, 0.60)
  m$rv_w50_lad  <- m$nominal / ci(m$rent_rel,      0.50)   # LAD geography robustness
  # single-channel NEW measures (uncapped), for the clean headline + robustness:
  m$rv_general_only   <- m$nominal / m$gen_rel            # CPI-only real value (clean erosion headline)
  m$rv_rent_only_ttwa <- m$nominal / m$rent_rel_ttwa      # local-rent-only (housing stress)

  if (!has_parent_available)
    message("build_real_value: parent flag '", parent_col,
            "' not found; parental top-up set to 0.")
  if (!has_specialist_available)
    message("build_real_value: specialist flag '", specialist_col,
            "' not found; specialist top-up set to 0.")
  dplyr::select(m, -.yr)
}