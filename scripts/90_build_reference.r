# ===========================================================================
# LSF real-value geography reference  -  Part A (single self-contained build)
#
# For every English HEI, the local RENT (ONS PIPR) and HOUSE PRICE (Land
# Registry UK HPI) it faces by year 2020-2026, plus CPIH, CPI (general prices
# excluding owner-occupier housing) and the LSF award schedule. Outputs four
# CSVs to copy to the work machine:
#   provider_costofliving.csv   cpih_index.csv   cpi_index.csv   lsf_awards.csv
#
# WHAT CHANGED (2026-07-15, the "weighted cost-of-living index" rebuild):
#   The OLD headline real value was nominal * CPIH_factor * rent_factor, a
#   PRODUCT of two haircuts. Because housing is ALREADY inside CPIH (the H is
#   owner-occupier housing), that product counts housing inflation twice and
#   overstates erosion (a ~2% general + ~2% rent move compounds to ~4%/yr).
#   The NEW construct treats real value as a proper budget-weighted cost-of-
#   living deflator: the student's pound buys a fixed basket where housing is
#   ONE part, counted once at its real budget weight w:
#       cost_index = w * (local_rent / national_rent_2020)
#                  + (1 - w) * (CPI / CPI_2020)      # CPI excludes OOH housing
#       real_value = nominal / cost_index
#   This script writes the BUILDING BLOCKS (gen_rel, rent_rel*, hp_rel*) into
#   provider_costofliving.csv; the weight w and the final real_value are set in
#   functions/real_value.r so w can be changed without re-downloading. The OLD
#   multiplicative columns are KEPT (suffixed _gbp) for comparison/robustness.
#
# All R. readxl reads the ONS .xlsx; httr+jsonlite hit postcodes.io.
# Run top to bottom. Inputs auto-download to ./data on first run (cached).
#
# WORK-MACHINE NOTES:
#  * Only the CONFIG block changes.
#  * PIPR_URL and HPI_URL are date-versioned by ONS/Land Registry. Update the
#    date in each to the latest monthly release before a refresh run.
# ===========================================================================

suppressMessages({
  library(readxl); library(readr); library(dplyr); library(tidyr)
  library(stringr); library(purrr); library(httr); library(jsonlite)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# ---- CONFIG ----------------------------------------------------------------
DIR   <- "reference"   # <- change on work machine
YEARS <- 2020:2026
ENG   <- "E92000001"                                            # England national row

HEI_URL  <- "https://learning-provider.data.ac.uk/data/learning-providers-plus.csv"
PIPR_URL <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/priceindexofprivaterentsukmonthlypricestatistics/17june2026/priceindexofprivaterentsukmonthlypricestatistics13.xlsx"
HPI_URL  <- "https://publicdata.landregistry.gov.uk/market-trend-data/house-price-index-data/Average-prices-2026-04.csv"
CPIH_URL <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
# CPI ALL ITEMS index (D7BT, 2015=100). CPI EXCLUDES owner-occupier housing
# costs by construction, so it is the "general prices without the big housing
# component" leg of the weighted index (housing is added back via local rent).
CPI_URL  <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bt/mm23"
# ---------------------------------------------------------------------------

data_dir <- file.path(DIR, "data"); dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
grab <- function(url, name, binary = FALSE) {
  f <- file.path(data_dir, name)
  if (!file.exists(f)) {
    message("downloading ", name);
    download.file(url, f, mode = if (binary) "wb" else "w", quiet = TRUE)
  }
  f
}
HEI_CSV  <- grab(HEI_URL,  "hei_providers.csv")
PIPR_XL  <- grab(PIPR_URL, "pipr_rent.xlsx", binary = TRUE)
HPI_CSV  <- grab(HPI_URL,  "ukhpi_avg.csv")
CPIH_CSV <- grab(CPIH_URL, "cpih_l522.csv")
CPI_CSV  <- grab(CPI_URL,  "cpi_d7bt.csv")

# ---- 1. HEI list -----------------------------------------------------------
hei <- read_csv(HEI_CSV, show_col_types = FALSE) |>
  transmute(ukprn = UKPRN, provider = VIEW_NAME, postcode = POSTCODE,
            lon = as.numeric(LONGITUDE), lat = as.numeric(LATITUDE)) |>
  filter(!is.na(postcode), postcode != "")

# ---- 1b. Postcode corrections (documented overrides for known-bad register entries) ----
# hei_providers.csv (JISC Learning Providers Plus) has stale/incorrect postcodes for
# a handful of providers, which silently drops them at the geocoding step below with
# no error. Verified against postcodes.io + each institution's public campus address,
# 2026-07-09:
#   - Birmingham City University (UKPRN 10007140, register postcode B42 2SU):
#     postcode TERMINATED 2018-11 (postcodes.io confirms). Reverse-geocoding the
#     register's own stored lat/lon (-1.897282, 52.517286) resolves to B42 2GX,
#     same site, Birmingham LAD (E08000025).
#   - University of Northampton (UKPRN 10007138, register postcode NN2 5PH):
#     postcode does not exist (404) - looks like a transposed-digit typo in the
#     source register. NN1 5PH is Waterside Campus, the university's documented
#     current address, and resolves correctly.
postcode_fixes <- tibble::tribble(
  ~ukprn,      ~postcode_fixed,
  10007140,    "B42 2GX",   # Birmingham City University
  10007138,    "NN1 5PH"    # University of Northampton
) |> mutate(ukprn = as.numeric(ukprn))   # force type match against hei$ukprn (belt and braces)
hei <- hei |>
  mutate(ukprn = as.numeric(ukprn)) |>
  left_join(postcode_fixes, by = "ukprn") |>
  mutate(postcode = coalesce(postcode_fixed, postcode)) |>
  select(-postcode_fixed)

# ---- 2. Geocode postcodes -> LAD/MSOA/LSOA/region (cached) ------------------
GEO_CACHE <- file.path(data_dir, "hei_geocoded.csv")
if (file.exists(GEO_CACHE)) {
  geo <- read_csv(GEO_CACHE, show_col_types = FALSE)
} else {
  bulk_geocode <- function(pcs) {
    res <- POST("https://api.postcodes.io/postcodes",
                body = list(postcodes = pcs), encode = "json", timeout(60))
    stop_for_status(res)
    map_dfr(content(res, "parsed", simplifyVector = FALSE)$result, function(x) {
      r <- x$result
      if (is.null(r)) return(tibble(postcode = x$query, lad_code = NA, lad_name = NA,
                                    msoa_code = NA, lsoa_code = NA, region = NA, country = NA))
      tibble(postcode = x$query, lad_code = r$codes$admin_district %||% NA,
             lad_name = r$admin_district %||% NA, msoa_code = r$codes$msoa %||% NA,
             lsoa_code = r$codes$lsoa %||% NA, region = r$region %||% NA,
             country = r$country %||% NA)
    })
  }
  geo <- map_dfr(split(hei$postcode, ceiling(seq_along(hei$postcode) / 100)), bulk_geocode)
  write_csv(geo, GEO_CACHE)
}
hei <- hei |> left_join(distinct(geo, postcode, .keep_all = TRUE), by = "postcode")
message("HEIs: ", nrow(hei), " | geocoded: ", sum(!is.na(hei$lad_code)),
        " | England: ", sum(hei$country == "England", na.rm = TRUE))

# loud reporting for any HEI that failed to geocode (would otherwise silently
# drop out of `ref` later with no trace - same pattern as match_providers()'s
# unmatched-provider reporting in functions/real_value.r)
failed_geo <- hei |> filter(is.na(lad_code)) |> distinct(provider, postcode)
if (nrow(failed_geo) > 0)
  message("WARNING: ", nrow(failed_geo), " HEI(s) failed to geocode (will be dropped):\n",
          paste(" -", failed_geo$provider, "(", failed_geo$postcode, ")", collapse = "\n"))

# ---- helper: collapse a code x month price series to code x year -----------
by_year <- function(df, code, date, val, newname) {
  df |>
    transmute(code = .data[[code]],
              year = as.integer(format(as.Date(.data[[date]]), "%Y")),
              v = suppressWarnings(as.numeric(.data[[val]]))) |>
    filter(year %in% YEARS) |>
    group_by(code, year) |>
    summarise("{newname}" := mean(v, na.rm = TRUE), .groups = "drop")
}

# ---- 3. RENT (PIPR): LAD, region (fallback) and England ---------------------
pipr <- read_excel(PIPR_XL, sheet = "Table 1", skip = 2)
names(pipr) <- str_squish(names(pipr))
pr <- pipr |> transmute(area = `Area code`, aname = `Area name`, dt = `Time period`,
                        rent_all = `Rental price`, rent_2bed = `Rental price two bed`)
rent_lad <- by_year(pr, "area", "dt", "rent_all", "rent_all") |>
  left_join(by_year(pr, "area", "dt", "rent_2bed", "rent_2bed"), by = c("code", "year")) |>
  rename(lad_code = code)
rent_reg <- by_year(filter(pr, str_starts(area, "E12")) |> mutate(area = aname),
                    "area", "dt", "rent_all", "reg_rent_all") |> rename(region = code)
nat_rent <- rent_lad |> filter(lad_code == ENG) |> transmute(year, nat_rent_all = rent_all)

# ---- 4. HOUSE PRICE (UK HPI): LAD, region (fallback) and England ------------
hpi <- read_csv(HPI_CSV, show_col_types = FALSE)
hp_lad <- by_year(hpi, "Area_Code", "Date", "Average_Price", "house_price") |> rename(lad_code = code)
hp_reg <- by_year(mutate(filter(hpi, str_starts(Area_Code, "E12")), rn = Region_Name),
                  "rn", "Date", "Average_Price", "reg_hp") |> rename(region = code)
nat_hp <- hp_lad |> filter(lad_code == ENG) |> transmute(year, nat_house_price = house_price)

# ---- 4b. TTWA layer -------------------------------------------------------
# LAD<->TTWA composition from ONS OG (OA-2021 -> TTWA-2011 -> LAD-2022), with
# OA counts as population weights (OAs are ~equal population by design).
# One server-side group-by request; cached. TTWA cost = OA-weighted mean of its
# member LADs' rent / house price. Each HEI's LAD -> its dominant (largest) TTWA.
TTWA_CACHE <- file.path(data_dir, "ttwa_composition.csv")
if (file.exists(TTWA_CACHE)) {
  comp <- read_csv(TTWA_CACHE, show_col_types = FALSE)
} else {
  ag_svc <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
    "Output_Area_2021_to_TTWAs_2011_to_LAD_2022_Lookup_for_England_and_Wales_2022",
    "/FeatureServer/0/query")
  res <- GET(ag_svc, query = list(
    where = "LAD22CD LIKE 'E%'",
    groupByFieldsForStatistics = "LAD22CD,LAD22NM,TTWA11CD,TTWA11NM",
    outStatistics = '[{"statisticType":"count","onStatisticField":"OA21CD","outStatisticFieldName":"n_oa"}]',
    f = "json"), timeout(60))
  stop_for_status(res)
  comp <- map_dfr(content(res, "parsed", simplifyVector = FALSE)$features,
                  ~ as_tibble(.x$attributes))
  write_csv(comp, TTWA_CACHE)
}
comp <- comp |> transmute(lad_code = LAD22CD, ttwa_code = TTWA11CD, ttwa_name = TTWA11NM, n_oa)

# TTWA cost per year = OA-weighted mean of member-LAD rent / house price
lad_costs <- rent_lad |> select(lad_code, year, rent_all) |>
  full_join(select(hp_lad, lad_code, year, house_price), by = c("lad_code", "year"))
# NB many-to-many is INTENTIONAL here: `comp` has multiple rows per lad_code when a
# LAD's constituent Output Areas span more than one TTWA, and `lad_costs` has one row
# per lad_code PER YEAR. Joining on lad_code alone deliberately cross-multiplies every
# TTWA a LAD touches by every year - required so the group_by(ttwa_code, year) below
# can compute each TTWA's population-weighted average cost per year across all its
# member LADs. `relationship = "many-to-many"` only silences the warning once we've
# PROVEN it's this benign LAD x TTWA x year expansion and not accidental duplicate
# rows on either side (which would double-weight a LAD and silently corrupt the
# weighted mean without showing up as a coverage/missingness problem):
stopifnot("comp has duplicate (lad_code, ttwa_code) rows" =
            !anyDuplicated(comp[c("lad_code", "ttwa_code")]))
stopifnot("lad_costs has duplicate (lad_code, year) rows" =
            !anyDuplicated(lad_costs[c("lad_code", "year")]))
ttwa_cost <- comp |>
  inner_join(lad_costs, by = "lad_code", relationship = "many-to-many") |>
  group_by(ttwa_code, year) |>
  summarise(ttwa_rent  = weighted.mean(rent_all,    n_oa, na.rm = TRUE),
            ttwa_hp    = weighted.mean(house_price, n_oa, na.rm = TRUE), .groups = "drop")
hei_ttwa <- comp |> group_by(lad_code) |>         # each LAD -> its dominant TTWA
  slice_max(n_oa, n = 1, with_ties = FALSE) |>
  select(lad_code, ttwa_code, ttwa_name)

# ---- 5. CPIH (annual mean of monthly, 2015=100) ----------------------------
cpih <- read_csv(CPIH_CSV, col_names = c("period", "value"), show_col_types = FALSE) |>
  filter(str_detect(period, "^[0-9]{4} [A-Z]{3}$")) |>
  mutate(year = as.integer(str_sub(period, 1, 4)), v = as.numeric(value)) |>
  filter(year %in% YEARS) |>
  group_by(year) |> summarise(cpih = mean(v, na.rm = TRUE), .groups = "drop")
write_csv(cpih, file.path(DIR, "cpih_index.csv"))

# ---- 5b. CPI ALL ITEMS (annual mean of monthly, 2015=100) ------------------
# General prices EXCLUDING owner-occupier housing. This is the non-housing leg
# of the weighted cost-of-living index (housing is added once, via local rent).
# Same monthly-to-annual collapse as CPIH.
cpi <- read_csv(CPI_CSV, col_names = c("period", "value"), show_col_types = FALSE) |>
  filter(str_detect(period, "^[0-9]{4} [A-Z]{3}$")) |>
  mutate(year = as.integer(str_sub(period, 1, 4)), v = as.numeric(value)) |>
  filter(year %in% YEARS) |>
  group_by(year) |> summarise(cpi = mean(v, na.rm = TRUE), .groups = "drop")
write_csv(cpi, file.path(DIR, "cpi_index.csv"))

# ---- 5c. Fixed 2020 national anchors (base for the weighted cost index) -----
# The weighted index compares each place-year cost to the NATIONAL 2020 level
# (a fixed base), so the index carries BOTH the cross-place level (channel 1:
# some places cost more) AND the over-time rise (channel 2: everything got
# dearer). Nominal package variation (channel 3) enters later in real_value.r.
BASE_YEAR     <- 2020L
nat_rent_base <- nat_rent$nat_rent_all[nat_rent$year == BASE_YEAR][1]
nat_hp_base   <- nat_hp$nat_house_price[nat_hp$year == BASE_YEAR][1]
cpi_base      <- cpi$cpi[cpi$year == BASE_YEAR][1]
if (any(is.na(c(nat_rent_base, nat_hp_base, cpi_base))))
  stop("Missing a 2020 national anchor (rent / house price / CPI). Check inputs.")

# ---- 6. LSF award schedule (England £/yr, non-means-tested, frozen 2020-26) --
# Verified vs gov.uk LSF guidance 7th-9th editions (2023-26). TDAE (expenditure)
# and ESF (hardship, up to £3,000) are variable and excluded from the schedule.
lsf_awards <- tidyr::crossing(year = YEARS,
  tibble(component = c("training_grant", "parental_support", "specialist_subject"),
         amount    = c(5000,             2000,               1000))) |>
  arrange(year, component)
write_csv(lsf_awards, file.path(DIR, "lsf_awards.csv"))

# ---- 7. Assemble provider reference (long by year, with region fallback) ----
ref <- hei |>
  filter(country == "England", !is.na(lad_code)) |>
  left_join(hei_ttwa, by = "lad_code") |>
  select(ukprn, provider, postcode, lad_code, lad_name, msoa_code, lsoa_code, region,
         ttwa_code, ttwa_name) |>
  tidyr::crossing(year = YEARS) |>
  left_join(rent_lad,  by = c("lad_code", "year")) |>
  left_join(rent_reg,  by = c("region", "year")) |>
  left_join(hp_lad,    by = c("lad_code", "year")) |>
  left_join(hp_reg,    by = c("region", "year")) |>
  left_join(ttwa_cost, by = c("ttwa_code", "year")) |>
  left_join(nat_rent,  by = "year") |>
  left_join(nat_hp,    by = "year") |>
  left_join(cpih,      by = "year") |>
  left_join(cpi,       by = "year") |>
  mutate(rent_all    = coalesce(rent_all, reg_rent_all),   # region fallback (LAD)
         house_price = coalesce(house_price, reg_hp),
         ttwa_rent   = coalesce(ttwa_rent, rent_all),      # LAD fallback (TTWA)
         ttwa_hp     = coalesce(ttwa_hp, house_price),
         cost_index_rent = rent_all    / nat_rent_all,      # LEGACY: vs SAME-year national
         cost_index_hp   = house_price / nat_house_price,   # LEGACY: vs SAME-year national
         # ---- building blocks for the NEW weighted cost-of-living index -----
         # all relative to the FIXED 2020 national anchor (so they carry both
         # the cross-place level and the over-time rise). Each ~1 in cheap-2020
         # places, >1 in expensive places / later years. real_value.r combines
         # rent_rel* (housing leg) with gen_rel (non-housing leg) at weight w.
         gen_rel       = cpi        / cpi_base,             # general prices (CPI ex OOH) vs 2020
         rent_rel      = rent_all   / nat_rent_base,        # local rent (LAD)  vs national 2020
         rent_rel_ttwa = ttwa_rent  / nat_rent_base,        # local rent (TTWA) vs national 2020
         hp_rel        = house_price / nat_hp_base,          # local house price (LAD)  vs national 2020
         hp_rel_ttwa   = ttwa_hp    / nat_hp_base) |>        # local house price (TTWA) vs national 2020
  select(-reg_rent_all, -reg_hp)

# ---- 7b. Centralised REAL VALUE (haircut: cheapest area in base year = full £) ---
# Grant is worth its face value only where housing is cheapest; more expensive
# areas and later years are a discount off that. Every factor <= 1, so no
# measure ever exceeds face value. Anchor = 10th pct of HEI-area values in BASE.
BASE       <- 2020
CORE_GRANT <- 5000                       # training grant (universal, frozen); illustrative core £
cpih_base  <- cpih$cpih[cpih$year == BASE][1]
anc <- ref |> filter(year == BASE) |>
  summarise(rent = quantile(rent_all, 0.10, na.rm = TRUE),
            hp   = quantile(house_price, 0.10, na.rm = TRUE),
            rent_ttwa = quantile(ttwa_rent, 0.10, na.rm = TRUE),
            hp_ttwa   = quantile(ttwa_hp,   0.10, na.rm = TRUE))
ref <- ref |> mutate(
  infl_factor = cpih_base / cpih,                          # general inflation over time (CPIH)
  # LAD geography (campus location)
  rent_factor = pmin(1, anc$rent / rent_all),
  hp_factor   = pmin(1, anc$hp   / house_price),
  # TTWA geography (functional market students disperse across)
  rent_factor_ttwa = pmin(1, anc$rent_ttwa / ttwa_rent),
  hp_factor_ttwa   = pmin(1, anc$hp_ttwa   / ttwa_hp),
  # combined: CPIH x local housing (both channels; product already <= 1)
  rent_cpih_factor      = infl_factor * rent_factor,
  hp_cpih_factor        = infl_factor * hp_factor,
  rent_ttwa_cpih_factor = infl_factor * rent_factor_ttwa,
  hp_ttwa_cpih_factor   = infl_factor * hp_factor_ttwa,
  real_value_cpih_gbp           = round(CORE_GRANT * infl_factor),
  real_value_rent_gbp           = round(CORE_GRANT * rent_factor),
  real_value_hp_gbp             = round(CORE_GRANT * hp_factor),
  real_value_rent_ttwa_gbp      = round(CORE_GRANT * rent_factor_ttwa),
  real_value_hp_ttwa_gbp        = round(CORE_GRANT * hp_factor_ttwa),
  real_value_rent_cpih_gbp      = round(CORE_GRANT * rent_cpih_factor),
  real_value_hp_cpih_gbp        = round(CORE_GRANT * hp_cpih_factor),
  real_value_rent_ttwa_cpih_gbp = round(CORE_GRANT * rent_ttwa_cpih_factor),
  real_value_hp_ttwa_cpih_gbp   = round(CORE_GRANT * hp_ttwa_cpih_factor))
write_csv(ref, file.path(DIR, "provider_costofliving.csv"))

# ---- 8. VALIDATION ---------------------------------------------------------
message("\n=== VALIDATION ===")
message("providers (England): ", n_distinct(ref$provider), " | rows: ", nrow(ref),
        " | missing rent: ", sum(is.na(ref$rent_all)),
        " | missing house price: ", sum(is.na(ref$house_price)))
cat("\nCPIH (2015=100):  frozen grant real value = 100*cpih[2020]/cpih[y]\n")
cpih |> mutate(real_value_of_5000_grant = round(5000 * cpih[year==2020] / cpih)) |>
  as.data.frame() |> print(row.names = FALSE)
cat("\n2025 rent-adjusted real value (haircut, capped 5000): LAD vs TTWA geography\n")
ref |> filter(year == 2025,
              provider %in% c("University of Sunderland", "University of Hull",
                              "The University of Manchester", "University of Oxford",
                              "King's College London", "University College London")) |>
  transmute(provider, lad_name, ttwa = ttwa_name,
            rv_lad = real_value_rent_gbp, rv_ttwa = real_value_rent_ttwa_gbp) |>
  arrange(-rv_lad) |> as.data.frame() |> print(row.names = FALSE)
cat("\nTTWA coverage: ", sum(!is.na(ref$ttwa_rent[ref$year==2025])), "/",
    sum(ref$year==2025), " HEI-rows have a TTWA rent\n", sep = "")

# NEW weighted-index building blocks: general (CPI) erosion, and a worked cost
# index at w=0.5 for a cheap vs an expensive place. This is what real_value.r
# turns into real_value = nominal / cost_index.
cat("\nCPI (ex-housing) general erosion of a frozen 5000 grant (non-housing leg only):\n")
tibble(year = YEARS) |>
  left_join(cpi, by = "year") |>
  mutate(gen_rel = cpi / cpi_base,
         real_value_general_only = round(5000 / gen_rel)) |>
  select(year, real_value_general_only) |> as.data.frame() |> print(row.names = FALSE)
cat("\nWorked weighted cost index (w=0.5) and real value of a 5000 grant, 2025:\n")
ref |> filter(year == 2025,
              provider %in% c("University of Sunderland", "University of Oxford",
                              "King's College London")) |>
  transmute(provider, ttwa = ttwa_name,
            cost_index_w50 = round(0.5 * rent_rel_ttwa + 0.5 * gen_rel, 3),
            real_value_w50 = round(5000 / (0.5 * rent_rel_ttwa + 0.5 * gen_rel))) |>
  arrange(-real_value_w50) |> as.data.frame() |> print(row.names = FALSE)

message("\nWritten: provider_costofliving.csv, cpih_index.csv, cpi_index.csv, lsf_awards.csv")