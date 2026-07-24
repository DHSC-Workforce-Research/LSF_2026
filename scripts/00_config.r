# ===========================================================================
# scripts/00_config.r
#
# Single source of truth for every constant the pipeline uses. Sourced first by
# 01_data.r, 02_analysis.r and 03_deck.r:
#
#   source("scripts/00_config.r")
#
# It loads functions/ and then sets the constants, so a value defined here wins
# over any leftover definition inside a function file. Nothing here reads data,
# fits a model or draws anything; changing a value here is the only way to move
# a headline number, and the numbers harness (tests/check_numbers.r) will say so.
#
# Values are carried over verbatim from where they were previously defined
# mid-script; the source is named in the comment on each block. Where two
# scripts used the same NAME for different values (GRID_N, MIN_N), both values
# are kept under distinct names - nothing is unified, nothing is rounded.
#
# ASCII only in this repo except the pound sign; Windows source() truncates the
# file at the first exotic character.
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)

# ---- paths -----------------------------------------------------------------
# Data and output roots live in functions/paths.r (data_dir/derived_dir/
# outputs_dir); only the in-repo reference folder is a constant.
REF_DIR <- "reference"                       # was: 04, 05c, 05d, 06, 07, 08, p0, p2, p3

# ---- panel window ----------------------------------------------------------
YEARS      <- 2020:2026                      # was: g1 (YEARS)
BASE_YEAR  <- 2020L                          # index base year; was: g1 (BASE/BASE_YEAR), 05c
DROP_YEARS <- c(2020L)                       # thin pilot year, dropped from Arm 3; was: 08

# ---- outcome and headline measure ------------------------------------------
OUTCOME     <- "left_before_finish"          # was: 04
PRIMARY     <- "real_value_rent_ttwa_cpih"   # headline real-value column; was: 05, 06, 07, 08, p2
PRIMARY_LBL <- "Weighted CoL rent+CPI (TTWA)"

# ---- cost-of-living deflator -----------------------------------------------
# real_value = nominal / [ w * rent_rel + (1 - w) * gen_rel ].
# w = housing share of a student's budget (DfE SIES); 0.5 central, 0.4/0.6 band.
# This is the knob that moves the real-value headline. Was: functions/real_value.r,
# read defensively by 05d and 08 via exists(); now defined once, here.
HOUSING_WEIGHT      <- 0.5
HOUSING_WEIGHT_BAND <- c(0.4, 0.6)           # sensitivity band (rv_w40_ttwa / rv_w60_ttwa)

# ---- award schedule (nominal, frozen 2020-2026) ----------------------------
CORE_GRANT         <- 5000                   # universal training grant; was: g1 (CORE_GRANT), 05c (CORE), 08 (literal)
PARENTAL_SUPPORT   <- 2000                   # parental support top-up
SPECIALIST_SUBJECT <- 1000                   # specialist subject top-up

# ---- fixed-effects specifications ------------------------------------------
FE_STUDENT <- "course + entry_year"          # student-level models; was: 06, p2
FE_PANEL   <- "course + year"                # panel/wave models;   was: 06, p2
FE_FAMILY  <- "course_family + entry_year"   # placement family FE; was: p3

# ---- survey and component controls -----------------------------------------
SURVEY_VARS <- c("fund_availability", "grant_influence", "crit_course",
                 "crit_uni", "grant_helps_stay")          # was: 05, 06
SURVEY_RHS  <- paste(SURVEY_VARS, collapse = " + ")
COMP_VARS   <- c("parental", "specialist", "regional")    # was: 05 (COMP_VARS), p3 (CONTROLS)
COMP_RHS    <- paste(COMP_VARS, collapse = " + ")

# ---- reporting scenarios ---------------------------------------------------
POUND_STEPS <- c(500, 1000)                  # "£X less real LSF" scenarios; was: 06
POUNDS_LESS <- 1000                          # the single step p2 reports in; was: p2

# ---- prediction grids ------------------------------------------------------
# Two different values under one old name; both kept.
GRID_N_CURVE  <- 60L                         # was: 07 (GRID_N), predicted-probability curves
GRID_N_HAZARD <- 50L                         # was: 08 (GRID_N), hazard curves

# ---- sample-size floors ----------------------------------------------------
MIN_PROVIDER_N     <- 50L                    # first-years in a provider-year cell; was: 08
MIN_PROVIDER_YEARS <- 3L                     # years a provider needs in the panel; was: 08
MIN_BAND_N         <- 200L                   # students in a placement-hours band; was: p2
MIN_SD_GBP         <- 150                    # within-course SD of real value; was: p2
MIN_PROGRAMME      <- 2L                     # programmes per family, else no within variation; was: p3
MIN_RANGE_H        <- 50                     # placement-hours spread that identifies; was: p3
MIN_MODEL_N        <- 200L                   # students per placement model; was: p3 (MIN_N)

# ---- disclosure ------------------------------------------------------------
MIN_CELL_N <- 10L                            # suppression floor for published cells; was: p1, p1b, d5 (MIN_N)
TOP_BOX    <- c("4", "5")                    # "important" on the 1-5 scales; was: d5

# ---- workbook parsing ------------------------------------------------------
MAX_ROW  <- 200L                             # generous row scan; blank trailing rows ignored; was: d4
SCAN_COL <- 8L                               # read wide, trim to detected width per sheet; was: d4
