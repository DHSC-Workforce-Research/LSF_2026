# ===========================================================================
# functions/analysis_framing.r
#
# analysis_framing()  -  the tables behind the two framing slides that follow
# the erosion spine. Three channels by which the same frozen grant is worth
# less: TIME (the erosion slide, computed from the committed CPI reference),
# PLACE (this file), PACKAGE (this file).
#
# Taken from scripts/05d_framing_slides.r. Unlike the other task-5 moves this
# is NOT a pure relocation: 05d computed both tables inside the slide script
# and never persisted them, so the deck layer had no CSV to read. The
# computation is carried over unchanged and two write_csv calls are added.
#
# Three deliberate changes beyond that:
#   - CORE is bound to as.integer(CORE_GRANT). 05d used 5000L; config holds
#     5000 as a double, and the package table right_joins on `amount`, which
#     dplyr refuses across integer and double. The integer type is load-bearing.
#   - the unused local `w` (housing weight, never referenced in 05d) is dropped.
#   - the two ggplot blocks stay in 05d for task 6.
#
# ROW ORDER IS THE CONTRACT. Both tables are written in the exact order the
# slides plot them (place ascending by real value, package ascending by
# amount). Factor levels do not survive a CSV, so the deck layer must rebuild
# them from row order rather than sorting again.
#
# Writes to outputs_dir():
#   tbl_rv_place.csv    provider, short label, real value, reference year
#   tbl_rv_package.csv  package tier, students, share, sample size
# ===========================================================================

analysis_framing <- function() {
  out <- outputs_dir()
  CORE <- as.integer(CORE_GRANT)

  gbp <- function(z) paste0("£", format(round(z), big.mark = ",", trim = TRUE))

  # =========================================================================
  # PLACE: the same grant is worth less where costs are high
  # =========================================================================
  ref <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE)
  if (!all(c("provider", "year", "rent_rel_ttwa") %in% names(ref)))
    stop("provider_costofliving.csv missing rent_rel_ttwa; re-run 90_build_reference.r.")
  yr <- max(ref$year, na.rm = TRUE)

  # HOUSING ONLY: local rent is the thing that varies across places (CPI is
  # national - identical everywhere in a year - so it adds nothing spatial). Deflate
  # the GBP5,000 grant by LOCAL RENT and anchor so the CHEAPEST rent area in the
  # country is worth the full GBP5,000; every more-expensive area is a discount off
  # that (real value <= face value). Time erosion is the separate erosion slide.
  rv_place <- ref |>
    filter(year == yr) |>
    distinct(provider, rent_rel_ttwa) |>
    filter(!is.na(rent_rel_ttwa)) |>
    mutate(rv = CORE * min(rent_rel_ttwa) / rent_rel_ttwa)

  # a recognisable spread of nursing-heavy providers across the cost range; only
  # those whose register name matches are shown (others silently drop).
  curated <- c(
    "University of Sunderland", "University of Hull", "University of Bradford",
    "University of Wolverhampton", "University of Central Lancashire",
    "Sheffield Hallam University", "University of Nottingham",
    "The University of Manchester", "University of Leeds", "University of Birmingham",
    "University of the West of England, Bristol", "University of Southampton",
    "University of Brighton", "University of Oxford",
    "King's College London", "University College London"
  )
  # bookend with the true national extremes: cheapest rent area (= full face
  # value) at the top, most expensive at the bottom, recognisable cities between.
  cheapest_provider <- rv_place |> slice_max(rv, n = 1, with_ties = FALSE) |> pull(provider)
  dearest_provider  <- rv_place |> slice_min(rv, n = 1, with_ties = FALSE) |> pull(provider)
  show_set <- unique(c(cheapest_provider, curated, dearest_provider))
  d_place <- rv_place |> filter(provider %in% show_set) |> arrange(rv)
  matched <- nrow(d_place)
  message("place table: ", matched, " providers (year ", yr, "). National cheapest = ",
          cheapest_provider, " | dearest = ", dearest_provider)
  if (matched < 4) stop("Too few providers matched; check reference / spellings.")

  # short display label (drop "University of", "The", ", Bristol" noise)
  d_place <- d_place |>
    mutate(lab = provider |>
             str_replace("^The University of ", "") |>
             str_replace("^University of ", "") |>
             str_replace(", Bristol$", "") |>
             str_replace("^King's College London$", "King's (London)") |>
             str_replace("^University College London$", "UCL") |>
             str_replace(" College London$", " (London)") |>
             str_trunc(30))
  stopifnot("two curated providers collapse to the same short label" =
              !any(duplicated(d_place$lab)))

  # `year` and `face_value` travel with the table so the deck can write its
  # subtitle without reaching back into the reference data.
  place_tbl <- d_place |>
    transmute(provider, lab, rv, year = yr, face_value = CORE)
  write_csv(place_tbl, file.path(out, "tbl_rv_place.csv"))
  progress("  wrote tbl_rv_place.csv (", nrow(place_tbl), " providers)")

  # =========================================================================
  # PACKAGE: on top of the core grant, some students get more
  # =========================================================================
  SAMPLE <- as.data.frame(readRDS(file.path(derived_dir(), "lsf_analysis_sample.rds")))
  if (!"parental" %in% names(SAMPLE))   SAMPLE$parental   <- FALSE
  if (!"specialist" %in% names(SAMPLE)) SAMPLE$specialist <- FALSE
  par01 <- dplyr::coalesce(to_01(SAMPLE$parental), 0L)
  spe01 <- dplyr::coalesce(to_01(SAMPLE$specialist), 0L)
  n_tot <- length(par01)

  # fixed 4-tier scaffold so a tier with 0 students still shows as a 0% bar
  lvl <- tibble::tribble(
    ~amount,      ~label,
    5000L, "Training grant only",
    6000L, "+ Specialist subject",
    7000L, "+ Parental support",
    8000L, "+ Both top-ups")

  pkg <- tibble(amount = CORE + 2000L * par01 + 1000L * spe01) |>
    count(amount, name = "n") |>
    right_join(lvl, by = "amount") |>
    mutate(n = coalesce(n, 0L), pct = 100 * n / n_tot) |>
    arrange(amount)

  package_tbl <- pkg |>
    transmute(amount, label, n, pct, n_total = n_tot,
              cat = sprintf("%s\n(%s)", label, gbp(amount)))
  write_csv(package_tbl, file.path(out, "tbl_rv_package.csv"))
  progress("  wrote tbl_rv_package.csv (", nrow(package_tbl), " tiers)")

  # ---- console summary -----------------------------------------------------
  cat("\n== PLACE: real value of the core grant by area (", yr, ") ==\n", sep = "")
  print(as.data.frame(d_place |> transmute(provider, real_value_gbp = round(rv))), row.names = FALSE)
  cat("\n== PACKAGE: share of students by total grant ==\n")
  print(as.data.frame(pkg |> transmute(package_gbp = amount, label, students = n, pct = round(pct, 1))),
        row.names = FALSE)

  invisible(list(place = place_tbl, package = package_tbl))
}
