# ===========================================================================
# scripts/05d_framing_slides.r
#
# The two "why real value varies" framing slides that sit after the erosion
# spine (05c) and before the analysis. Three channels of variation:
#   1. TIME    -> 05c_erosion_slide.r (already built)
#   2. PLACE   -> slide_rv_place.png    (this script): same GBP5,000 grant is
#                 worth far less where the cost of living is high.
#   3. PACKAGE -> slide_rv_package.png  (this script): on top of the universal
#                 GBP5,000, some students get parental / specialist top-ups.
#
# ASCII ONLY (except £sign) - source() on the Windows work machine truncates
# a script at the first exotic non-ASCII char.
#
# Reads: reference/provider_costofliving.csv (place) + the analysis sample
# (package). Run any time after g1 + 01.
#   source("scripts/05d_framing_slides.r", encoding = "UTF-8")
# Outputs -> outputs_dir()/slide_rv_place.png , slide_rv_package.png
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(tidyr); library(stringr)
})
# match the DHSC Arial used elsewhere on the slide for data labels/annotations
update_geom_defaults("text", list(family = "Arial"))

REF_DIR <- "reference"
CORE    <- 5000L
w       <- if (exists("HOUSING_WEIGHT")) HOUSING_WEIGHT else 0.5

teal <- dcol("dhsc_teal", "#01A188")
blue <- dcol("dhsc_blue", "#0063BE")
risk <- dcol("risk", "#D4351C")
grey <- dcol("midgrey", "#6F777B")
ink  <- dcol("ink", "#0B0C0C")

wrap_title <- function(x, w = 54) str_wrap(x, width = w)
wrap_sub   <- function(x, w = 96) str_wrap(x, width = w)
wrapcap    <- function(x, w = 122) str_wrap(x, width = w)
gbp <- function(z) paste0("£", format(round(z), big.mark = ",", trim = TRUE))

# ===========================================================================
# SLIDE 2 - PLACE: the same grant is worth less where costs are high
# ===========================================================================
ref <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE)
if (!all(c("provider", "year", "rent_rel_ttwa", "gen_rel") %in% names(ref)))
  stop("provider_costofliving.csv missing weighted-index columns; re-run g1.")
yr <- max(ref$year, na.rm = TRUE)

# real value of the core GBP5,000 grant per provider in the latest year, using
# the SAME weighted cost-of-living deflator as the analysis (housing weight w).
rv_place <- ref |>
  filter(year == yr) |>
  distinct(provider, rent_rel_ttwa, gen_rel) |>
  mutate(rv = CORE / (w * rent_rel_ttwa + (1 - w) * gen_rel)) |>
  filter(!is.na(rv))

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
d_place <- rv_place |> filter(provider %in% curated) |> arrange(rv)
matched <- nrow(d_place)
message("place slide: ", matched, " of ", length(curated), " curated providers matched (year ", yr, ")")
if (matched < 4) stop("Too few curated providers matched the reference names; check spellings.")

# short display label (drop "University of", "The", ", Bristol" noise)
d_place <- d_place |>
  mutate(lab = provider |>
           str_replace("^The University of ", "") |>
           str_replace("^University of ", "") |>
           str_replace(", Bristol$", "") |>
           str_replace(" College London$", " (London)"))
stopifnot("two curated providers collapse to the same short label" =
            !any(duplicated(d_place$lab)))
d_place <- d_place |> mutate(lab = factor(lab, levels = lab))

p_place <- ggplot(d_place, aes(x = rv, y = lab)) +
  geom_vline(xintercept = CORE, linetype = "22", colour = grey, linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = rv, yend = lab), colour = "grey85", linewidth = 0.8) +
  geom_point(aes(colour = rv), size = 5) +
  geom_text(aes(label = gbp(rv)), hjust = -0.15, size = 3.7, colour = ink) +
  annotate("text", x = CORE, y = matched + 0.95, label = "Face value £5,000",
           hjust = 0.5, vjust = 0, size = 3.4, colour = grey, family = "Arial") +
  scale_colour_gradient(low = risk, high = teal, guide = "none") +
  scale_x_continuous(limits = c(0, max(d_place$rv) * 1.20),
                     labels = function(z) paste0("£", format(z, big.mark = ",", trim = TRUE)),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.6))) +
  labs(
    title = wrap_title("The same grant is worth far less where the cost of living is high"),
    subtitle = wrap_sub(paste0(
      "Real value of the universal £5,000 training grant in ", yr,
      ", deflated by local rent and general prices (housing weight ", w,
      "). It stretches furthest in low-cost areas and is most eroded in expensive cities.")),
    x = NULL, y = NULL,
    caption = wrapcap(paste0(
      "Source: ONS private rents + CPI, DHSC analysis. Real value = £5,000 / [",
      w, " x (local rent / national 2020) + ", 1 - w,
      " x (CPI / 2020)]. Illustrative provider selection across the cost range."))
  ) +
  theme_dhsc_slide(15) +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = margin(16, 22, 12, 14))

save_slide(p_place, file.path(outputs_dir(), "slide_rv_place.png"))

# ===========================================================================
# SLIDE 3 - PACKAGE: on top of the core grant, some students get more
# ===========================================================================
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
  arrange(amount) |>
  mutate(cat = factor(sprintf("%s\n(%s)", label, gbp(amount)),
                      levels = sprintf("%s\n(%s)", label, gbp(amount))))
p_package <- ggplot(pkg, aes(x = pct, y = cat, fill = amount)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 4.4, colour = ink) +
  scale_fill_gradient(low = blue, high = teal, guide = "none") +
  scale_x_continuous(limits = c(0, max(pkg$pct) * 1.20),
                     labels = function(z) paste0(z, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(limits = rev) +
  labs(
    title = wrap_title("On top of the core grant, some students receive more"),
    subtitle = wrap_sub(paste0(
      "Every LSF student gets the universal £5,000 training grant. Parents and carers add ",
      "£2,000; shortage-specialist subjects add £1,000; some get both, up to £8,000. ",
      "Share of students at each total package (n = ", format(n_tot, big.mark = ","), ").")),
    x = NULL, y = NULL,
    caption = wrapcap(paste0(
      "Source: NHS LSF analysis sample, DHSC. Non-means-tested core components (training / parental / ",
      "specialist); hardship and expenditure elements excluded."))
  ) +
  theme_dhsc_slide(15) +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = margin(16, 22, 12, 14))

save_slide(p_package, file.path(outputs_dir(), "slide_rv_package.png"))

# ---- console summary -------------------------------------------------------
cat("\n== PLACE: real value of £5,000 grant by area (", yr, ") ==\n", sep = "")
print(as.data.frame(d_place |> transmute(provider, real_value_gbp = round(rv))), row.names = FALSE)
cat("\n== PACKAGE: share of students by total grant ==\n")
print(as.data.frame(pkg |> transmute(package_gbp = amount, label, students = n, pct = round(pct, 1))),
      row.names = FALSE)
