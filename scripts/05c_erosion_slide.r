# ===========================================================================
# scripts/05c_erosion_slide.r
#
# THE SPINE SLIDE: the LSF's cash value is frozen, but its REAL value has
# eroded. One clean, defensible headline chart for the deck opener:
#   - Nominal grant: flat £5,000 (core training grant).
#   - Real value:    £5,000 deflated by CPI (general prices, ex owner-occupier
#                    housing = the honest national real-terms measure).
#   The widening gap = purchasing power lost. ~23% by 2026.
#
# Uses CPI-only (not the weighted rent+CPI headline) on purpose: this is the
# unimpeachable national real-terms number for the opener. The weighted measure
# is for the regression arms; this slide is the "why it matters" hook.
#
# Reads: reference/cpi_index.csv (written by g1). Run any time after g1.
#   source("scripts/05c_erosion_slide.r", encoding = "UTF-8")
# Outputs -> outputs_dir()/slide_rv_erosion.png
# ===========================================================================

purrr::walk(list.files("functions", full.names = TRUE), source)
suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(tidyr); library(stringr)
})

REF_DIR   <- "reference"
CORE      <- 5000L      # universal training grant (frozen 2020-2026)
BASE_YEAR <- 2020L

teal   <- dcol("dhsc_teal", "#01A188")
grey   <- dcol("midgrey", "#6F777B")
risk   <- dcol("risk", "#D4351C")
ink    <- dcol("ink", "#0B0C0C")

wrap_title <- function(x, w = 54) str_wrap(x, width = w)
wrap_sub   <- function(x, w = 96) str_wrap(x, width = w)
wrapcap    <- function(x, w = 120) str_wrap(x, width = w)

# ---- build the erosion series ---------------------------------------------
cpi <- read_csv(file.path(REF_DIR, "cpi_index.csv"), show_col_types = FALSE)
if (!all(c("year", "cpi") %in% names(cpi))) stop("cpi_index.csv missing year/cpi (run g1).")
cpi_base <- cpi$cpi[cpi$year == BASE_YEAR][1]
if (is.na(cpi_base)) stop("No CPI value for base year 2020.")

ero <- cpi |>
  transmute(
    year    = as.integer(year),
    nominal = CORE,
    real    = CORE * cpi_base / cpi
  ) |>
  arrange(year)

end   <- ero |> slice_max(year, n = 1)
loss_gbp <- round(end$nominal - end$real)
loss_pct <- round(100 * (1 - end$real / end$nominal))
real_end <- round(end$real)

ero_long <- ero |>
  pivot_longer(c(nominal, real), names_to = "series", values_to = "gbp") |>
  mutate(series = recode(series,
                         nominal = "Cash value (frozen)",
                         real    = "Real value (CPI-adjusted)"))

# ---- plot ------------------------------------------------------------------
p <- ggplot(ero, aes(x = year)) +
  # shaded lost-purchasing-power wedge between the two lines
  geom_ribbon(aes(ymin = real, ymax = nominal), fill = risk, alpha = 0.12) +
  geom_line(data = ero_long,
            aes(y = gbp, colour = series, linetype = series), linewidth = 1.3) +
  geom_point(data = ero_long, aes(y = gbp, colour = series), size = 2.2) +
  annotate("text", x = end$year, y = (end$nominal + end$real) / 2,
           label = sprintf("£%s lost\n(-%d%%)", format(loss_gbp, big.mark = ","), loss_pct),
           hjust = 1.05, vjust = 0.5, colour = risk, fontface = "bold",
           size = 4.6, lineheight = 0.95) +
  scale_colour_manual(values = c("Cash value (frozen)" = grey,
                                 "Real value (CPI-adjusted)" = teal)) +
  scale_linetype_manual(values = c("Cash value (frozen)" = "22",
                                    "Real value (CPI-adjusted)" = "solid")) +
  scale_x_continuous(breaks = ero$year, expand = expansion(mult = c(0.02, 0.10))) +
  scale_y_continuous(limits = c(0, CORE * 1.05),
                     labels = function(z) paste0("£", format(z, big.mark = ",", trim = TRUE)),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = wrap_title(sprintf(
      "The Learning Support Fund has lost about %d%% of its real value since 2020", loss_pct)),
    subtitle = wrap_sub(sprintf(
      paste0("The core training grant has been frozen at £%s since 2020. After general inflation ",
             "(CPI), it is worth about £%s in 2020 money by %d, roughly %d%% less. The shaded ",
             "wedge is purchasing power lost to a frozen cash value."),
      format(CORE, big.mark = ","), format(real_end, big.mark = ","), end$year, loss_pct)),
    x = NULL, y = NULL, colour = NULL, linetype = NULL,
    caption = wrapcap(paste0(
      "Source: ONS CPI all-items index (D7BT), DHSC analysis. Real value = £", format(CORE, big.mark = ","),
      " x CPI(2020)/CPI(year). CPI excludes owner-occupier housing; where local rents rose faster ",
      "than the national basket, the real-value loss for students in high-cost areas is larger."))
  ) +
  theme_dhsc_slide(15) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        plot.margin = margin(16, 22, 12, 14))

save_slide(p, file.path(outputs_dir(), "slide_rv_erosion.png"))

cat("\nErosion series (CPI-adjusted real value of a frozen £", CORE, " grant):\n", sep = "")
print(as.data.frame(ero |> mutate(real = round(real))), row.names = FALSE)
cat(sprintf("\n%d: real value £%s = %d%% of face; £%s (-%d%%) lost since 2020.\n",
            end$year, format(real_end, big.mark = ","),
            round(100 * real_end / CORE), format(loss_gbp, big.mark = ","), loss_pct))
