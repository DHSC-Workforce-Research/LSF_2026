purrr::walk(list.files("functions", full.names = TRUE), source)
library(readr); library(dplyr); library(tidyr); library(ggplot2)

long   <- read_csv(file.path(derived_dir(), "financial_confidence_long.csv"),
                   show_col_types = FALSE)

banded <- add_net(confidence_by_group(long))
write_csv(banded, file.path(derived_dir(), "financial_confidence_by_band.csv"))

# Ranked table, least confident first (Grand Total rows excluded)
ranking <- banded[!banded$is_total,
                  c("Demographic", "Group", "n",
                    "Unconfident_pct", "Neutral_pct", "Confident_pct", "Net_pct")]
ranking <- ranking[order(ranking$Net_pct), ]
cat("\n==== Ranked by net confidence (least confident first) ====\n\n")
print(as.data.frame(ranking), row.names = FALSE)

# Charts
save_confidence_plot(plot_confidence_diverging(banded), "financial_confidence_diverging.png")
save_confidence_plot(plot_confidence_ranked(banded),    "financial_confidence_ranked.png")