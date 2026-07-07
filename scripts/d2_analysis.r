purrr::walk(list.files("functions", full.names = TRUE), source)
library(readr); library(dplyr); library(tidyr); library(ggplot2)

long <- read_csv(file.path(derived_dir(), "financial_confidence_long.csv"),
                 show_col_types = FALSE)

# Short, chart-ready labels; drop non-response rows (see labels.R for options)
long <- tidy_groups(long)

# Show what survived, so you can veto the typology before trusting the charts
cat("\n==== groups kept after tidy ====\n\n")
print(long |> distinct(Demographic, Group) |> arrange(Demographic, Group) |> as.data.frame(),
      row.names = FALSE)

banded <- add_net(confidence_by_group(long))
write_csv(banded, file.path(derived_dir(), "financial_confidence_by_band.csv"))

ranking <- banded[, c("Demographic", "Group", "n",
                      "Unconfident_pct", "Neutral_pct", "Confident_pct", "Net_pct")]
ranking <- ranking[order(ranking$Net_pct), ]
cat("\n==== ranked by net confidence (least confident first) ====\n\n")
print(as.data.frame(ranking), row.names = FALSE)

save_confidence_plot(plot_confidence_ranked(banded),    "financial_confidence_ranked.png")
save_confidence_plot(plot_confidence_diverging(banded), "financial_confidence_diverging.png")