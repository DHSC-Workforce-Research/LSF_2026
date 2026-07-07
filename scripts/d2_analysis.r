purrr::walk(list.files("functions", full.names = TRUE), source)
library(readr); library(dplyr); library(tidyr); library(ggplot2)

long <- tidy_groups(read_csv(file.path(derived_dir(), "financial_confidence_long.csv"),
                             show_col_types = FALSE))

banded <- add_net(confidence_by_group(long))
write_csv(banded, file.path(derived_dir(), "financial_confidence_by_band.csv"))

# Ranked table, MOST unconfident first (the hardship signal)
ranking <- banded[order(-banded$Unconfident_pct),
                  c("Demographic", "Group", "n",
                    "Unconfident_pct", "Neutral_pct", "Confident_pct")]
cat("\n==== ranked by % unconfident (highest first) ====\n\n")
print(as.data.frame(ranking), row.names = FALSE)

# Headline chart: % unconfident by group. Flip to "Confident" for the mirror.
save_confidence_plot(plot_confidence_bar(banded, "Unconfident"),
                     "financial_unconfident_by_group.png")
save_confidence_plot(plot_confidence_bar(banded, "Confident"),
                     "financial_confident_by_group.png")