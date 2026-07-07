# ANALYSIS-STAGE visuals. Grand Total rows are dropped so only real subgroups
# are compared. Percentages are each group's own share (denominator = group n).

theme_confidence <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(colour = "grey35"),
      legend.position    = "none",
      strip.text         = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

add_net <- function(banded) {
  banded$Net_pct <- banded$Confident_pct - banded$Unconfident_pct
  banded
}

.strip_key <- function(x) sub("^.*___", "", x)

# HEADLINE: % in one band per group (default Unconfident = ratings 1-2).
# Sorted worst-first, faceted by demographic, dashed line at the survey average.
#   band: "Unconfident", "Confident", or "Neutral"
plot_confidence_bar <- function(banded, band = "Unconfident",
                                title = NULL, ncol = 3, show_values = TRUE) {
  pct_col <- paste0(band, "_pct")
  d <- banded[!banded$is_total, ]
  d$value <- d[[pct_col]]
  d$key <- factor(paste(d$Demographic, d$Group, sep = "___"),
                  levels = paste(d$Demographic, d$Group, sep = "___")[order(d$value)])

  ref  <- stats::weighted.mean(d$value, d$n)
  fill <- switch(band, Unconfident = "#B2182B", Confident = "#2166AC", "#7F7F7F")
  opts <- switch(band, Unconfident = "1-2", Confident = "4-5", "3")
  if (is.null(title)) title <- sprintf("%% %s by group", tolower(band))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = key)) +
    ggplot2::geom_col(fill = fill, width = 0.75) +
    ggplot2::geom_vline(xintercept = ref, linetype = "dashed",
                        colour = "grey30", linewidth = 0.3) +
    ggplot2::scale_y_discrete(labels = .strip_key) +
    ggplot2::facet_wrap(~ Demographic, scales = "free_y", ncol = ncol) +
    ggplot2::labs(title = title,
                  subtitle = sprintf("Dashed line = survey average (%.0f%%)", ref),
                  x = sprintf("%% %s (rated %s)", tolower(band), opts), y = NULL) +
    theme_confidence()

  if (show_values)
    p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", value)),
                                hjust = -0.15, size = 2.6, colour = "grey25")
  p + ggplot2::expand_limits(x = max(d$value) * 1.08)
}

# OPTIONAL alternative: net-confidence ranking (kept for reference, not called
# by default). Net = Confident% - Unconfident%.
plot_confidence_ranked <- function(banded, ncol = 3) {
  d <- add_net(banded[!banded$is_total, ])
  d$key <- factor(paste(d$Demographic, d$Group, sep = "___"),
                  levels = paste(d$Demographic, d$Group, sep = "___")[order(d$Net_pct)])
  d$sign <- ifelse(d$Net_pct >= 0, "pos", "neg")
  ggplot2::ggplot(d, ggplot2::aes(x = Net_pct, y = key, fill = sign)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::scale_y_discrete(labels = .strip_key) +
    ggplot2::scale_fill_manual(values = c(pos = "#2166AC", neg = "#B2182B")) +
    ggplot2::facet_wrap(~ Demographic, scales = "free_y", ncol = ncol) +
    ggplot2::labs(title = "Net financial confidence by group",
                  x = "Net confidence (pp)", y = NULL) +
    theme_confidence()
}

save_confidence_plot <- function(p, filename, width = 11, height = 8) {
  path <- file.path(derived_dir(), filename)
  ggplot2::ggsave(path, p, width = width, height = height, dpi = 200, bg = "white")
  message("Written: ", path)
  invisible(path)
}