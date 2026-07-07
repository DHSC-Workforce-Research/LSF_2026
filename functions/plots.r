# ANALYSIS-STAGE visuals for banded confidence data (output of
# confidence_by_group). All charts drop the Grand Total rows so only real
# subgroups are compared. Net confidence = Confident% - Unconfident%.

theme_confidence <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(colour = "grey30"),
      legend.position    = "top",
      strip.text.y       = ggplot2::element_text(angle = 0, face = "bold", hjust = 0)
    )
}

# Add Net_pct = Confident_pct - Unconfident_pct
add_net <- function(banded) {
  banded$Net_pct <- banded$Confident_pct - banded$Unconfident_pct
  banded
}

# Diverging Likert bar: Unconfident (left, negative), Neutral (centred),
# Confident (right, positive). Faceted by demographic, groups sorted by net.
plot_confidence_diverging <- function(banded,
    title    = "Financial confidence in covering expenses next year",
    subtitle = "Unconfident (1-2)  <-   share of group   ->  Confident (4-5)") {

  d <- add_net(banded[!banded$is_total, ])
  d <- d[order(d$Demographic, d$Net_pct), ]        # contiguous block per demographic
  d$Group <- factor(d$Group, levels = unique(d$Group))
  d$y <- as.integer(d$Group)

  seg <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    U <- d$Unconfident_pct[i]; N <- d$Neutral_pct[i]; C <- d$Confident_pct[i]
    data.frame(
      Demographic = d$Demographic[i], y = d$y[i],
      band = c("Unconfident", "Neutral", "Confident"),
      xmin = c(-(N/2 + U), -N/2, N/2),
      xmax = c(-N/2,        N/2,  N/2 + C),
      stringsAsFactors = FALSE
    )
  }))
  seg$band <- factor(seg$band, levels = c("Unconfident", "Neutral", "Confident"))

  ggplot2::ggplot(seg) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = y - 0.42, ymax = y + 0.42, fill = band)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::scale_y_continuous(breaks = d$y, labels = as.character(d$Group),
                                expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_fill_manual(values = c(Unconfident = "#B2182B",
                                          Neutral     = "#BDBDBD",
                                          Confident   = "#2166AC")) +
    ggplot2::facet_grid(Demographic ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(title = title, subtitle = subtitle, x = "% of group", y = NULL, fill = NULL) +
    theme_confidence()
}

# Net-confidence ranking across ALL groups: one bar per group, sorted, sign-coloured.
plot_confidence_ranked <- function(banded,
    title    = "Net financial confidence by group",
    subtitle = "Confident (4-5) minus Unconfident (1-2), in percentage points") {

  d <- add_net(banded[!banded$is_total, ])
  d$label <- stats::reorder(paste(d$Demographic, d$Group, sep = ": "), d$Net_pct)
  d$sign  <- ifelse(d$Net_pct >= 0, "Net confident", "Net unconfident")

  ggplot2::ggplot(d, ggplot2::aes(x = Net_pct, y = label, fill = sign)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.0f", Net_pct),
                                    hjust = ifelse(d$Net_pct >= 0, -0.2, 1.2)),
                       size = 3, colour = "grey20") +
    ggplot2::scale_fill_manual(values = c("Net confident"   = "#2166AC",
                                          "Net unconfident" = "#B2182B")) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = "Net confidence (percentage points)", y = NULL, fill = NULL) +
    theme_confidence()
}

# Save a plot to _derived
save_confidence_plot <- function(p, filename, width = 9, height = 8) {
  path <- file.path(derived_dir(), filename)
  ggplot2::ggsave(path, p, width = width, height = height, dpi = 200, bg = "white")
  message("Written: ", path)
  invisible(path)
}