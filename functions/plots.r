# ANALYSIS-STAGE visuals for banded confidence data (output of
# confidence_by_group). Grand Total rows are dropped so only real subgroups
# are compared. Net confidence = Confident% - Unconfident%.
# Small multiples: one facet per demographic, group names only on the axis.

theme_confidence <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(colour = "grey35"),
      legend.position    = "top",
      strip.text         = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

add_net <- function(banded) {
  banded$Net_pct <- banded$Confident_pct - banded$Unconfident_pct
  banded
}

# Internal: unique Demographic+Group key, ordered by net, so within-panel
# sorting is correct even when group labels repeat across demographics.
.net_key <- function(d) {
  k <- paste(d$Demographic, d$Group, sep = "___")
  d$key <- factor(k, levels = k[order(d$Net_pct)])
  d
}
.strip_key <- function(x) sub("^.*___", "", x)

# HEADLINE CHART: net-confidence bars, one small panel per demographic.
plot_confidence_ranked <- function(banded,
    title = "Net financial confidence by group",
    subtitle = "Confident (4-5) minus Unconfident (1-2), percentage points",
    ncol = 3) {

  d <- .net_key(add_net(banded[!banded$is_total, ]))
  d$sign <- ifelse(d$Net_pct >= 0, "Net confident", "Net unconfident")

  ggplot2::ggplot(d, ggplot2::aes(x = Net_pct, y = key, fill = sign)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::scale_y_discrete(labels = .strip_key) +
    ggplot2::scale_fill_manual(values = c("Net confident"   = "#2166AC",
                                          "Net unconfident" = "#B2182B")) +
    ggplot2::facet_wrap(~ Demographic, scales = "free_y", ncol = ncol) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = "Net confidence (pp)", y = NULL, fill = NULL) +
    theme_confidence()
}

# BREAKDOWN CHART: diverging Likert, one panel per demographic.
plot_confidence_diverging <- function(banded,
    title = "Financial confidence in covering expenses next year",
    subtitle = "Unconfident (1-2)  <-  share of group  ->  Confident (4-5)",
    ncol = 2) {

  d <- add_net(banded[!banded$is_total, ])
  d <- d[order(d$Demographic, d$Net_pct), ]
  d$key <- factor(paste(d$Demographic, d$Group, sep = "___"),
                  levels = paste(d$Demographic, d$Group, sep = "___"))
  d$y <- as.integer(d$key)

  seg <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    U <- d$Unconfident_pct[i]; N <- d$Neutral_pct[i]; C <- d$Confident_pct[i]
    data.frame(
      Demographic = d$Demographic[i], y = d$y[i], glab = d$Group[i],
      band = c("Unconfident", "Neutral", "Confident"),
      xmin = c(-(N/2 + U), -N/2, N/2),
      xmax = c(-N/2,        N/2,  N/2 + C),
      stringsAsFactors = FALSE
    )
  }))
  seg$band <- factor(seg$band, levels = c("Unconfident", "Neutral", "Confident"))
  ylab <- setNames(d$Group, d$y)

  ggplot2::ggplot(seg) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = y - 0.42, ymax = y + 0.42, fill = band)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::scale_y_continuous(breaks = as.integer(names(ylab)), labels = ylab,
                                expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_fill_manual(values = c(Unconfident = "#B2182B",
                                          Neutral     = "#BDBDBD",
                                          Confident   = "#2166AC")) +
    ggplot2::facet_wrap(~ Demographic, scales = "free_y", ncol = ncol) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "% of group", y = NULL, fill = NULL) +
    theme_confidence()
}

# Save a plot to _derived. Bump width/height if a demographic has many groups.
save_confidence_plot <- function(p, filename, width = 11, height = 8) {
  path <- file.path(derived_dir(), filename)
  ggplot2::ggsave(path, p, width = width, height = height, dpi = 200, bg = "white")
  message("Written: ", path)
  invisible(path)
}