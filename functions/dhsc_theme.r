# ---------------------------------------------------------------------------
# DHSC / Government Analysis Function visual template: one palette, theme,
# scales, and a save helper so every chart shares an aesthetic. Call
# theme_dhsc() plus a dhsc scale on any ggplot. Categorical colours are the
# cross-government Analysis Function palette (accessible); plus DHSC brand
# accents and semantic risk/protective colours for effect plots.
# ---------------------------------------------------------------------------

dhsc_cols <- c(
  # Government Analysis Function categorical palette (the official, accessible set)
  af_blue = "#12436D", af_teal = "#28A197", af_pink = "#801650",
  af_orange = "#F46A25", af_grey = "#3D3D3D", af_purple = "#A285D1",
  # DHSC brand accents
  dhsc_teal = "#01A188", dhsc_blue = "#0063BE",
  # semantic + neutral
  risk = "#D4351C", good = "#28A197",
  ink = "#0B0C0C", midgrey = "#6F777B", gridgrey = "#E6E6E6"
)

# ordered categorical palette
dhsc_pal <- function() unname(dhsc_cols[c("af_blue","af_teal","af_orange","af_pink","af_purple","af_grey")])

theme_dhsc <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text                  = ggplot2::element_text(colour = dhsc_cols[["ink"]]),
      plot.title            = ggplot2::element_text(face = "bold", size = base_size + 4,
                                                    margin = ggplot2::margin(b = 4)),
      plot.subtitle         = ggplot2::element_text(colour = dhsc_cols[["midgrey"]],
                                                    margin = ggplot2::margin(b = 10)),
      plot.caption          = ggplot2::element_text(colour = dhsc_cols[["midgrey"]],
                                                    size = base_size - 2, hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title            = ggplot2::element_text(colour = dhsc_cols[["midgrey"]]),
      panel.grid.minor      = ggplot2::element_blank(),
      panel.grid.major      = ggplot2::element_line(colour = dhsc_cols[["gridgrey"]], linewidth = .4),
      strip.text            = ggplot2::element_text(face = "bold", hjust = 0, colour = dhsc_cols[["ink"]]),
      legend.position       = "top",
      legend.title          = ggplot2::element_blank(),
      plot.background       = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin           = ggplot2::margin(14, 18, 12, 14)
    )
}

scale_colour_dhsc <- function(...) ggplot2::scale_colour_manual(values = dhsc_pal(), ...)
scale_fill_dhsc   <- function(...) ggplot2::scale_fill_manual(values = dhsc_pal(), ...)

# semantic two-colour scale for effect / forest plots (risk vs protective)
scale_colour_dhsc_dir <- function(more = "More likely", less = "Less likely (protective)") {
  ggplot2::scale_colour_manual(
    values = stats::setNames(c(dhsc_cols[["risk"]], dhsc_cols[["good"]]), c(more, less)))
}

# ggsave with project defaults (white background, decent resolution)
save_dhsc <- function(plot, file, width = 8.5, height = 7, dpi = 200) {
  ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = "white")
}