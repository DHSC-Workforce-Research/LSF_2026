# ===========================================================================
# deck_helpers.r  -  presentation helpers for 12_build_deck.r
#   save_slide()       : ggsave at true 16:9 widescreen (13.33 x 7.5in), high-dpi
#   theme_dhsc_slide() : larger-font variant of theme_dhsc() for on-screen reading
#   dhsc_table_plot()  : render a data frame as a DHSC-branded table IMAGE, built
#                        in pure ggplot2 (teal #01A188 header, Arial, zebra rows).
#                        No officer / gt / chromote, so it runs on the locked
#                        work machine where officer is CRAN-blocked.
#   dcol()             : safe colour lookup into dhsc_cols with a fallback, so a
#                        missing palette key degrades gracefully instead of erroring.
# ===========================================================================

# safe accessor into the existing dhsc_cols palette ------------------------
dcol <- function(name, fallback) {
  v <- tryCatch(dhsc_cols[[name]], error = function(e) NULL)
  if (is.null(v) || (length(v) == 1 && is.na(v))) fallback else v
}

# save any ggplot at a fixed widescreen slide size -------------------------
save_slide <- function(plot, file, width = 13.33, height = 7.5, dpi = 200, bg = "white") {
  ggplot2::ggsave(file, plot, width = width, height = height, units = "in", dpi = dpi, bg = bg)
  progress("  slide -> ", basename(file))
  invisible(file)
}

# slide-legible variant of the house theme ---------------------------------
theme_dhsc_slide <- function(base = 15) {
  theme_dhsc() +
    ggplot2::theme(
      text          = ggplot2::element_text(size = base),
      plot.title    = ggplot2::element_text(size = base * 1.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base * 1.05, colour = "grey30"),
      axis.text     = ggplot2::element_text(size = base * 0.95),
      legend.text   = ggplot2::element_text(size = base * 0.95),
      plot.caption  = ggplot2::element_text(size = base * 0.75, colour = "grey45"),
      plot.margin   = ggplot2::margin(16, 18, 12, 12))
}

# render a data frame as a branded table image (pure ggplot, no extra pkgs) -
#   df     : data frame; every column is coerced to character for display
#   align  : per-column "l"/"r"/"c"; default = first col left, rest right
dhsc_table_plot <- function(df, title = NULL, subtitle = NULL, caption = NULL,
                            align = NULL, header_fill = "#01A188",
                            stripe = "#F2F2F2", text_col = "#262626",
                            base_size = 13, family = "Arial") {
  df  <- dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
  nc  <- ncol(df); nr <- nrow(df); cols <- names(df)
  if (is.null(align)) align <- c("l", rep("r", max(nc - 1, 0)))
  align <- rep(align, length.out = nc)

  body <- df |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(-".row", names_to = "col_name", values_to = "value") |>
    dplyr::mutate(col = match(col_name, cols), y = nr - .row + 1, is_header = FALSE)
  head <- tibble::tibble(col_name = cols, value = cols, col = seq_len(nc),
                         y = nr + 1, .row = 0L, is_header = TRUE)
  ax <- tibble::tibble(col = seq_len(nc), a = align) |>
    dplyr::mutate(xt = dplyr::case_when(a == "l" ~ col - 0.46, a == "r" ~ col + 0.46, TRUE ~ col + 0),
                  hj = dplyr::case_when(a == "l" ~ 0,          a == "r" ~ 1,          TRUE ~ 0.5))
  cells <- dplyr::bind_rows(head, body) |> dplyr::left_join(ax, by = "col")

  ggplot2::ggplot(cells, ggplot2::aes(x = col, y = y)) +
    ggplot2::geom_tile(data = dplyr::filter(cells, !is_header),
                       ggplot2::aes(fill = (.row %% 2 == 0)),
                       width = 1, height = 1, colour = "white", linewidth = 0.7, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = stripe, `FALSE` = "white")) +
    ggplot2::geom_tile(data = dplyr::filter(cells, is_header),
                       fill = header_fill, width = 1, height = 1, colour = "white", linewidth = 0.7) +
    ggplot2::geom_text(data = dplyr::filter(cells, is_header),
                       ggplot2::aes(x = xt, label = value, hjust = hj),
                       colour = "white", fontface = "bold", size = base_size / 2.5, family = family) +
    ggplot2::geom_text(data = dplyr::filter(cells, !is_header),
                       ggplot2::aes(x = xt, label = value, hjust = hj),
                       colour = text_col, size = base_size / 2.85, family = family) +
    ggplot2::scale_x_continuous(limits = c(0.5, nc + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.5, nr + 1.5), expand = c(0, 0)) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_void(base_size = base_size, base_family = family) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = base_size * 1.5, family = family),
      plot.subtitle = ggplot2::element_text(size = base_size * 1.05, colour = "grey30", family = family),
      plot.caption  = ggplot2::element_text(size = base_size * 0.8,  colour = "grey45", family = family),
      plot.margin   = ggplot2::margin(16, 16, 16, 16))
}