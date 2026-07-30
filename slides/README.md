# slides/

Hand-authored slide generators, separate from the analysis pipeline. These
produce narrative slides (executive summary, section dividers) whose text is
written by hand rather than computed from data, so they do not belong in
02_analysis / 03_deck.

## exec_summary_slide.R
Renders the LSF executive summary as a 16:9 PNG (2666 x 1500, DHSC / GSS
Analysis Function palette). Depends only on ggplot2 and stringr.

    source("slides/exec_summary_slide.R")   # writes exec_summary.png to the working directory

Edit the wording in the `ideas` list at the top; the layout reflows. ASCII-only
apart from the pound sign, so Windows source() will not truncate it.
