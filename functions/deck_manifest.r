# ===========================================================================
# functions/deck_manifest.r
#
# Single source of truth for the presented deck. 03_deck.r iterates this table:
# for each row it calls builder(source_tables) and saves the plot as
# slide_NN_slug.png, where NN is the row's number. Slide numbers come from HERE,
# never hard-coded in plotting code. Reordering the deck = edit this table and
# re-run 03 only. The plotting layer fits no models; it only reads the CSVs
# listed in source_tables.
#
# ORDER (set 2026-07-24). The deck opens on the problem, not on descriptives:
# erosion / place / package are the three channels by which the same frozen
# £5,000 is worth less (time, place, package), and they were built as slides
# one-two-three. Retention then states what is to be explained, the arms give
# the finding, and robustness follows so it is there when challenged rather
# than in the way. Placement and equity are numbered annexes.
#
#   A. The problem      1-3    real-value erosion, place, package
#   B. What we explain  4-7    retention and leaving intention
#   C. The finding      8-11   Arms 1-3
#   D. Robustness       12-14  spec ladder, survivorship, honest ceiling
#   E. Placement annex  15-17
#   F. Equity annex     18-24  ethnicity gets its own slide beside each
#                              composite, because it carries too many
#                              categories to read in a facet grid
#   G. Technical annex  25-27  geography, real-value construct, model
#
# Reordering is one edit here plus a re-run of 03. Anything not in this table
# is archived on backup/dev-2026-07-22, not lost.
#
# source_script records where the plotting code currently lives, so task 6
# knows what to move.
# ===========================================================================

deck_manifest <- function() {
  tibble::tribble(
    ~section,        ~number, ~slug,                               ~title,                                             ~builder,                            ~source_tables,                                   ~source_script,
    # --- A. The problem: one frozen grant, three channels of erosion ---
    "A. Problem",         1L, "rv_erosion",                        "LSF real-value erosion since 2020",                "build_slide_rv_erosion",            "reference/cpi_index.csv",                        "05c_erosion_slide",
    "A. Problem",         2L, "rv_place",                          "Real value varies by place",                       "build_slide_rv_place",              "tbl_rv_place.csv",                               "05d_framing_slides",
    "A. Problem",         3L, "rv_package",                        "Real value in the funding package",                "build_slide_rv_package",            "tbl_rv_package.csv",                             "05d_framing_slides",
    # --- B. What we are explaining ---
    "B. Retention",       4L, "retention",                         "Retention funnel",                                 "build_slide_retention",             "tbl_retention_funnel.csv",                       "03_visualise",
    "B. Retention",       5L, "retention_courses",                 "Retention by course",                              "build_slide_retention_courses",     "tbl_retention_by_course.csv",                    "03_visualise",
    "B. Retention",       6L, "intention",                         "Considered-leaving intention",                     "build_slide_intention",             "tbl_dynamics_intention.csv",                     "03_visualise",
    "B. Retention",       7L, "factors",                           "What drives leaving",                              "build_slide_factors",               "tbl_factors.csv",                                "03_visualise",
    # --- C. The finding: the three arms ---
    "C. Finding",         8L, "rv_leave_curve",                    "Arm 1: leaving falls as real value rises",         "build_slide_rv_leave_curve",        "tbl_rv_pred_curves.csv;tbl_rv_pp_per_1k.csv",    "07_real_value_comms",
    "C. Finding",         9L, "rv_hazard_leave_next",              "Arm 2: real value and leaving next year",          "build_slide_rv_hazard_leave_next",  "tbl_hazard_pred_curves.csv;tbl_hazard_1k.csv",   "08_hazard_and_recruitment",
    "C. Finding",        10L, "rv_three_arms",                     "Three arms summary",                               "build_slide_rv_three_arms",         "tbl_rv_spec_ladder.csv;tbl_hazard_1k.csv;tbl_recruit_fe_results.csv", "08_hazard_and_recruitment",
    "C. Finding",        11L, "rv_recruit_effect",                 "Arm 3: no recruitment signal within providers",    "build_slide_rv_recruit_effect",     "tbl_recruit_fe_results.csv",                     "05e_recruit_effect_slide",
    # --- D. Robustness: the answers to "is it just selection?" ---
    "D. Robustness",     12L, "rv_spec_ladder",                    "Arm 1: robust across the spec ladder",             "build_slide_rv_spec_ladder",        "tbl_rv_spec_ladder.csv",                         "05b_visualise_real_value",
    "D. Robustness",     13L, "survivorship",                      "Survivorship and selection",                       "build_slide_survivorship",          "tbl_survivorship.csv",                           "03_visualise",
    "D. Robustness",     14L, "auc",                               "Honest ceiling: how well can we predict",          "build_slide_auc",                   "tbl_auc_decile.csv;tbl_auc_summary.csv",             "03_visualise",
    # --- E. Placement hours annex ---
    "E. Placement",      15L, "placement_programme_lollipop",      "Placement hours by programme",                     "build_slide_placement_lollipop",    "P1_outcomes_by_programme.csv",                   "p4_placement_slides",
    "E. Placement",      16L, "placement_interaction",             "Hours x real value interaction",                   "build_slide_placement_interaction", "P2_marginal_by_hours.csv",                     "p4_placement_slides",
    "E. Placement",      17L, "placement_family_fe",               "Placement hours under family FE",                  "build_slide_placement_family_fe",   "P3_family_fe_results.csv",                       "p4_placement_slides",
    # --- F. Equity (funding triangle) annex ---
    # Source tables verified against what the scripts actually write (2026-07-24):
    # the two triangle entries were draft guesses and are now the real names.
    # 18/20 are the two halves of one cross-tab, each followed by its
    # ethnicity companion (19/21). Ethnicity is split out for legibility only;
    # the rates, reference line and colour rule are identical.
    "F. Equity",         18L, "confidence_confident",              "Financial confidence: confident group",             "build_slide_confidence_confident",  "financial_confidence_by_band.csv",              "d3_visualise_demographics",
    "F. Equity",         19L, "confidence_confident_ethnicity",    "Financial confidence: confident, ethnicity",        "build_slide_confidence_confident_ethnicity",  "financial_confidence_by_band.csv",    "d3_visualise_demographics",
    "F. Equity",         20L, "confidence_unconfident",            "Financial confidence: unconfident group",           "build_slide_confidence_unconfident","financial_confidence_by_band.csv",              "d3_visualise_demographics",
    "F. Equity",         21L, "confidence_unconfident_ethnicity",  "Financial confidence: unconfident, ethnicity",      "build_slide_confidence_unconfident_ethnicity","financial_confidence_by_band.csv",    "d3_visualise_demographics",
    "F. Equity",         22L, "triangle_scatter_risk_dependence",  "Triangle: risk vs dependence",                      "build_slide_triangle_risk_dep",     "triangle_group_matrix.csv",                "d7_triangle_cross_question",
    "F. Equity",         23L, "triangle_rates",                    "Triangle: funding rates by group",                  "build_slide_triangle_rates",        "funding_triangle_rates.csv",                 "d6_funding_triangle_slides",
    "F. Equity",         24L, "triangle_rates_ethnicity",          "Triangle: funding rates, ethnicity",                "build_slide_triangle_rates_ethnicity","funding_triangle_rates.csv",               "d6_funding_triangle_slides",
    # --- G. Technical annex: the method, drawn rather than described ---------
    # These three fit nothing and read nothing. source_tables is empty: the
    # deck layer passes an empty list and the builders ignore it.
    "G. Technical",      25L, "tech_geography",                    "How provider location becomes a local rent",        "build_slide_tech_geography",        "",                                           "deck_builders_technical",
    "G. Technical",      26L, "tech_realvalue",                    "How the frozen grant becomes a real value",         "build_slide_tech_realvalue",        "",                                           "deck_builders_technical",
    "G. Technical",      27L, "tech_model",                        "What the models estimate",                          "build_slide_tech_model",            "",                                           "deck_builders_technical"
  )
}
