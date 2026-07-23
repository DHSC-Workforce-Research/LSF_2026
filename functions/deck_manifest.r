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
# STATUS: DRAFT for Lee to cut at checkpoint 1. The rows below are the default
# proposal from the plan's open question (main deck = longitudinal descriptives
# + real-value story; placement and equity as numbered annexes). Delete rows to
# drop slides, reorder to renumber, edit titles freely. source_script records
# where the plotting code currently lives, so task 6 knows what to move.
#
# Anything not in this table is archived on backup/dev-2026-07-22, not lost.
# ===========================================================================

deck_manifest <- function() {
  tibble::tribble(
    ~section,      ~number, ~slug,                               ~title,                                             ~builder,                          ~source_tables,                                   ~source_script,
    # --- A. Panel (longitudinal descriptives) ---
    "A. Panel",         1L, "retention",                         "Retention funnel",                                 "build_slide_retention",           "tbl_retention_funnel.csv",                       "03_visualise",
    "A. Panel",         2L, "retention_courses",                 "Retention by course",                              "build_slide_retention_courses",   "tbl_retention_by_course.csv",                    "03_visualise",
    "A. Panel",         3L, "intention",                         "Considered-leaving intention",                     "build_slide_intention",           "tbl_dynamics_intention.csv",                     "03_visualise",
    "A. Panel",         4L, "factors",                           "What drives leaving",                              "build_slide_factors",             "tbl_factors.csv",                                "03_visualise",
    # --- B. Real value (the story) ---
    "B. Real value",    5L, "rv_erosion",                        "LSF real-value erosion since 2020",                "build_slide_rv_erosion",          "reference/cpi_index.csv",                        "05c_erosion_slide",
    "B. Real value",    6L, "rv_place",                          "Real value varies by place",                       "build_slide_rv_place",            "tbl_rv_place.csv",                               "05d_framing_slides",
    "B. Real value",    7L, "rv_package",                        "Real value in the funding package",                "build_slide_rv_package",          "tbl_rv_package.csv",                             "05d_framing_slides",
    "B. Real value",    8L, "rv_leave_curve",                    "Arm 1: leaving falls as real value rises",         "build_slide_rv_leave_curve",      "tbl_rv_pred_curves.csv",                         "07_real_value_comms",
    "B. Real value",    9L, "rv_spec_ladder",                    "Arm 1: robust across the spec ladder",             "build_slide_rv_spec_ladder",      "tbl_rv_spec_ladder.csv",                         "05b_visualise_real_value",
    "B. Real value",   10L, "survivorship",                      "Survivorship and selection",                       "build_slide_survivorship",        "tbl_survivorship.csv",                           "02_analyse",
    "B. Real value",   11L, "rv_hazard_leave_next",              "Arm 2: real value and leaving next year",          "build_slide_rv_hazard_leave_next","tbl_hazard_pred_curves.csv;tbl_hazard_1k.csv",   "08_hazard_and_recruitment",
    "B. Real value",   12L, "rv_three_arms",                     "Three arms summary",                               "build_slide_rv_three_arms",       "tbl_rv_spec_ladder.csv;tbl_hazard_1k.csv;tbl_recruit_fe_results.csv", "08_hazard_and_recruitment",
    "B. Real value",   13L, "rv_recruit_effect",                 "Arm 3: recruitment effect",                        "build_slide_rv_recruit_effect",   "tbl_recruit_fe_results.csv",                     "05e_recruit_effect_slide",
    "B. Real value",   14L, "auc",                               "Honest ceiling: how well can we predict",          "build_slide_auc",                 "tbl_rv_auc.csv;tbl_auc_summary.csv",             "02_analyse",
    # --- C. Placement hours annex ---
    "C. Placement",    15L, "placement_programme_lollipop",      "Placement hours by programme",                     "build_slide_placement_lollipop",  "P1_outcomes_by_programme.csv",                   "p4_placement_slides",
    "C. Placement",    16L, "placement_interaction",             "Hours x real value interaction",                   "build_slide_placement_interaction","P2_interaction_results.csv",                    "p4_placement_slides",
    "C. Placement",    17L, "placement_family_fe",               "Placement hours under family FE",                  "build_slide_placement_family_fe", "P3_family_fe_results.csv",                        "p4_placement_slides",
    # --- D. Equity (funding triangle) annex ---
    "D. Equity",       18L, "confidence_confident",              "Financial confidence: confident group",            "build_slide_confidence_confident","financial_confidence_by_band.csv",               "d3_visualise_demographics",
    "D. Equity",       19L, "confidence_unconfident",            "Financial confidence: unconfident group",          "build_slide_confidence_unconfident","financial_confidence_by_band.csv",             "d3_visualise_demographics",
    "D. Equity",       20L, "triangle_scatter_risk_dependence",  "Triangle: risk vs dependence",                     "build_slide_triangle_risk_dep",   "tbl_triangle_cross_question.csv",                "d7_triangle_cross_question",
    "D. Equity",       21L, "triangle_rates",                    "Triangle: funding rates by group",                 "build_slide_triangle_rates",      "tbl_funding_triangle_rates.csv",                 "d6_funding_triangle_slides"
  )
}
