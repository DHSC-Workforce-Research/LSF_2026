# ===========================================================================
# functions/deck_text.r
#
# EVERY WORD ON EVERY SLIDE. One file, keyed by the manifest slug. If a string
# appears on a slide, it is here: title, subtitle, caption, and the odd callout
# box. The builders in deck_builders*.r hold geometry and data handling only,
# and reach for text through lbl().
#
# HOW IT WORKS
#   lbl("rv_place", "title")                     -> the wrapped title
#   lbl("rv_place", "subtitle", core = "£5,000") -> {core} substituted, then wrapped
#   lbl_raw(...)                                  -> the same without wrapping
#
#   Braces are the only templating: {name} in the text is replaced by the
#   named argument. An unmatched brace is left alone and is therefore visible
#   on the slide, which is the loud failure we want.
#
#   Fragments shared across slides (the source lines) live in `.common` and are
#   substituted automatically wherever {src_panel} and friends appear, so the
#   provenance sentence is written once.
#
# WRAP WIDTHS
#   Each entry carries its own wrap widths, because the slides came from six
#   different scripts and wrap at different points. Changing a width here moves
#   where that slide's title breaks and nothing else.
#
# THE SECURE OVERLAY
#   Titles state findings, and findings do not belong in a public repo before
#   publication. So the strings below are the descriptive, safe versions, and
#   _derived/slide_labels.json on the secure machine overlays the ones that
#   state a result. Same keys, same braces. The overlay is read once per
#   session; a missing file is normal and silent, and the deck still builds.
#
#   If the repo is made private, delete the overlay and put the real titles
#   straight in here. Nothing else changes.
# ===========================================================================

# ---- shared fragments ------------------------------------------------------
deck_text_common <- function() list(
  src_panel = "Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis.",

  src_comms = paste0(
    "Source: NHS LSF panel 2020-2026, DHSC analysis. Real LSF = nominal package deflated by a ",
    "weighted cost-of-living index ({w_rent}% local rent TTWA, {w_cpi}% CPI). Predicted ",
    "probabilities for a typical student profile. Associational, not causal."),

  src_d3 = paste(
    "Source: NHS Learning Support Fund sample demographics (BSA questionnaire",
    "analysis cross-tabs), DHSC analysis. Shares are within-group; not a model",
    "of leaving. Financial confidence is asked of continuing (year 2+) students."),

  src_d6 = paste(
    "Source: NHS Learning Support Fund demographic cross-tabs (BSA questionnaire",
    "analysis), DHSC analysis. Within-group shares; denominator is the summed",
    "response cells. Equity/distributional, not a model of leaving. Non-response",
    "and 'prefer not to say' excluded; groups under n=10 suppressed."),

  src_d7 = paste(
    "Source: NHS Learning Support Fund demographic cross-tabs (BSA), DHSC analysis.",
    "GROUP-LEVEL (ecological) associations, not individual. leave_course and",
    "confidence are continuing (y2+) items; funding-influence items are first-year,",
    "so links assume stable group composition across waves. PNTS/non-response",
    "excluded; groups under n=10 suppressed."),

  src_placement = "Source: NHS LSF panel 2020-2026; DHSC programme placement hours, FY26/27.",

  note_zoom = "Note: y-axis does not start at 0% (scale zoomed to show the gradient)."
)

# ---- the slides ------------------------------------------------------------
deck_text_defaults <- function() list(

  # === A. PROBLEM ==========================================================

  rv_erosion = list(
    wrap = c(title = 54, subtitle = 96, caption = 122),
    title = "The Learning Support Fund has lost about {loss_pct}% of its real value since 2020",
    subtitle = paste0(
      "The core training grant has been frozen at {core} since 2020. After general inflation (CPI), ",
      "it is worth about {real_end} in 2020 money by {end_year}, roughly {loss_pct}% less. The shaded ",
      "wedge is purchasing power lost to a frozen cash value."),
    caption = paste0(
      "Source: ONS CPI all-items index (D7BT), DHSC analysis. Real value = {core} x CPI(2020)/CPI(year). ",
      "CPI excludes owner-occupier housing; where local rents rose faster than the national basket, the ",
      "real-value loss for students in high-cost areas is larger.")
  ),

  rv_place = list(
    wrap = c(title = 54, subtitle = 96, caption = 122),
    title = "The same grant is worth far less where the cost of living is high",
    subtitle = paste0(
      "How far the universal {core} training grant stretches against LOCAL RENT in {year}, scaled so it ",
      "is worth the full {core} where rents are lowest and less where they are higher. The cheapest and ",
      "most expensive university areas in the country are shown at the ends."),
    caption = paste0(
      "Source: ONS private rents (TTWA), DHSC analysis. {core} deflated by local rent only, anchored so ",
      "the lowest-rent English university area equals face value. Recognisable providers plus the ",
      "national cheapest and most expensive.")
  ),

  rv_package = list(
    wrap = c(title = 54, subtitle = 96, caption = 122),
    title = "On top of the core grant, some students receive more",
    subtitle = paste0(
      "Every LSF student gets the universal {core} training grant. Parents and carers add {parental}; ",
      "shortage-specialist subjects add {specialist}; some get both, up to {package_max}. Share of ",
      "students at each total package (n = {n_total})."),
    caption = paste0(
      "Source: NHS LSF analysis sample, DHSC. Non-means-tested core components (training / parental / ",
      "specialist); hardship and expenditure elements excluded.")
  ),

  # === B. RETENTION ========================================================
  # Titles here are the descriptive versions. The findings-bearing titles are
  # overlaid from _derived/slide_labels.json.

  retention = list(
    wrap = c(title = 72, subtitle = 118, caption = 135),
    title = "Retention funnel",
    subtitle = "Of every 100 students who start, the share still enrolled at the start of each study year.",
    caption = paste0(
      "Course length derived from the data. Only cohorts old enough to be observed to their final year ",
      "are included (3-year: 2021-2023 starts; 4-year: 2021-2022). 2020 launch year excluded. {src_panel}")
  ),

  retention_courses = list(
    wrap = c(title = 72, subtitle = 118, caption = 135),
    title = "Retention by course",
    subtitle = paste0(
      "Of every 100 students who start each course, the share still enrolled at the start of each study ",
      "year. The eight biggest courses."),
    caption = paste0(
      "Observed proportions, not modelled. Only cohorts observed to their final year are included. ",
      "2020 launch year excluded. {src_panel}")
  ),

  intention = list(
    wrap = c(title = 72, subtitle = 118, caption = 135),
    title = "Considered leaving, and what happened next",
    subtitle = paste0(
      "Continuing students. Share no longer claiming the following year, by whether they said that year ",
      "they might have to leave their course."),
    caption = paste0(
      "Counted only where the student had course left and a full next year of data existed. {src_panel}")
  ),

  factors = list(
    wrap = c(title = 72, subtitle = 118, caption = 135),
    title = "What is associated with leaving",
    subtitle = paste0(
      "Adjusted odds of leaving before finishing for students with each characteristic versus those ",
      "without, holding course and cohort equal. 1.0 = no difference; bars are 95% confidence intervals."),
    caption = "Single-factor logistic models, course and cohort fixed effects. {src_panel}"
  ),

  survivorship = list(
    wrap = c(title = 72, subtitle = 118, caption = 135),
    title = "Survivorship and selection",
    subtitle = paste0(
      "Odds of leaving before finishing: all students, then only those who reach year 2, then adjusted ",
      "for financial confidence."),
    caption = "Logistic models, course and cohort fixed effects. {src_panel}"
  ),

  auc = list(
    wrap = c(title = 72, subtitle = 118, caption = 135),
    title = "How well the model predicts who leaves",
    subtitle = paste0(
      "Students sorted into ten equal groups by the model's predicted risk, against the overall average ",
      "leaving rate."),
    caption = "Logistic model of the five entry funding items. {src_panel}",
    box = "Model accuracy {auc}\n(a coin toss scores 0.50,\na perfect model 1.00)"
  ),

  # === C. FINDING ==========================================================

  rv_leave_curve = list(
    wrap = c(title = 52, subtitle = 95, caption = 118),
    title = "As real LSF at entry falls, predicted leaving rises",
    subtitle = paste0(
      "ARM 1 (main retention): one observation per student. Real LSF is measured at course entry only ",
      "(not year 2/3 while still enrolled). Outcome = left before finishing (once). ",
      "S0 = real LSF only; S1 = course + entry-year FE; S2 = + funding survey answers. ",
      "Ribbon = 95% CI for a typical student. Not individual prediction."),
    caption = "{src_comms}",
    y_lab = "Predicted probability of leaving before finishing",
    callout = paste0(
      "£1,000 lower real LSF\n(around the mean):\nabout {pp} pp on\npredicted probability\n({spec})")
  ),

  rv_hazard_leave_next = list(
    wrap = c(title = 52, subtitle = 95, caption = 118),
    title = "A lower real LSF is associated with a higher chance of leaving the following year",
    subtitle = paste0(
      "Hazard: among students still expected to have course left, predicted probability that this year ",
      "is their last LSF claim. One exit per spell (not double-counted across earlier years). ",
      "S1 = course and survey-year fixed effects. Ribbon = 95% CI for a typical profile."),
    caption = paste0(
      "Source: NHS LSF panel 2020-2026, DHSC analysis. Real LSF = {measure}. Associational, not causal. ",
      "At-risk sample uses course-length expected finish; exit = last claim year."),
    x_lab = "Real LSF value this year (£, {measure})",
    y_lab = "Predicted probability of leaving by next year",
    callout = "£1,000 lower real LSF\n(around the mean):\nabout {pp} pp on\nleave-next probability\n(S1)"
  ),

  rv_three_arms = list(
    wrap = c(title = 48, subtitle = 95, caption = 118),
    title = "We can use the 'real' value of the LSF to understand retention risks",
    subtitle = paste0(
      "Arm 1 does not use year-2/year-3 real LSF and does not double-count multi-wave students.  \n",
      "Arm 2 asks whether lower real LSF this year raises the chance of not returning next year.\n",
      "Arm 3 asks whether places where real LSF fell harder also saw fewer first-year claimants."),
    caption = paste0(
      "Source: NHS LSF panel 2020-2026. Real LSF construct: {measure} on nominal package (arms 1-2) or ",
      "core training grant (arm 3). Associational.")
  ),

  rv_recruit_effect = list(
    wrap = c(title = 56, subtitle = 100, caption = 128),
    title = "Recruitment: no reliable link once you compare like with like",
    subtitle = paste0(
      "Estimated change in the number of first-year LSF claimants if a place's real grant value were ",
      "£1,000 lower. The raw comparison (orange) looks large and negative, but it just reflects big ",
      "expensive cities vs small cheap towns. Comparing each provider with itself over time, the range ",
      "crosses zero, so there is no reliable effect."),
    caption = paste0(
      "Source: NHS LSF panel{cells}. Grey = 95% range includes zero (no reliable effect); orange = raw, ",
      "unadjusted (confounded by place)."),
    x_lab = "Change in first-year claimants per £1,000 lower real LSF  (dot = estimate, bar = 95% CI)"
  ),

  # === D. ROBUSTNESS =======================================================

  rv_spec_ladder = list(
    wrap = c(title = 0, subtitle = 0, caption = 130),
    title = "Does real LSF value still predict leaving once we control for survey answers?",
    subtitle = paste0(
      "Odds ratio per 1 SD of real grant value ({measure}). ",
      "S0 = real value only; S1 = + funding survey items; S2 = + grant components; ",
      "S3 = S1 + financial confidence (year-2 survivors)."),
    caption = paste0(
      "Source: NHS Learning Support Fund panel 2020-2026, DHSC analysis. Associational models; course ",
      "and entry-year fixed effects."),
    x_lab = "Odds ratio of leaving before finishing (1.0 = no association)"
  ),

  # === E. PLACEMENT ANNEX ==================================================

  placement_programme_lollipop = list(
    wrap = c(title = 56, subtitle = 100, caption = 128),
    title = "Placement hours are not associated in the main with leaving",
    subtitle = paste0(
      "Left (blue): mean placement hours per year by programme, DHSC FY26/27. ",
      "Right (orange): unadjusted percentage leaving before finishing. Programmes ",
      "ordered by placement hours. Bar length is scaled within each metric; the tip ",
      "prints the observed value. If leaving tracked hours the two sides would mirror."),
    caption = paste0(
      "{src_placement} Unadjusted, no controls. Programme cells below n=10 suppressed ",
      "(no percentage shown).")
  ),

  placement_interaction = list(
    wrap = c(title = 56, subtitle = 100, caption = 128),
    title = "A lower grant raises leaving on every course, not just long-placement ones",
    subtitle = paste0(
      "Marginal effect from a logistic model of leaving-before-finishing on ",
      "(real LSF value x placement hours), with course and entry-year fixed effects. ",
      "Points: change in odds per £1,000 lower real grant value, evaluated at a ",
      "short- and a long-placement course. Identified from within-course variation in ",
      "real value across areas and years."),
    caption = paste0(
      "{src_placement} The placement-hours main effect is absorbed by course fixed effects by design ",
      "and is not estimated here. Grey = 95% range includes zero."),
    x_lab = "Change in odds of leaving per £1,000 lower real LSF  (dot = estimate, bar = 95% CI)"
  ),

  placement_family_fe = list(
    wrap = c(title = 56, subtitle = 100, caption = 128),
    title = "The effect of more placement hours within a subject is unclear",
    subtitle = paste0(
      "Odds ratio for leaving-before-finishing per 100 additional placement hours ",
      "per year, logistic model with course-family (not course) fixed effects. ",
      "Identified from between-course, within-family variation in programme hours. ",
      "EXPLORATORY: subject differences within a family are uncontrolled, and no ",
      "demographic covariates exist on this branch."),
    caption = paste0(
      "{src_placement}{not_estimated} ! = crosswalk-ambiguous. No demographic controls are available ",
      "on this branch."),
    x_lab = "Odds ratio per 100 extra placement hours per year (1 = no difference)"
  ),

  # === F. EQUITY ANNEX =====================================================

  confidence_confident = list(
    wrap = c(title = 72, subtitle = 118, caption = 128),
    title = "High financial confidence is also uneven across groups",
    subtitle = paste0(
      "Share rating 4-5 on covering living expenses next year. ",
      "Dashed line = n-weighted average of groups on this slide ({ref}). ",
      "Colour: more than 1 SD from that average (teal = more confident). ",
      "Ethnicity omitted here for space; see the ethnicity low-confidence slide."),
    caption = "{src_d3}"
  ),

  confidence_unconfident = list(
    wrap = c(title = 72, subtitle = 118, caption = 128),
    title = "Financial confidence is uneven across claimant groups",
    subtitle = paste0(
      "Share rating 1-2 on covering living expenses next year. ",
      "Dashed line = n-weighted average of groups on this slide ({ref}). ",
      "Colour: more than 1 SD from that average (red = more worried, teal = less). ",
      "Ethnicity is on the next slide (too many categories to fit here)."),
    caption = "{src_d3}"
  ),

  triangle_scatter_risk_dependence = list(
    wrap = c(title = 72, subtitle = 116, caption = 128),
    title = "Retention risk vs funding-driven choice, across groups",
    subtitle = paste0(
      "Each point a demographic group (size = n). Spearman rho = {rho} across {n_groups} groups. ",
      "Ecological; wave-mismatched (see note)."),
    caption = "{src_d7}",
    x_lab = "Felt may leave course (%)",
    y_lab = "Funding important to WHERE to study (4-5, %)"
  ),

  triangle_rates = list(
    wrap = c(title = 72, subtitle = 118, caption = 128),
    title = "Who feels most at risk of leaving is uneven across claimant groups",
    subtitle = paste0(
      "Dashed line = n-weighted average across shown groups (~{ref}%). Colour: more than 1 SD from that ",
      "average (red = above, teal = below). Retention risk lever; equity/distributional, not causal."),
    caption = "{src_d6}",
    x_lab = "Share who felt they may leave their course (%)",
    lab_above = "More at-risk than average",
    lab_below = "Less at-risk than average"
  ),

  # === G. TECHNICAL ANNEX ==================================================

  tech_geography = list(
    wrap = c(title = 92, subtitle = 150, caption = 190),
    title = "A student's cost of living is measured where their provider is, not where they live",
    subtitle = "Four public sources, merged once, to give every provider a local rent for every year 2020 to 2026.",
    caption = paste0(
      "Built by scripts/90_build_reference.r. Sources: JISC Learning Providers Plus; postcodes.io; ",
      "ONS Output Area (2021) to TTWA (2011) to LAD (2022) lookup; ONS Price Index of Private Rents. ",
      "TTWA = travel-to-work area, the ONS geography of where people commute for work, used here as ",
      "the housing market a student can realistically rent in.")
  ),

  tech_realvalue = list(
    wrap = c(title = 92, subtitle = 150, caption = 190),
    title = "The grant is deflated by the basket a student actually buys, with housing counted once",
    subtitle = "Nominal award divided by a budget-weighted cost-of-living index, anchored to England in 2020.",
    caption = paste0(
      "Weight set in scripts/00_config.r (HOUSING_WEIGHT = {w}); index built in functions/real_value.r. ",
      "Rent from ONS PIPR, prices from ONS CPI series D7BT. All figures in 2020 money.")
  ),

  tech_model = list(
    wrap = c(title = 92, subtitle = 150, caption = 190),
    title = "One logistic regression per estimate, all comparisons within course and cohort",
    subtitle = paste0(
      "Fitted with fixest::feglm. Real value is the coefficient of interest; everything else is there ",
      "to rule out an alternative explanation."),
    caption = paste0(
      "Specification set in scripts/00_config.r (FE_STUDENT, SURVEY_VARS, COMP_VARS); fitted in ",
      "functions/analysis_real_value.r. Confidence intervals at 95%.")
  )
)

# ---- assembly --------------------------------------------------------------
# Read the secure overlay once per session and merge it over the defaults,
# entry by entry, so a JSON that supplies only a title keeps the repo's
# subtitle, caption and wrap widths.
deck_text <- local({
  TXT <- NULL
  function(refresh = FALSE) {
    if (!is.null(TXT) && !refresh) return(TXT)
    out <- deck_text_defaults()
    path <- tryCatch(file.path(derived_dir(), "slide_labels.json"),
                     error = function(e) NA_character_)
    if (!is.na(path) && file.exists(path) &&
        requireNamespace("jsonlite", quietly = TRUE)) {
      ov <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                     error = function(e) NULL)
      if (is.list(ov) && length(ov)) {
        unknown <- setdiff(names(ov), names(out))
        if (length(unknown))
          message("deck_text: slide_labels.json has ", length(unknown),
                  " key(s) matching no slide, ignored: ",
                  paste(unknown, collapse = ", "))
        for (k in intersect(names(ov), names(out)))
          out[[k]] <- utils::modifyList(out[[k]], as.list(ov[[k]]))
      }
    }
    TXT <<- out
    out
  }
})

# substitute {name} tokens: the shared fragments first, then the caller's values
deck_fill <- function(x, vals = list()) {
  if (!length(x) || is.na(x[1])) return(x)
  vals <- c(vals, deck_text_common())
  for (nm in names(vals))
    x <- gsub(paste0("{", nm, "}"), as.character(vals[[nm]]), x, fixed = TRUE)
  # a second pass, because the shared fragments carry tokens of their own
  for (nm in names(vals))
    x <- gsub(paste0("{", nm, "}"), as.character(vals[[nm]]), x, fixed = TRUE)
  x
}

# the unwrapped string
lbl_raw <- function(slide, field, ..., default = "") {
  v <- tryCatch(deck_text()[[slide]][[field]], error = function(e) NULL)
  if (is.null(v) || !length(v) || (length(v) == 1 && is.na(v))) v <- default
  deck_fill(v, list(...))
}

# the string as it goes on the slide: substituted, then wrapped at this
# slide's width. Width 0 (or an unlisted field) means do not wrap.
lbl <- function(slide, field, ..., default = "") {
  v <- lbl_raw(slide, field, ..., default = default)
  if (!nzchar(v)) return(v)
  w <- tryCatch(deck_text()[[slide]]$wrap[[field]], error = function(e) NULL)
  if (is.null(w) || is.na(w) || w <= 0) return(v)
  stringr::str_wrap(v, width = as.integer(w))
}
