# Placement hours: two arms and a descriptive

> **Script names changed in the 2026-07-24 RAP refactor.** The design below is
> unchanged and still correct; only the file layout moved. Everything that was
> spread across the numbered and lettered scripts now runs as
> `00_config -> 01_data -> 02_analysis -> 03_deck`, with the analysis functions
> in `functions/analysis_*.r` and the slides driven by `functions/deck_manifest.r`.
> The pre-refactor scripts are on branch `backup/dev-2026-07-22`.

Branch: `feat/placement-hours`, cut from `feat/real-lsf-controlled`
Hours source: **DHSC programme-level average placement hours per year, FY26/27** (excluding adjustments), 24 programme codes, 23 carrying a value.

All arms are **associational**. No unfunded control group, no randomisation, one time-invariant hours snapshot.

---

## Read these two things first

**1. Course fixed effects absorb placement hours completely.**
Hours are a programme-level constant. Every model in this repo uses `course + entry_year` FE (`functions/estimate_utils.r`). A course dummy and an hours term are perfectly collinear, so `fixest` drops one. An hours main effect and course FE cannot both exist. Arm P1 concedes the main effect and estimates the interaction; Arm P2 buys the main effect by dropping course FE and pays for it in confounding. There is no third option that gets both.

**2. The high-hours band is close to a nursing dummy.**
Of the 23 programmes with a value, everything above 660 hours is a nursing field, midwifery, ODP, or Dental Therapist. Nursing spans 694 (Children's) to 829 (Dual Professional). "High placement hours" is very nearly a synonym for "is a nurse". Any unadjusted comparison across bands is nurses versus everyone else wearing a different label. `scripts/p1_descriptives.r` publishes band composition in the same file as the band outcomes so nobody can quote one without the other, and prints a warning if one family holds 75% or more of the high band.

---

## Where the hours come from

`reference/placement_hours.csv` — 24 rows: `programme_code, programme_name, hours_per_year, course_family`.

- Sonography (Direct Entry, 2040) has **no value** and is stored empty, not zero. A zero would enter every mean silently.
- Undergraduate Clinical Pharmacy (1229) arrived as `143 ` with a trailing space; stripped at authoring time.
- Families: `nursing` (1212, 1213, 1216, 1217, 1218, 1219, 1231), `dental` (1070, 1071, 1230), `ahp` (1214, 1215, 1220–1228, 2040), `other` (1229, 1232). Midwifery sits in `nursing` on regulator and course-structure grounds.

`reference/course_crosswalk.csv` — free-text `course` to programme code, **hand-authored**. `course` in the survey is read verbatim (`01_read_tidy.r` stage D) and there is no canonical course list in this repo, so `scripts/p0_audit_course_strings.r` prints what students actually typed before the mapping is written. Matching is exact on a squished, case-folded key. No fuzzy matching: the three dental categories differ three-fold in hours (231 / 375 / 747) and a plausible wrong match is worse than an honest `NA`.

The crosswalk shipped on this branch is **provisional**. Rows marked `provisional` are educated guesses at likely free-text variants; they must be checked against the p0 audit on the work machine. Rows marked `AMBIGUOUS` are deliberately left blank.

---

## Variables (`functions/placement_hours.r`)

`attach_placement_hours(sample)` returns the sample with the same row count and:

| Variable | Meaning |
|---|---|
| `hours_per_year` | Raw. `NA` where unmatched or where DHSC supplied no figure. |
| `hours_per100` | `hours_per_year / 100`. The modelling unit, so effects read as "per 100 extra hours" alongside the existing "per £1,000". |
| `course_family` | `nursing` / `dental` / `ahp` / `other`. |
| `hours_band` | `low` / `medium` / `high`, cut on **student-weighted** tertiles. Cutting on the 23 programmes would put ~8 programmes per band regardless of how many students sit behind them, and nursing dominates the sample. Cut points are reported. |
| `hours_demeaned` | `hours_per100` minus its family mean. The Arm P2 working variable. |

Students with no hours value are `NA` on the band, never `low` — a missing value is not a short placement. `placement_match_report()` prints matched / unmatched counts, the match rate, band cut points, band and family counts, and the top unmatched strings. It runs before any estimate.

---

## P0 — Descriptive: outcomes by hours band

| | |
|---|---|
| **Unit** | One row per student |
| **X** | `hours_band` (student-weighted tertiles) |
| **Y** | `left_before_finish`, `considered_leaving`, `fund_availability`, `grant_influence`, `grant_helps_stay`, `confidence` |
| **FE** | None. Raw contrast. |
| **Script** | `scripts/p1_descriptives.r` |

**Identified off:** nothing. This is an unadjusted comparison.
**Published with:** band composition by family and by top programme, in the same folder, written first.
**Gate:** stops if fewer than 50% of students carry an hours value.
**Suppression:** cells below n=10 are marked `suppressed`, never blanked. A gap reads as a zero and a zero is a claim.

---

## Arm P1 — Real LSF × placement hours (HEADLINE)

| | |
|---|---|
| **Unit** | One row per student; hazard variant on student-years |
| **X** | `rv_gbp * hours_per100`, where `rv_gbp` is `real_value_rent_ttwa_cpih` |
| **Y** | `left_before_finish`, `left_2y_plus_early`, `one_wave_only`, `considered_leaving`; hazard variant on `left_next` |
| **FE** | `course + entry_year`; hazard variant `course + year` |
| **Script** | `scripts/p2_hours_interaction.r` |

**Question:** does thin real LSF predict leaving *more strongly* where placements are longer? The mechanism is that heavy placement hours crowd out paid part-time work, so the same real grant has to stretch further.

**Identified off:** within-course variation in real value across places and years — the same variation `05_real_value_controlled.r` already uses.

**Hours main effect:** absorbed by course FE, by design. The script confirms its absence from the coefficient table and states it. It is **not** a null result and must never be reported as one.

**Reported as:** the £1,000 effect at a short-placement course and at a long-placement course separately, in the register `pound_effect()` already produces. The bare interaction coefficient is not a finding anyone can read. Standard errors on those quantities come from the model vcov (linear combination `b_rv + h·b_int`), not from the interaction term alone.

**Gate:** the script prints mean within-course SD of real value by band before estimating anything. If real value barely moves within course on high-hours courses, the interaction has no variation to work with and a tight interval around it is false precision. Bands below £150 mean within-course SD, or below 200 students, are marked THIN and the arm does not support a claim about them.

---

## Arm P2 — Hours under course-family FE (EXPLORATORY)

| | |
|---|---|
| **Unit** | One row per student |
| **X** | `hours_per100` (+ bursary component flags) |
| **Y** | `left_before_finish`, `left_2y_plus_early`, `considered_leaving`, `grant_helps_stay` |
| **FE** | `course_family + entry_year`; per-family models use `entry_year` alone |
| **Script** | `scripts/p3_hours_family_fe.r` |

**Identified off:** between-course, within-family variation. Nursing 694–829 (a genuine but narrow 135-hour range over a large sample); dental 231–747 (three-fold, but the smallest cells and the worst crosswalk risk).

**Confounding:** not controlled, by construction. This compares a midwife to an adult nurse. Placement hours are not the only thing that differs between them. That sentence is on the face of the chart, not in the appendix.

**Controls:** the analysis sample carries no demographics. `lsf_analysis_sample.rds` has the bursary component flags (`parental`, `specialist`, `regional`) and nothing else usable. Age, ethnicity and disability live on `feat/demographics-slides` and are deliberately **not** joined — a cross-branch join would make this arm unreproducible from this branch alone. Every coefficient is demographically unadjusted and the output says so.

**Not estimated when:** a family has fewer than 2 programmes, an hours range under 50, or fewer than 200 students. Those families print their verdict instead of a coefficient. Dental output carries a crosswalk-ambiguity flag.

**Check:** raw hours and family-demeaned hours must give identical coefficients under family FE. The script asserts this and reports MISMATCH if the demeaning is wrong.

---

## Slides

`scripts/p4_placement_slides.r` reads the saved CSV tables and refits nothing.

1. `slide_placement_band_composition.png` — composition bar with the leaving rate printed above it. One panel, so the number cannot travel without the composition underneath it.
2. `slide_placement_interaction.png` — Arm P1, the £1,000 effect at short- vs long-placement courses.
3. `slide_placement_family_fe.png` — Arm P2 per-family coefficients; families with no identifying variation appear in the caption as not-estimated rather than being dropped.

Slide text can be overridden from `placement_slide_labels.json` in the pack folder (not committed; data-leak precaution already in force on `main`).

---

## Caveats that belong on the slide, not in a footnote

- **Snapshot applied retrospectively.** FY26/27 hours are used for 2020 entrants. Placement requirements are regulator-set and move slowly, so this is defensible. It is still an assumption.
- **Crosswalk risk is the project risk.** The match rate is reported before any estimate. Below the threshold, the honest output is the match rate, not a model.
- **Dental** has the widest hours contrast, the smallest n, and the worst matching. It runs, flagged, or not at all.
- **No causal claim** is available from any of this.

---

## Out of scope

Total placement burden (`hours_per_year × course_length`), placement hours crossed with the geographic real-value work, hours as a moderator in the recruitment arm. All plausible, all compounding two estimates on an already thin cell structure. Not in the first pass.

---

## Run order (work machine)

```r
# after 01_read_tidy
source("scripts/p0_audit_course_strings.r", encoding = "UTF-8")  # then complete the crosswalk BY HAND
source("scripts/p1_descriptives.r",         encoding = "UTF-8")
source("scripts/p2_hours_interaction.r",    encoding = "UTF-8")
source("scripts/p3_hours_family_fe.r",      encoding = "UTF-8")
source("scripts/p4_placement_slides.r",     encoding = "UTF-8")
```

See `RUNME_PLACEMENT_HOURS.txt`.
