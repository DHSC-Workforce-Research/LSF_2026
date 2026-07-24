# Real LSF analysis: three arms

> **Script names changed in the 2026-07-24 RAP refactor.** The design below is
> unchanged and still correct; only the file layout moved. Everything that was
> spread across the numbered and lettered scripts now runs as
> `00_config -> 01_data -> 02_analysis -> 03_deck`, with the analysis functions
> in `functions/analysis_*.r` and the slides driven by `functions/deck_manifest.r`.
> The pre-refactor scripts are on branch `backup/dev-2026-07-22`.

Branch: `feat/real-lsf-controlled`  
Headline construct: **CPIH x local rent (TTWA)** on the student's nominal package  
(`training £5k + parental £2k + specialist £1k` when flagged).

All arms are **associational**. No unfunded control group.

---

## Arm 1 — Entry real LSF → leave before finish (MAIN retention)

| | |
|---|---|
| **Unit** | One row per student |
| **X** | Real LSF at **course entry** only (entry college + entry year) |
| **Y** | `left_before_finish` (last claim before expected finish) |
| **FE** | Course + entry year |
| **Scripts** | `06_findings_pack.r` (numbers), `07_real_value_comms.r` (leave curve / controls slides) |

**Counts:** multi-wave students are **not** double-counted as multiple leaves.  
**Ignores:** year-2 / year-3 real LSF while still enrolled.  
**Entry-year FE:** holds constant which cohort they *started* in (national time at entry).

---

## Arm 2 — This year's real LSF → gone next year (HAZARD)

| | |
|---|---|
| **Unit** | Student-year, among those still **at risk** |
| **X** | Real LSF in year *t* (that wave's place x year) |
| **Y** | `left_next` = last wave is *t* and more course was expected |
| **FE** | Course + survey year |
| **Scripts** | `06` (table), `08_hazard_and_recruitment.r` (slides + full write-up) |

**Counts:** one exit event when *t* is the last claim — not two leaves for Y1+Y2.  
**Uses** mid-course real LSF (answers "we only used entry for Arm 1").  
**Caveat:** disappearance from claiming, not full admin dropout.

---

## Arm 3 — Real LSF by place-year → first-year starter volumes (RECRUITMENT)

| | |
|---|---|
| **Unit** | Provider (college) x entry year (optional: region x year) |
| **X** | Place-year real LSF (core £5k training grant x CPIH x rent TTWA) |
| **Y** | Count (or log count) of **first-year** LSF claimants at that place-year |
| **FE** | Provider + year (preferred); region + year as secondary |
| **Scripts** | `08_hazard_and_recruitment.r` |

**Question:** where real LSF tanked more, did first-year claimant numbers fall more (relative to other places)?  

**Limits:** only people who already claimed LSF; no "never applied" group; capacity/COVID/policy confounders remain.  
**Need:** enough **within-provider** (or within-region) variation in real LSF after year FE — script reports this.

Drop thin 2020 by default.

---

## Run order (work machine)

```r
# after 01_read_tidy
source("scripts/06_findings_pack.r", encoding = "UTF-8")
source("scripts/07_real_value_comms.r", encoding = "UTF-8")
source("scripts/08_hazard_and_recruitment.r", encoding = "UTF-8")
```

Or Git Bash:

```bash
bash scripts/run_real_value_arms.sh
```
