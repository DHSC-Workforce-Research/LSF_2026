# LSF_2026

Analysis of the NHS Learning Support Fund claimant panel, 2020 to 2026.

Code only. No data lives in this repo, and nothing individual-level is ever
written to it. Individual records sit in the secure share and aggregate outputs
go to the NW025 project area; both paths are built in `functions/paths.r` from
the Windows user profile, so nothing is hard-coded to one machine.

## Slide text

Every word on every slide is in `functions/deck_text.r`, keyed by the manifest
slug: title, subtitle, caption, axis labels, callout boxes. The builders in
`deck_builders*.r` hold geometry and data handling and reach for text through
`lbl(slug, field, ...)`, which substitutes `{token}` values and wraps at that
slide's width. Wrap widths differ by slide, on purpose, and are recorded next
to the text they wrap.

Titles state findings, so the findings-bearing versions are not in this repo.
`_derived/slide_labels.json` on the secure machine overlays `deck_text.r` key by
key; supply only the fields to change. See `reference/slide_labels.example.json`.
A missing overlay is normal: the deck builds with the descriptive titles.

`tests/check_deck_text.r` fails if a manifest slide has no text, a builder is
missing, a `{token}` never gets substituted, or a literal title creeps back into
a builder.

This replaced three mechanisms: JSON for six slides, hardcoded strings for
eleven, and a `placement_slide_labels.json` that was found by a recursive search
of the outputs tree and silently overrode the placement titles.

## Run order

```r
# from the repo root, in this order
source("scripts/01_data.r")       # raw workbooks   -> analysis tables
source("scripts/02_analysis.r")   # analysis tables -> tidy result CSVs
source("scripts/03_deck.r")       # result CSVs     -> slide_NN_slug.png
source("tests/check_numbers.r")   # prove the numbers did not move
```

`00_config.r` is sourced by each of the three; you never run it directly.
`90_build_reference.r` is separate and rarely run: it rebuilds the committed
`reference/*.csv` from ONS, Land Registry and CPI sources, so run it only when
that external data is refreshed.

```
                 reference/*.csv        <- 90_build_reference.r (rarely)
                        |
 secure share ---> 01_data.r ---> derived tables (secure)
                                        |
                                   02_analysis.r ---> outputs/tables/tbl_*.csv
                                                          |
                                                     03_deck.r ---> outputs/deck/slide_NN_slug.png
```

## The three layers, and why they are separate

**`01_data.r`** builds analysis tables and nothing else. No model is fitted
here and no finding is stated.

**`02_analysis.r`** fits every model in the project and persists every result
as a tidy CSV. If a number appears on a slide, it was computed here and written
to disk first. The analysis functions live in `functions/analysis_*.r`.

**`03_deck.r`** draws. It reads only the CSVs the manifest names and fits
nothing, which is checkable: grep for `glm(`, `feglm(` or `lm(` over
`03_deck.r` and `functions/deck_*.r` returns only `geom_smooth(method = "lm")`,
a visual trend line.

The split is what makes the deck cheap to change. Reordering slides, cutting
one, or restyling never touches a model.

## The deck manifest

`functions/deck_manifest.r` is the single source of truth for the presented
deck: one row per slide, giving its section, number, slug, title, builder
function and source tables. `03_deck.r` walks that table and saves each slide
as `slide_NN_slug.png`, where `NN` comes from the manifest row and nowhere
else. No plotting code knows or can override its own number.

To reorder the deck, edit the manifest and re-run `03_deck.r`. To drop a slide,
delete its row. To add one, add a row and write its `build_slide_*()` function
in `functions/deck_builders.r`. A row whose builder is missing, or whose source
table has not been produced, is reported and skipped, and the run prints what
was left out, so a partial deck can never look complete.

Some tables are written in the row order their slide plots them, because factor
levels do not survive a CSV. Where that matters the builder rebuilds the factor
from row order, and the analysis function's header says so.

## Verifying that the numbers have not moved

`tests/check_numbers.r` is the tripwire for any change to this repo. It locates
the result tables, recomputes 33 probes and compares them to
`tests/expected_values.csv`, asserting counts exactly and statistics to three
decimal places. Run it after the chain:

```r
source("tests/check_numbers.r")   # check mode is the default
```

`tests/README.md` covers capture mode, which is how the baseline was frozen.

Every run also lints for non-ASCII bytes. **This matters more than it sounds:**
Windows `source()` silently truncates a file at the first exotic character, so
a stray em dash in a comment can drop the second half of a script with no error
at all. That cost a day in July 2026. The only non-ASCII permitted in a `.r`
file is the pound sign. Set `LSF_HARNESS_STRICT_ASCII=1` to make a violation
fatal.

## Layout

```
scripts/     00_config, 01_data, 02_analysis, 03_deck, 90_build_reference
functions/   analysis_*.r   one per analysis stage, called by 02
             deck_*.r       manifest, builders and helpers, called by 03
             data_build.r   the parse and join steps, called by 01
             everything else: shared utilities (paths, theme, estimation)
reference/   committed inputs from ONS / Land Registry / CPI, plus the course
             crosswalk. Rebuilt only by 90_build_reference.r.
tests/       the numbers and ASCII harness
docs/        plans
```

Everything the pipeline writes goes into a named subfolder of the outputs area,
so its root holds folders, not loose files:

```
outputs/
  deck/                        slide_NN_slug.png, plus 00_running_order.txt
  tables/                      the standalone analysis tables (tbl_*.csv)
  real_value_comms/            Arm 1 predicted-probability curve tables
  real_value_hazard_recruit/   Arm 2 / Arm 3 tables
  findings_pack/               emailable numbers pack, overwritten each run
  placement_hours/             placement-hours pack, overwritten each run
```

The paths come from `functions/paths.r` (`tables_dir()`, `findings_pack_dir()`,
`placement_pack_dir()`, `deck_dir()`); nothing writes to the outputs root. Both
the harness and the deck locate tables by recursive search, so which subfolder
a table sits in does not matter to any reader. The two packs use fixed names and
are overwritten each run, so they do not accumulate one dated copy per run.

`functions/codebook.r` is documentation rather than pipeline code: it holds the
verbatim survey question wording behind every variable and the raw-to-analysis
name mapping. Nothing calls it, and it is kept deliberately, because that
mapping is not recoverable from anywhere else.

`REAL_VALUE_THREE_ARMS.md` and `PLACEMENT_HOURS_ARMS.md` document the design of
the analysis arms: units, identification and limits. Read those before changing
a model.

## History

The pre-refactor layout, roughly 27 scripts with analysis and plotting
interleaved, is preserved on branch `backup/dev-2026-07-22`. Anything pruned
from this branch is there, including the exploratory slides that are not in the
presented deck.
