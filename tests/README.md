# tests/ - the refactor tripwire

`check_numbers.r` proves the RAP refactor did not change a single result. The
refactor moves code verbatim; this harness reproduces the frozen acceptance
numbers from the analysis outputs and fails loudly on any drift.

It uses base R only (no extra packages), locates output tables by recursive
filename search under `outputs_dir()` / `derived_dir()` (newest file wins, so it
does not care which sub-folder a table lives in), and honours `LSF_OUTPUT_DIR`.

## Checkpoint 1 (run once, against the CURRENT outputs, before anything moves)

From the repo root on the work machine, after the analysis chain has written its
outputs:

```r
# 1. freeze the baseline: fills every blank expected value from current outputs
#    and verifies the hard-coded frozen numbers match.
Sys.setenv(LSF_HARNESS_MODE = "capture")
source("tests/check_numbers.r")

# 2. confirm it is green
Sys.setenv(LSF_HARNESS_MODE = "check")
source("tests/check_numbers.r")
```

Capture should report `all hard-coded frozen numbers match current outputs`. If
it reports a `FROZEN MISMATCH`, a probe points at the wrong cell (or a number
moved) - stop and tell me before anything is refactored. Commit the filled
`expected_values.csv` so later checkpoints compare against a real baseline.

Also at this checkpoint: edit `functions/deck_manifest.r` to the deck cut you
actually present (it ships as the plan's default proposal).

## Later checkpoints (after each move)

```r
source("tests/check_numbers.r")   # MODE defaults to check
```

Green means the moved code reproduces the baseline to 3 dp on statistics and
exactly on counts. Red stops the line.

## What is checked

- Panel size (students, student-years), Arm 1 spec ladder (S1/S3 OR per SD,
  per-GBP1000 rows), Arm 2 hazard (OR, n, CI, p), Arm 3 recruitment FE
  (provider/region/no-FE pct change, p, cells), erosion (recomputed from
  `reference/cpi_index.csv`, no secure data needed). 32 probes; the 13 with a
  value in `expected_values.csv` are the plan's frozen contract, the rest are
  captured at baseline for tighter pinning.

## ASCII lint

Every run scans `.r` files for non-ASCII bytes (the UTF-8 pound sign excepted;
Windows `source()` truncates at the first exotic byte). At baseline it is
report-only: a few files legitimately carry non-ASCII that gets converted to
`\u` escapes when they move. Set `LSF_HARNESS_STRICT_ASCII=1` to make a
violation fatal - that is the gate for the post-refactor checkpoint
(success criterion 5), once the files are clean.
