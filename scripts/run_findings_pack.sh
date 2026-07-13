#!/usr/bin/env bash
# Run from LSF_2026 repo root in Git Bash.
# Produces aggregate-only findings_numbers.txt (safe to paste/email).
set -euo pipefail

cd "$(dirname "$0")/.."
echo "== repo: $(pwd) =="
echo "== branch before =="
git rev-parse --abbrev-ref HEAD
git status -sb

echo "== pull feat/real-lsf-controlled =="
git fetch origin
git checkout feat/real-lsf-controlled
git pull --rebase origin feat/real-lsf-controlled || git pull origin feat/real-lsf-controlled

echo "== run 06 (UTF-8 source) =="
# Windows R: prefer Rscript on PATH
if command -v Rscript.exe >/dev/null 2>&1; then
  RS=Rscript.exe
elif command -v Rscript >/dev/null 2>&1; then
  RS=Rscript
else
  echo "ERROR: Rscript not found on PATH. Open RStudio and run: source('scripts/06_findings_pack.r', encoding='UTF-8')"
  exit 1
fi

"$RS" -e "source('scripts/06_findings_pack.r', encoding='UTF-8')"

# Find newest findings pack
OUT_BASE="${LSF_OUTPUT_DIR:-}"
if [[ -z "$OUT_BASE" ]]; then
  # default NW025 path (Windows via Git Bash)
  UP="${USERPROFILE:-$HOME}"
  OUT_BASE="$UP/Department of Health and Social Care/NW025 - Research/1. Projects/Learning Support Fund Evaluation/LSF Review 2026/outputs"
fi

PACK=$(ls -dt "$OUT_BASE"/findings_pack_* 2>/dev/null | head -1 || true)
if [[ -z "$PACK" ]]; then
  echo "ERROR: no findings_pack_* folder under: $OUT_BASE"
  exit 1
fi

echo "== pack: $PACK =="
# Copy plain text to repo root Desktop-friendly location
DEST="$PACK/findings_numbers.txt"
cp -f "$DEST" "./findings_numbers.txt" 2>/dev/null || true
cp -f "$DEST" "$USERPROFILE/Desktop/findings_numbers.txt" 2>/dev/null || true
cp -f "$DEST" "$HOME/Desktop/findings_numbers.txt" 2>/dev/null || true

echo ""
echo "DONE. Paste the file contents back to Mithril / email yourself:"
echo "  $DEST"
echo "Also copied to repo root: ./findings_numbers.txt (if writeable)"
echo ""
echo "----- BEGIN findings_numbers.txt -----"
# force strip CR and show
sed 's/\r$//' "$DEST"
echo "----- END findings_numbers.txt -----"
