#!/usr/bin/env bash
# Run real-LSF arms 1-3 numbers + slides (repo root, Git Bash / work machine).
set -euo pipefail
cd "$(dirname "$0")/.."
echo "== $(pwd) =="
git rev-parse --abbrev-ref HEAD

if command -v Rscript.exe >/dev/null 2>&1; then RS=Rscript.exe
elif command -v Rscript >/dev/null 2>&1; then RS=Rscript
else
  echo "Rscript not on PATH — run in RStudio:"
  echo '  source("scripts/06_findings_pack.r", encoding="UTF-8")'
  echo '  source("scripts/07_real_value_comms.r", encoding="UTF-8")'
  echo '  source("scripts/08_hazard_and_recruitment.r", encoding="UTF-8")'
  exit 1
fi

echo "== 06 findings pack =="
"$RS" -e "source('scripts/06_findings_pack.r', encoding='UTF-8')"
echo "== 07 entry leave + survey outcome curves =="
"$RS" -e "source('scripts/07_real_value_comms.r', encoding='UTF-8')"
echo "== 08 hazard + recruitment =="
"$RS" -e "source('scripts/08_hazard_and_recruitment.r', encoding='UTF-8')"
echo "DONE. Check outputs_dir()/real_value_comms and real_value_hazard_recruit"
