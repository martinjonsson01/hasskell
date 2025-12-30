#!/usr/bin/env bash
set -euo pipefail

IGNORE_SPECS=(
  # "Hasskell.Language.DiagnosticSpec"
  # "Hasskell.Language.ProvenanceSpec"
)

SEED_ARG=""
SPEC_NAMES=()
ALLOW_WARNINGS=false

# parse args
for arg in "$@"; do
  case "$arg" in
  --seed=*)
    SEED_ARG="$arg"
    ;;
  --spec=*)
    SPEC_NAMES+=("${arg#--spec=}")
    ;;
  --warnings)
    ALLOW_WARNINGS=true
    ;;
  *)
    echo "unknown arg: $arg" >&2
    exit 1
    ;;
  esac
done

# seed handling
if [[ -n "${SEED_ARG}" ]]; then
  n="${SEED_ARG#--seed=}"
  SEED_EXPR="(Test.Syd.Run.FixedSeed ${n})"
else
  SEED_EXPR="Test.Syd.Run.RandomSeed"
fi

# auto-discover specs if none provided
if [[ ${#SPEC_NAMES[@]} -eq 0 ]]; then
  mapfile -t SPEC_NAMES < <(
    fd 'Spec\.hs$' |
      grep '/test/' |
      grep -v '/Spec\.hs$' |
      sed -E 's|^.*?/test/||; s|\.hs$||; s|/|.|g'
  )
fi

# apply ignore list
for ignore in "${IGNORE_SPECS[@]}"; do
  SPEC_NAMES=("${SPEC_NAMES[@]/$ignore/}")
done

# drop empty entries
tmp=()
for s in "${SPEC_NAMES[@]}"; do
  if [[ -n "$s" ]]; then
    tmp+=("$s")
  fi
done
SPEC_NAMES=("${tmp[@]}")

if [[ ${#SPEC_NAMES[@]} -eq 0 ]]; then
  echo "no specs to run after filtering ignore list" >&2
  exit 1
fi

# build expression:
# Test.Syd.sydTestWith settings (spec1.spec >> spec2.spec >> ...)
# NOTE: sydTestWith expects a single combined TestDef; (>>) sequences them.
SPEC_EXPR=""
for s in "${SPEC_NAMES[@]}"; do
  if [[ -z "$SPEC_EXPR" ]]; then
    SPEC_EXPR="${s}.spec"
  else
    SPEC_EXPR="${SPEC_EXPR} Prelude.>> ${s}.spec"
  fi
done

echo "${SPEC_EXPR}"

EXPR="Test.Syd.sydTestWith \
  (Test.Syd.OptParse.defaultSettings \
    { Test.Syd.OptParse.settingGoldenStart    = Prelude.True \
    , Test.Syd.OptParse.settingSeed           = ${SEED_EXPR} \
    , Test.Syd.OptParse.settingSkipPassed     = Prelude.True \
    , Test.Syd.OptParse.settingRetries        = 0 \
    , Test.Syd.OptParse.settingReportProgress = Test.Syd.OptParse.ReportProgress \
    }) \
  (${SPEC_EXPR})"

STACK="stack --work-dir .watch-stack-work \
  ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test"

WARNINGS=""
if [ "${ALLOW_WARNINGS}" = true ]; then
  WARNINGS="--warnings"
fi

ghcid \
  --command "${STACK}" \
  --test "${EXPR}" \
  ${WARNINGS} \
  --color=always
