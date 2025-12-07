#!/usr/bin/env bash
set -euo pipefail

RESET="Prelude.False"
SPEC=""

for arg in "$@"; do
  case "$arg" in
    --reset)
      RESET="Prelude.True"
      ;;
    *)
      if [[ -z "$SPEC" ]]; then
        SPEC="$arg"
      fi
      ;;
  esac
done

: "${SPEC:?missing spec name}"

EXPR="Test.Syd.sydTestWith \
  (Test.Syd.OptParse.defaultSettings \
    { Test.Syd.OptParse.settingGoldenStart = Prelude.True \
    , Test.Syd.OptParse.settingGoldenReset = ${RESET} \
    }) \
  ${SPEC}.spec"

stack --work-dir .watch-stack-work \
  ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test \
  --ghci-options="-e \"${EXPR}\""

