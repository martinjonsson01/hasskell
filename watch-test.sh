#!/usr/bin/env bash

set -euo pipefail

SPEC=${1}

EXPR="Test.Syd.sydTestWith \
  (Test.Syd.OptParse.defaultSettings \
    { Test.Syd.OptParse.settingGoldenStart = Prelude.True \
    }) \
  ${SPEC}.spec"

STACK="stack --work-dir .watch-stack-work \
  ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test"

ghcid \
  --command "${STACK}" \
  --warnings \
  --test "${EXPR}" \
  --color=always
