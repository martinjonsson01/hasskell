#!/usr/bin/env bash
set -euo pipefail

SPEC=$1
SEED_ARG=${2:-}

if [[ -n "${SEED_ARG}" ]]; then
  # user passed --seed=n
  # strip flag and read n
  n="${SEED_ARG#--seed=}"
  SEED_EXPR="(Test.Syd.Run.FixedSeed ${n})"
else
  # default
  SEED_EXPR="Test.Syd.Run.RandomSeed"
fi

EXPR="Test.Syd.sydTestWith \
  (Test.Syd.OptParse.defaultSettings \
    { Test.Syd.OptParse.settingGoldenStart = Prelude.True \
    , Test.Syd.OptParse.settingSeed = ${SEED_EXPR} \
    }) \
  ${SPEC}.spec"

STACK="stack --work-dir .watch-stack-work \
  ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test"

ghcid \
  --command "${STACK}" \
  --test "${EXPR}" \
  --color=always
