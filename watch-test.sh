#!/usr/bin/env bash

set -euo pipefail

ghcid \
  --command "stack --work-dir .watch-stack-work ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test" \
  --warnings \
  --test "Test.Syd.sydTest ${1}.spec" \
  --color=always
