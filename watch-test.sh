#!/usr/bin/env bash

ghcid \
  --command "stack ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test" \
  --warnings \
  --test "Test.Syd.sydTest Hasskell.${1}.spec" \
  --color=always
