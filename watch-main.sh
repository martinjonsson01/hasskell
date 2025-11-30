#!/usr/bin/env bash

ghcid \
  --command "stack ghci hasskell-lib:lib hasskell-lib:test:hasskell-lib-test" \
  --warnings \
  --test "Test.Syd.sydTest Hasskell.HomeAssistant.ClientSpec.spec" \
  --color=always
