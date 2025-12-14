stack build \
  --fast \
  --ghc-options "-j4 +RTS -A128m -n2m -RTS" \
  --work-dir .manual-stack-work \
  --file-watch
