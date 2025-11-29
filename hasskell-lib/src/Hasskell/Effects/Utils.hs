{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.Utils
  ( -- Utils
    runMapError,
  )
where

import Effectful
import Effectful.Error.Static

runMapError ::
  (Error e2 :> es, Show e2) =>
  (e1 -> e2) ->
  Eff (Error e1 : es) a ->
  Eff es a
runMapError errConstructor action =
  runErrorNoCallStack action
    >>= either (throwError . errConstructor) pure
