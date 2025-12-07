{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.DiagnosticSpec (spec) where

import Hasskell.Language.Reconciler
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hedgehog
import Hedgehog.Internal.Gen qualified as InternalGen
import Hedgehog.Internal.Tree qualified as InternalTree
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reconciler diagnostic" $ do
    it "warns about unknown entity" $ stagedGolden $ \goldenStage -> do
      (unknownEntity, observed) <- sampleDeterministic (Seed 0 1) genWorldWithoutEntity
      let (_, report) = reconcile observed (lightAlwaysOn unknownEntity)
      renderedReport <- renderReport report
      goldenStage $ pureGoldenTextFile "test_resources/warn_unknown_entity.golden" renderedReport

sampleDeterministic :: Seed -> Gen a -> IO a
sampleDeterministic seed gen =
  let loop n =
        if n <= 0
          then
            expectationFailure "Hedgehog generator failed to produce a value"
          else do
            case InternalGen.evalGen 30 seed gen of
              Nothing ->
                loop (n - 1)
              Just x ->
                pure $ InternalTree.treeValue x
   in loop (100 :: Int)
