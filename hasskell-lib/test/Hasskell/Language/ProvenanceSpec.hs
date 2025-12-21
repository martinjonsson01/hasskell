{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.ProvenanceSpec (spec) where

import Data.List (uncons)
import Data.Maybe (fromJust)
import Hasskell.HomeAssistant.API
import Hasskell.Language.Provenance
import Hasskell.Language.Reconciler
import Hasskell.Language.World
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reason trace" $ do
    it "shows derivation chain leading to action" $ stagedGolden $ \goldenStage -> do
      let onEntity = EntityId "lightA"
          offEntity = EntityId "lightB"
      observed <-
        sampleDeterministic (Seed 0 1) $
          genWorldWithToggleds [(onEntity, On), (offEntity, Off)]
      let lightOnSpec = lightAlwaysOn offEntity
      lightsSpec <-
        sampleDeterministic (Seed 0 1) $
          genSpecWithPolicy observed lightOnSpec
      let (MkReconciliationPlan steps, _) = reconcile observed lightsSpec
          (step, _) = fromJust $ uncons steps

      renderedStep <- renderStep step

      goldenStage $ pureGoldenTextFile "test_resources/trace_light_always_on.golden" renderedStep
