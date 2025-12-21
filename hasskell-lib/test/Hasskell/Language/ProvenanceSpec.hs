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
      observedWorld <-
        sampleDeterministic (Seed 0 1) $
          genWorldWithToggleds [(onEntity, On), (offEntity, Off)]
      let lightOnSpec = lightAlwaysOn offEntity
      lightsSpec <-
        sampleDeterministic (Seed 0 1) $
          genSpecWithPolicy observedWorld lightOnSpec
      let (MkReconciliationPlan steps, _) = reconcile observedWorld lightsSpec
          (JustifyAction {stepReason}, _) = fromJust $ uncons steps

      renderedExplanation <- renderExplanation stepReason

      goldenStage $ pureGoldenTextFile "test_resources/trace_light_always_on.golden" renderedExplanation
