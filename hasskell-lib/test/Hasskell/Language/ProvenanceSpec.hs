{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.ProvenanceSpec (spec) where

import Hasskell
import Hasskell.HomeAssistant.API
import Hasskell.Language.Reconciler
import Hasskell.Language.Report
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd hiding (shouldBe)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reason trace" $ do
    it "includes if expression and boolean derivation" $ stagedGolden $ \goldenStage -> do
      let lightA = EntityId "lightA"
          lightB = EntityId "lightB"
      observedOn <- sample $ genWorldWithToggleds [(lightA, Off), (lightB, Off)]
      let boolPolicy =
            policy
              "make lightB the inverse of lightA"
              ( if_ (toggledStateOf lightA `is` on)
                  `then_` (lightB `shouldBe` off)
                  `else_` (lightB `shouldBe` on)
              )
      let (plan, _) = reconcile observedOn boolPolicy
      renderedPlan <- renderPlanTrace unadornedStyle plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_if_and_boolean_derivation.golden" renderedPlan

    it "includes shouldBe" $ stagedGolden $ \goldenStage -> do
      let onEntity = EntityId "lightA"
          offEntity = EntityId "lightB"
      observedWorld <- sample $ genWorldWithToggleds [(onEntity, On), (offEntity, Off)]
      let lightOnSpec = lightAlwaysOn offEntity
      lightsSpec <- sample $ genSpecWithPolicy observedWorld lightOnSpec
      let (plan, _) = reconcile observedWorld lightsSpec

      renderedPlan <- renderPlanTrace unadornedStyle plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_shouldBe.golden" renderedPlan

    it "renders time references correctly" $ stagedGolden $ \goldenStage -> do
      (entity, observed) <- sample $ genWorldWithToggledAndTime Off (14, 39)
      let timePolicy =
            policy
              "turn light on at 14:39"
              ( if_ (currentTime `is` time @14 @39)
                  `then_` (entity `shouldBe` on)
              )
      let (plan, _) = reconcile observed timePolicy

      renderedPlan <- renderPlanTrace unadornedStyle plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_time.golden" renderedPlan

sample :: Gen a -> IO a
sample = sampleDeterministic (Seed 0 1)
