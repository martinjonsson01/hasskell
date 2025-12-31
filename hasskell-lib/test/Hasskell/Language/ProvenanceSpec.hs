{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.ProvenanceSpec (spec) where

import Hasskell
import Hasskell.Language.Reconciler
import Hasskell.Language.Report
import Hasskell.Language.Verifier
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Test.Syd hiding (shouldBe)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reason trace" $ do
    it "includes if expression and boolean derivation" $ stagedGolden $ \goldenStage -> do
      let lightA = light "lightA"
          lightB = light "lightB"
      observedOn <- sample $ genWorldWithToggleds [(lightA, Off), (lightB, Off)]
      let boolPolicy =
            policy
              "make lightB the inverse of lightA"
              ( if_ (toggledStateOf lightA `is` on)
                  `then_` (lightB `shouldBe` off)
                  `else_` (lightB `shouldBe` on)
              )
      let (verifiedBoolPolicy, _) = verify observedOn boolPolicy
          plan = reconcile observedOn verifiedBoolPolicy
      renderedPlan <- renderPlanTrace Plain plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_if_and_boolean_derivation.golden" renderedPlan

    it "includes shouldBe" $ stagedGolden $ \goldenStage -> do
      let onEntity = light "lightA"
          offEntity = light "lightB"
      observedWorld <- sample $ genWorldWithToggleds [(onEntity, On), (offEntity, Off)]
      let lightOnSpec = lightAlwaysOn offEntity
      lightsSpec <- sample $ genSpecWithPolicy observedWorld lightOnSpec
      let (verifiedLightsSpec, _) = verify observedWorld lightsSpec
          plan = reconcile observedWorld verifiedLightsSpec

      renderedPlan <- renderPlanTrace Plain plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_shouldBe.golden" renderedPlan

    it "renders time references correctly" $ stagedGolden $ \goldenStage -> do
      (SomeToggleable entity, _, observed) <- sample $ genWorldWithToggledAndTime Off (14, 39)
      let timePolicy =
            policy
              "turn light on at 14:39"
              ( if_ (currentTime `is` time @14 @39)
                  `then_` (entity `shouldBe` on)
              )
      let (verifiedTimePolicy, _) = verify observed timePolicy
          plan = reconcile observed verifiedTimePolicy

      renderedPlan <- renderPlanTrace Plain plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_time.golden" renderedPlan

    it "renders successful time comparison correctly" $ stagedGolden $ \goldenStage -> do
      (SomeToggleable entity, _, observed) <- sample $ genWorldWithToggledAndTime Off (14, 40)
      let timePolicy =
            policy
              "turn light on after 14:39"
              ( if_ (currentTime `isGreaterThan` time @14 @39)
                  `then_` (entity `shouldBe` on)
              )
      let (verifiedTimePolicy, _) = verify observed timePolicy
          plan = reconcile observed verifiedTimePolicy

      renderedPlan <- renderPlanTrace Plain plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_time_comparison_success.golden" renderedPlan

    it "renders failed time comparison correctly" $ stagedGolden $ \goldenStage -> do
      (SomeToggleable entity, _, observed) <- sample $ genWorldWithToggledAndTime Off (14, 20)
      let timePolicy =
            policy
              "turn light on after 14:39"
              ( if_ (currentTime `isGreaterThan` time @14 @39)
                  `then_` (entity `shouldBe` off)
                  `else_` (entity `shouldBe` on)
              )
      let (verifiedTimePolicy, _) = verify observed timePolicy
          plan = reconcile observed verifiedTimePolicy

      renderedPlan <- renderPlanTrace Plain plan

      goldenStage $ pureGoldenTextFile "test_resources/ProvenanceSpec/trace_time_comparison_failure.golden" renderedPlan
