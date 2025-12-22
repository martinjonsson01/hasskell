module Hasskell.Language.ReconcilerSpec (spec) where

import Data.Text qualified as T
import Hasskell
import Hasskell.HomeAssistant.API
import Hasskell.Language.Reconciler
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hedgehog
import Test.Syd hiding (shouldBe)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reconciler" $ do
    specify "can identify when a world needs no reconciliation" $
      property $ do
        state <- forAll $ genToggleState
        (entity, observed) <- forAll $ genWorldWithToggled state
        let (plan@(MkReconciliationPlan steps), report) = reconcile observed (lightAlways state entity)
        renderedReport <- renderReport report
        renderedPlan <- renderPlanTrace plan
        annotate (T.unpack renderedReport)
        annotate (T.unpack renderedPlan)
        steps === []

    specify "generates toggle command for entity" $
      property $ do
        state <- forAll $ genToggleState
        (entity, observed) <- forAll $ genWorldWithToggled state
        let opposite = if state == On then Off else On
        let (plan@(MkReconciliationPlan steps), report) = reconcile observed (lightAlways opposite entity)
        renderedReport <- renderReport report
        renderedPlan <- renderPlanTrace plan
        annotate (T.unpack renderedReport)
        annotate (T.unpack renderedPlan)
        (map stepAction steps) === [SetEntityState entity opposite]

    specify "conditionally turns off one light when another light is on" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, On), (lightB, On)]
        let boolPolicy =
              policy
                "if lightA is on, turn lightB off"
                ( eIf
                    (toggledStateOf lightA `is` On)
                    (lightB `shouldBe` Off)
                )
        let (plan@(MkReconciliationPlan steps), report) = reconcile observedOn boolPolicy
        renderedReport <- renderReport report
        renderedPlan <- renderPlanTrace plan
        annotate (T.unpack renderedReport)
        annotate (T.unpack renderedPlan)
        (map stepAction steps) === [SetEntityState lightB Off]

    specify "conditionally inverts light based on another" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, Off)]
        let boolPolicy =
              policy
                "make lightB the inverse of lightA"
                ( ifElse
                    (toggledStateOf lightA `is` On)
                    (lightB `shouldBe` Off)
                    (lightB `shouldBe` On)
                )
        let (plan@(MkReconciliationPlan steps), report) = reconcile observedOn boolPolicy
        renderedReport <- renderReport report
        renderedPlan <- renderPlanTrace plan
        annotate (T.unpack renderedReport)
        annotate (T.unpack renderedPlan)
        (map stepAction steps) === [SetEntityState lightB On]

    specify "does nothing when condition evaluates to false" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, On)]
        let boolPolicy =
              policy
                "if lightA is on, turn lightB off"
                ( eIf
                    (toggledStateOf lightA `is` On)
                    (lightB `shouldBe` Off)
                )
        let (plan@(MkReconciliationPlan steps), report) = reconcile observedOn boolPolicy
        renderedReport <- renderReport report
        renderedPlan <- renderPlanTrace plan
        annotate (T.unpack renderedReport)
        annotate (T.unpack renderedPlan)
        (map stepAction steps) === []

  describe "Reconciler warnings" $ do
    specify "are generated when referencing unknown entity" $
      property $ do
        (unknownEntity, observed) <- forAll $ genWorldWithoutEntity
        let (_, report) = reconcile observed (lightAlwaysOn unknownEntity)
        renderedReport <- renderReport report
        annotate (T.unpack renderedReport)
        assert (hasWarnings report)

    specify "are not generated when referencing known entity" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let (_, report) = reconcile observed (lightAlwaysOn offEntity)
        renderedReport <- renderReport report
        annotate (T.unpack renderedReport)
        assert (not . hasWarnings $ report)
