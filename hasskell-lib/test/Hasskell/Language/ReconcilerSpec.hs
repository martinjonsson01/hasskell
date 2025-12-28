module Hasskell.Language.ReconcilerSpec (spec) where

import Data.Text (Text)
import Data.Time
import Hasskell
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Reconciler
import Hasskell.Language.World
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd hiding (shouldBe)
import Test.Syd qualified as Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reconciler" $ do
    specify "can identify when a world needs no reconciliation" $
      property $ do
        state <- forAll $ genToggleState
        (entity, observed) <- forAll $ genWorldWithToggled state
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observed (lightAlways state entity)
        steps === []

    specify "generates toggle command for entity" $
      property $ do
        state <- forAll $ genToggleState
        (entity, observed) <- forAll $ genWorldWithToggled state
        let opposite = if state == On then Off else On
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observed (lightAlways opposite entity)
        (map stepAction steps) === [SetEntityState entity opposite]

    specify "conditionally turns off one light when another light is on" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, On), (lightB, On)]
        let boolPolicy =
              policy
                "if lightA is on, turn lightB off"
                ( if_ (toggledStateOf lightA `is` on)
                    `then_` (lightB `shouldBe` off)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === [SetEntityState lightB Off]

    specify "conditionally inverts light based on another" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, Off)]
        let boolPolicy =
              policy
                "make lightB the inverse of lightA"
                ( if_ (toggledStateOf lightA `is` on)
                    `then_` (lightB `shouldBe` off)
                    `else_` (lightB `shouldBe` on)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === [SetEntityState lightB On]

    specify "does nothing when condition evaluates to false" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, On)]
        let boolPolicy =
              policy
                "if lightA is on, turn lightB off"
                ( if_ (toggledStateOf lightA `is` on)
                    `then_` (lightB `shouldBe` off)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === []

    specify "mirrors state of two lights" $
      property $ do
        let lightA = EntityId "lightA"
            lightB = EntityId "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, On)]
        let boolPolicy =
              policy
                "mirror lightA state to lightB"
                (lightB `shouldBe` toggledStateOf lightA)
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === [SetEntityState lightB Off]

    specify "sets state depending on current time" $
      property $ do
        (entity, observed) <- forAll $ genWorldWithToggledAndTime Off (14, 39)
        let timePolicy =
              policy
                "turn light on at 14:39"
                ( if_ (currentTime `is` time @14 @39)
                    `then_` (entity `shouldBe` on)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observed timePolicy
        (map stepAction steps) === [SetEntityState entity On]

    specify "sets state when time is greater than" $
      timeComparisonProperty "after" isGreaterThan (>)

    specify "sets state when time is greater than or equal" $
      timeComparisonProperty "after or at" isGreaterOrEqualTo (>=)

    specify "greater than does not succeed on equal time" $
      timeEqualTest "after" isGreaterThan (const [])

    specify "greater or equal to succeeds on equal time" $
      timeEqualTest "after or at" isGreaterOrEqualTo (\entity -> [SetEntityState entity On])

  describe "Reconciler warnings" $ do
    specify "are generated when referencing unknown entity" $
      property $ do
        (unknownEntity, observed) <- forAll $ genWorldWithoutEntity
        (_, report) <- reconcileAnnotated observed (lightAlwaysOn unknownEntity)
        assert (hasWarnings report)

    specify "are not generated when referencing known entity" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        (_, report) <- reconcileAnnotated observed (lightAlwaysOn offEntity)
        assert (not . hasWarnings $ report)

type ComparisonExp =
  Located (Exp 'TTime) ->
  Located (Exp 'TTime) ->
  Located (Exp 'TBool)

timeComparisonProperty ::
  Text ->
  ComparisonExp ->
  (TimeOfDay -> TimeOfDay -> Bool) ->
  Property
timeComparisonProperty name comparer compareOp = property $ do
  (entity, observed) <- forAll $ genWorldWithToggled Off
  let timePolicy =
        policy
          ("turn light on " <> name <> " 13:42")
          ( if_ (currentTime `comparer` time @13 @42)
              `then_` (entity `shouldBe` on)
          )
      observedTime = observedTimeOfDay observed
      cutoffTime = TimeOfDay 13 42 0
  (MkReconciliationPlan steps, _) <- reconcileAnnotated observed timePolicy
  (map stepAction steps)
    === if observedTime `compareOp` cutoffTime then [SetEntityState entity On] else []

timeEqualTest :: Text -> ComparisonExp -> (EntityId -> [ReconciliationAction]) -> IO ()
timeEqualTest name comparer expectedActions = do
  (entity, observed) <- sample $ genWorldWithToggledAndTime Off (13, 42)
  let timePolicy =
        policy
          ("turn light on " <> name <> " 13:42")
          ( if_ (currentTime `comparer` time @13 @42)
              `then_` (entity `shouldBe` on)
          )
      (MkReconciliationPlan steps, _) = reconcile observed timePolicy
  (map stepAction steps) `Syd.shouldBe` (expectedActions entity)
