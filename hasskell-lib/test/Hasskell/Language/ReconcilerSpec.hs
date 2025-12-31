module Hasskell.Language.ReconcilerSpec (spec) where

import Data.Text (Text)
import Data.Time
import Hasskell
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Reconciler
import Hasskell.Language.Verifier
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
        (SomeToggleable entity, _, observed) <- forAll $ genWorldWithToggled state
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observed (lightAlways state entity)
        steps === []

    specify "generates toggle command for light" $
      property $ do
        state <- forAll $ genToggleState
        (SomeToggleable entity, domain, observed) <- forAll $ genWorldWithToggled state
        let entityId = makeKnownEntityIdUnsafe (idOf entity)
            opposite = if state == On then Off else On
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observed (lightAlways opposite entity)
        (map stepAction steps) === [SetEntityState entityId domain opposite]

    specify "conditionally turns off a light when a boolean is on" $
      property $ do
        let lightEntity = light "light"
            booleanEntity = inputBoolean "boolean"
        observedOn <-
          forAll $
            genWorldWithToggleds'
              [ (SomeToggleable lightEntity, On),
                (SomeToggleable booleanEntity, On)
              ]
        let boolPolicy =
              policy
                "if boolean is on, turn light off"
                ( if_ (toggledStateOf booleanEntity `is` on)
                    `then_` (lightEntity `shouldBe` off)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === [SetEntityState (makeKnownEntityIdUnsafe (idOf lightEntity)) domainLight Off]

    specify "conditionally inverts light based on another" $
      property $ do
        let lightA = light "lightA"
            lightB = light "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, Off)]
        let boolPolicy =
              policy
                "make lightB the inverse of lightA"
                ( if_ (toggledStateOf lightA `is` on)
                    `then_` (lightB `shouldBe` off)
                    `else_` (lightB `shouldBe` on)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === [SetEntityState (makeKnownEntityIdUnsafe (idOf lightB)) domainLight On]

    specify "does nothing when condition evaluates to false" $
      property $ do
        let lightA = light "lightA"
            lightB = light "lightB"
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
        let lightA = light "lightA"
            lightB = light "lightB"
        observedOn <- forAll $ genWorldWithToggleds [(lightA, Off), (lightB, On)]
        let boolPolicy =
              policy
                "mirror lightA state to lightB"
                (lightB `shouldBe` toggledStateOf lightA)
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observedOn boolPolicy
        (map stepAction steps) === [SetEntityState (makeKnownEntityIdUnsafe (idOf lightB)) domainLight Off]

    specify "sets state depending on current time" $
      property $ do
        (SomeToggleable entity, domain, observed) <- forAll $ genWorldWithToggledAndTime Off (14, 39)
        let timePolicy =
              policy
                "turn light on at 14:39"
                ( if_ (currentTime `is` time @14 @39)
                    `then_` (entity `shouldBe` on)
                )
        (MkReconciliationPlan steps, _) <- reconcileAnnotated observed timePolicy
        (map stepAction steps) === [SetEntityState (makeKnownEntityIdUnsafe (idOf entity)) domain On]

    specify "sets state in light domain for light entity" $
      toggleDomainProperty genWorldWithToggledLight domainLight

    specify "sets state in input boolean domain for input boolean entity" $
      toggleDomainProperty genWorldWithToggledInputBoolean domainInputBoolean

  describe "Reconciliation of comparisons" $ do
    specify "sets state when time is greater than" $
      timeComparisonProperty "after" isGreaterThan (>)

    specify "sets state when time is greater than or equal" $
      timeComparisonProperty "after or at" isGreaterOrEqualTo (>=)

    specify "sets state when time is less than" $
      timeComparisonProperty "before" isLessThan (<)

    specify "sets state when time is less than or equal" $
      timeComparisonProperty "before or at" isLessOrEqualTo (<=)

    specify "greater than does not succeed on equal time" $
      timeEqualTest "after" isGreaterThan (const2 [])

    specify "greater or equal to succeeds on equal time" $
      timeEqualTest
        "after or at"
        isGreaterOrEqualTo
        (\entity domain -> [SetEntityState entity domain On])

    specify "less than does not succeed on equal time" $
      timeEqualTest "before" isLessThan (const2 [])

    specify "less or equal to succeeds on equal time" $
      timeEqualTest
        "before or at"
        isLessOrEqualTo
        (\entity domain -> [SetEntityState entity domain On])

toggleDomainProperty ::
  (ToggleState -> Gen (SomeToggleable, ObservedWorld)) ->
  HASSDomain ->
  Property
toggleDomainProperty worldGen expectedDomain = property $ do
  state <- forAll $ genToggleState
  (SomeToggleable lightEntity, observed) <- forAll $ worldGen state
  let entityId = makeKnownEntityIdUnsafe (idOf lightEntity)
      opposite = if state == On then Off else On
  (MkReconciliationPlan steps, _) <- reconcileAnnotated observed (lightAlways opposite lightEntity)
  (map stepAction steps) === [SetEntityState entityId expectedDomain opposite]

type ComparisonExp =
  Located (Exp Raw 'TTime) ->
  Located (Exp Raw 'TTime) ->
  Located (Exp Raw 'TBool)

timeComparisonProperty ::
  Text ->
  ComparisonExp ->
  (TimeOfDay -> TimeOfDay -> Bool) ->
  Property
timeComparisonProperty name comparer compareOp = property $ do
  (SomeToggleable entity, domain, observed) <- forAll $ genWorldWithToggled Off
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
    === if observedTime `compareOp` cutoffTime
      then [SetEntityState (makeKnownEntityIdUnsafe (idOf entity)) domain On]
      else []

timeEqualTest :: Text -> ComparisonExp -> (KnownEntityId -> HASSDomain -> [ReconciliationAction]) -> IO ()
timeEqualTest name comparer expectedActions = do
  (SomeToggleable entity, domain, observed) <- sample $ genWorldWithToggledAndTime Off (13, 42)
  let timePolicy =
        policy
          ("turn light on " <> name <> " 13:42")
          ( if_ (currentTime `comparer` time @13 @42)
              `then_` (entity `shouldBe` on)
          )
      (verifiedTimePolicy, _) = verify observed timePolicy
      MkReconciliationPlan steps = reconcile observed verifiedTimePolicy
  (map stepAction steps) `Syd.shouldBe` (expectedActions (makeKnownEntityIdUnsafe (idOf entity)) domain)
