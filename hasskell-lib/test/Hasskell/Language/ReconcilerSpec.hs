module Hasskell.Language.ReconcilerSpec (spec) where

import Hasskell
import Hasskell.Language.Reconciler
import Hasskell.TestUtils.Gen
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reconciler" $ do
    specify "can identify when a world needs no reconciliation" $
      property $ do
        (onEntity, observed) <- forAll $ genWorldWithToggled On
        let worldSpec = policy "light is always on" (isOn $ toEntity onEntity)
        let MkReconciliationPlan steps = reconcile observed worldSpec
        steps === []

    specify "does not create an empty plan for world that needs changes" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let worldSpec = policy "light is always on" (isOn $ toEntity offEntity)
            MkReconciliationPlan steps = reconcile observed worldSpec
        steps /== []

    specify "generates turn on command for entity" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let worldSpec = policy "light is always on" (isOn $ toEntity offEntity)
            MkReconciliationPlan steps = reconcile observed worldSpec
        steps === [TurnOnEntity offEntity]
