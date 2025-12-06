module Hasskell.Language.ReconcilerSpec (spec) where

import Hasskell
import Hasskell.Language.Reconciler
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reconciler" $ do
    specify "can identify when a world needs no reconciliation" $
      property $ do
        (onEntity, observed) <- forAll $ genWorldWithToggled On
        let MkReconciliationPlan steps = reconcile observed (lightAlwaysOn onEntity)
        steps === []

    specify "does not create an empty plan for world that needs changes" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let MkReconciliationPlan steps = reconcile observed (lightAlwaysOn offEntity)
        steps /== []

    specify "generates turn on command for entity" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let MkReconciliationPlan steps = reconcile observed (lightAlwaysOn offEntity)
        steps === [TurnOnEntity offEntity]
