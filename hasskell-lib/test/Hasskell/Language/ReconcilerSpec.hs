module Hasskell.Language.ReconcilerSpec (spec) where

import Data.Text qualified as T
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
        let (MkReconciliationPlan steps, report) = reconcile observed (lightAlwaysOn onEntity)
        renderedReport <- renderReport report
        annotate (T.unpack renderedReport)
        steps === []

    specify "does not create an empty plan for world that needs changes" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let (MkReconciliationPlan steps, report) = reconcile observed (lightAlwaysOn offEntity)
        renderedReport <- renderReport report
        annotate (T.unpack renderedReport)
        steps /== []

    specify "generates turn on command for entity" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let (MkReconciliationPlan steps, report) = reconcile observed (lightAlwaysOn offEntity)
        renderedReport <- renderReport report
        annotate (T.unpack renderedReport)
        (map stepAction steps) === [TurnOnEntity offEntity]

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
