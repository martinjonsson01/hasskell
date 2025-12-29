{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.DiagnosticSpec (spec) where

import Data.Text qualified as T
import Hasskell.Language.AST
import Hasskell.Language.Reconciler
import Hasskell.Language.Report
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Reconciler diagnostic" $ do
    it "warns about unknown entity" $ stagedGolden $ \goldenStage -> do
      (SomeToggleable unknownEntity, observed) <- sample genWorldWithoutEntity
      let (_, report) = reconcile observed (lightAlwaysOn unknownEntity)
      renderedReport <- renderReport Plain report
      goldenStage $ pureGoldenTextFile "test_resources/DiagnosticSpec/warn_unknown_entity.golden" renderedReport

    specify "suggests correct entity on typos" $
      property $ do
        let expectedMatch = "some_entity_name"
            knownEntities = map light $ expectedMatch : ["light.some_other"]
        observed <- forAll (genWorldWithKnownEntities knownEntities)
        (_, report) <- reconcileAnnotated observed (lightAlwaysOn (light "some_titynamr"))
        renderedReport <- renderReport Plain report
        let expectedSuggestion = "did you mean `" <> expectedMatch <> "`?"
        annotate (T.unpack expectedSuggestion)
        assert (expectedSuggestion `T.isInfixOf` renderedReport)

    specify "suggests correct entity on forgotten prefix" $
      property $ do
        let expectedMatch = "light.some_entity_name"
            knownEntities = map light $ expectedMatch : ["light.some_other"]
        observed <- forAll (genWorldWithKnownEntities knownEntities)
        (_, report) <- reconcileAnnotated observed (lightAlwaysOn (light "some_entity_name"))
        renderedReport <- renderReport Plain report
        let expectedSuggestion = "did you mean `" <> expectedMatch <> "`?"
        annotate (T.unpack expectedSuggestion)
        assert (expectedSuggestion `T.isInfixOf` renderedReport)
