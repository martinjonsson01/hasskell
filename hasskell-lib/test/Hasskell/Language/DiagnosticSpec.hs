{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.DiagnosticSpec (spec) where

import Data.Text qualified as T
import Hasskell.Language.AST
import Hasskell.Language.Report
import Hasskell.Language.Verifier
import Hasskell.Language.World
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Verifier diagnostic" $ do
    specify "suggests correct entity on typos" $
      property $ do
        let expectedMatch = "some_entity_name"
            knownEntities = map light $ expectedMatch : ["light.some_other"]
        observed <- forAll (genWorldWithKnownEntities knownEntities)
        (_, report) <- verifyAnnotated observed (lightAlwaysOn (light "some_titynamr"))
        renderedReport <- renderReport Plain report
        let expectedSuggestion = "did you mean `" <> expectedMatch <> "`?"
        annotate (T.unpack expectedSuggestion)
        assert (expectedSuggestion `T.isInfixOf` renderedReport)

    specify "suggests correct entity on forgotten prefix" $
      property $ do
        let expectedMatch = "light.some_entity_name"
            knownEntities = map light $ expectedMatch : ["light.some_other"]
        observed <- forAll (genWorldWithKnownEntities knownEntities)
        (_, report) <- verifyAnnotated observed (lightAlwaysOn (light "some_entity_name"))
        renderedReport <- renderReport Plain report
        let expectedSuggestion = "did you mean `" <> expectedMatch <> "`?"
        annotate (T.unpack expectedSuggestion)
        assert (expectedSuggestion `T.isInfixOf` renderedReport)

    specify "displays correct domain on mismatch" $
      property $ do
        let entity = light "input_boolean.test"
        observed <- forAll (genWorldWithToggleds [observedInputBoolean entity On])
        (_, report) <- verifyAnnotated observed (lightAlwaysOn entity)
        renderedReport <- renderReport Plain report
        assert ("expected entity `input_boolean.test` to be light" `T.isInfixOf` renderedReport)
        assert ("actual domain is input_boolean" `T.isInfixOf` renderedReport)
