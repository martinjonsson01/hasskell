module Hasskell.Language.VerifierSpec (spec) where

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
  describe "Verifier" $ do
    it "warns about unknown entity" $ stagedGolden $ \goldenStage -> do
      let unknownEntity = light "some_unknown_entity"
      observed <- sample (genWorldWithoutThisEntity unknownEntity)
      let (_, report) = verify observed (lightAlwaysOn unknownEntity)
      renderedReport <- renderReport Plain report
      goldenStage $ pureGoldenTextFile "test_resources/VerifierSpec/warn_unknown_entity.golden" renderedReport

    specify "does not warn when referencing known entity" $
      property $ do
        (SomeToggleable offEntity, _, observed) <- forAll $ genWorldWithToggled Off
        (_, report) <- verifyAnnotated observed (lightAlwaysOn offEntity)
        assert (not . hasWarnings $ report)
