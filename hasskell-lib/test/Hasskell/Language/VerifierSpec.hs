module Hasskell.Language.VerifierSpec (spec) where

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
    specify "warns when referencing unknown entity" $
      property $ do
        (SomeToggleable unknownEntity, observed) <- forAll $ genWorldWithoutEntity
        (_, report) <- verifyAnnotated observed (lightAlwaysOn unknownEntity)
        assert (hasWarnings report)

    specify "does not warn when referencing known entity" $
      property $ do
        (SomeToggleable offEntity, _, observed) <- forAll $ genWorldWithToggled Off
        (_, report) <- verifyAnnotated observed (lightAlwaysOn offEntity)
        assert (not . hasWarnings $ report)
