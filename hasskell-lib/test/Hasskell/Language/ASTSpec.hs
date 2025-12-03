module Hasskell.Language.ASTSpec (spec) where

import Data.Text (Text)
import Hasskell.Language.AST
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Specification language" $ do
    specify "can describe turning on any entity" $
      property $ do
        e <- forAll genEntity
        let specification =
              policies
                [ policy "light on" $ turnOn e
                ]
        liftIO $ specification `shouldSatisfy` \_ -> True

genEntityName :: Gen Text
genEntityName = Gen.text (Range.constant 1 10) Gen.alphaNum

genEntity :: Gen (Exp 'TEntity)
genEntity = entity <$> genEntityName
