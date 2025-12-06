module Hasskell.Language.InterpreterSpec (spec) where

import Hasskell.Language.AST
import Hasskell.Language.Interpreter
import Hasskell.Language.World
import Hasskell.TestUtils.Gen
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Interpreter" $ do
    specify "can describe a light being on" $
      property $ do
        (offEntity, observed) <- forAll $ genWorldWithToggled Off
        let worldSpec = policy "light is always on" (isOn $ toEntity offEntity)
        let worldWithLightOn = observed `withEntityOn` offEntity
        interpret worldSpec observed === worldWithLightOn
