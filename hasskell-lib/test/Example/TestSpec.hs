module Example.TestSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  describe "(+)" $ do 
    it "does what you want it to" $ 
      (2 :: Integer)+ 3 == 5 
