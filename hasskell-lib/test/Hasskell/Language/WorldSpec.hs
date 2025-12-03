module Hasskell.Language.WorldSpec (spec) where

import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.World
import Hasskell.TestUtils.Utils
import Test.Syd

spec :: Spec
spec = do
  describe "World" $ do
    it "toggleable entities are real entities" $ do
      realEntityIds <- map stateEntityId <$> runWithClient getStates
      toggleableEntityIds <-
        (map toggleableId . worldToggleables)
          <$> runWithClient collectCurrentState
      toggleableEntityIds `shouldBeSubsetOf` realEntityIds

    it "contains any toggleable entity" $ do
      toggleableEntityIds <-
        (map toggleableId . worldToggleables)
          <$> runWithClient collectCurrentState
      length (toggleableEntityIds) `shouldNotBe` 0
