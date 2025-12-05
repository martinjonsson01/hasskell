module Hasskell.Language.WorldSpec (spec) where

import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.World
import Hasskell.TestUtils.Utils
import Test.Syd

spec :: Spec
spec = do
  describe "Collected current world state" $ do
    it "toggleable entities are real entities" $ do
      realEntityIds <- map stateEntityId <$> runWithClient getStates
      MkObserved _ world <- runWithClient collectCurrentState
      let toggleableEntityIds = map toggleableId . worldToggleables $ world
      toggleableEntityIds `shouldBeSubsetOf` realEntityIds

    it "contains any toggleable entity" $ do
      MkObserved _ world <- runWithClient collectCurrentState
      let toggleableEntityIds = map toggleableId . worldToggleables $ world
      length (toggleableEntityIds) `shouldNotBe` 0
