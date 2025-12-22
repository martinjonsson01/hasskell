module Hasskell.Language.WorldSpec (spec) where

import Data.HashMap.Strict qualified as HMap
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
      let toggleableEntityIds = HMap.keys . worldToggleables $ world
      toggleableEntityIds `shouldBeSubsetOf` realEntityIds

    it "contains any toggleable entity" $ do
      MkObserved _ world <- runWithClient collectCurrentState
      let toggleableEntityIds = HMap.keys . worldToggleables $ world
      length (toggleableEntityIds) `shouldNotBe` 0
