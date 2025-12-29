module Hasskell.Language.WorldSpec (spec) where

import Data.HashMap.Strict qualified as HMap
import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.World
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd hiding (shouldBe)
import Test.Syd.Hedgehog ()

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

    specify "correctly updates toggle state on state changed event" $ do
      property $ do
        state <- forAll $ genToggleState
        (entity, _, observed) <- forAll $ genWorldWithToggled state
        let toggledState = toggle state
            entityId = idOf entity
            event = StateChanged entityId toggledState
            updatedObserved = updateWorld observed event
            updatedToggleables = worldToggleables . observedWorld $ updatedObserved
        updatedToggleables HMap.!? entityId === Just toggledState
