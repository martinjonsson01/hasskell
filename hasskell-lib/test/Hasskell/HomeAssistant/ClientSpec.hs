module Hasskell.HomeAssistant.ClientSpec (spec) where

import Data.Text qualified as T
import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Hasskell.TestUtils.Utils
import Test.Syd

spec :: Spec
spec = do
  describe "HASS Client" $ do
    it "can get config" $ do
      config <- runWithClient getConfig
      configVersion config `shouldSatisfy` ("20" `T.isPrefixOf`)

    it "can get states" $ do
      states <- runWithClient getStates
      length (states) `shouldNotBe` 0

    it "can get entities" $ do
      entities <- runWithClient getEntities
      length (entities) `shouldNotBe` 0

    it "can get devices" $ do
      devices <- runWithClient getDevices
      length (devices) `shouldNotBe` 0

    it "can get services" $ do
      services <- runWithClient getServices
      length (services) `shouldNotBe` 0

    it "can subscribe to event updates" $ do
      runWithClient (subscribeToStateOf (EntityId "light.bollampa"))
