module Hasskell.HomeAssistant.ClientSpec (spec) where

import Control.Concurrent.STM qualified as STM
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful.Concurrent.STM
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

    setTimeout 10 . it "can subscribe to event updates" $ do
      eventsVar <- STM.atomically $ newTVar []
      let retryUntilEvent = do
            events <- readTVar eventsVar
            case NE.nonEmpty events of
              Just eventsNE -> pure eventsNE
              Nothing -> retry
          entity = EntityId "sensor.cloud_gateway_ultra_memory_utilization"

      events <-
        runWithClient $
          subscribeToStateOf entity (modifyTVar eventsVar . (:))
            >> atomically retryUntilEvent

      let event = NE.head events
          getEventEntityId = triggeredEntityId . variablesTrigger . eventVariables
      (getEventEntityId event) `shouldBe` entity
