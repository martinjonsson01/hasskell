module Hasskell.HomeAssistant.ClientSpec (spec) where

import Control.Concurrent.STM qualified as STM
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful.Concurrent.STM
import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.TestUtils.Utils
import Test.Syd hiding (shouldBe)
import Test.Syd qualified as Syd

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

    it "can get an entity's domains" $ do
      let entity = inputBoolean "input_boolean.test"
          eId = idOf entity
      supportedServices <- runWithClient (getSupportedServicesOf eId)
      let domains = HM.keys supportedServices
      domains `shouldContain` [domainInputBoolean]

    it "can get an entity's supported services" $ do
      let entity = inputBoolean "input_boolean.test"
          eId = idOf entity
      supportedServices <- runWithClient (getSupportedServicesOf eId)
      let services = mconcat (HM.elems supportedServices)
          expectedServices = HS.fromList [serviceTurnOff, serviceTurnOff, serviceToggle]
      context ("expected: " <> ppShow expectedServices) $
        services `shouldSatisfy` HS.isSubsetOf expectedServices

    setTimeout 1 . it "can subscribe to event updates" $ do
      eventsVar <- STM.atomically $ newTVar []
      let retryUntilEvent = do
            events <- readTVar eventsVar
            case NE.nonEmpty events of
              Just eventsNE -> pure eventsNE
              Nothing -> retry
          entity = inputBoolean "input_boolean.test"
          entityId = idOf entity

      events <-
        runWithClient $ do
          -- Start with the boolean off.
          turnOff domainInputBoolean entityId
          -- Set up the subscriber.
          subscribeToStateOf entityId (modifyTVar eventsVar . (:))
          -- Trigger a change in the entity, so an event is generated.
          turnOn domainInputBoolean entityId
          -- Wait until the event is received.
          atomically retryUntilEvent

      let event = NE.head events
          getEventEntityId = triggeredEntityId . variablesTrigger . eventVariables
      (getEventEntityId event) `Syd.shouldBe` entityId
