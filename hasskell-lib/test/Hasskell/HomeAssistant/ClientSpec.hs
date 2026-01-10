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
          eId = makeKnownEntityIdUnsafe (idOf entity)
      supportedServices <- runWithClient (getSupportedServicesOf eId)
      let domains = HM.keys supportedServices
      domains `shouldContain` [domainInputBoolean]

    it "can get an entity's supported services" $ do
      let entity = inputBoolean "input_boolean.test"
          eId = makeKnownEntityIdUnsafe (idOf entity)
      supportedServices <- runWithClient (getSupportedServicesOf eId)
      let services = mconcat (HM.elems supportedServices)
          expectedServices = HS.fromList [serviceTurnOff, serviceTurnOff, serviceToggle]
      context ("expected: " <> ppShow expectedServices) $
        services `shouldSatisfy` HS.isSubsetOf expectedServices

    setTimeout 1 . it "can subscribe to changes" $ do
      changesVar <- STM.atomically $ newTVar []
      let retryUntilChange = do
            changes <- readTVar changesVar
            case NE.nonEmpty changes of
              Just changesNE -> pure changesNE
              Nothing -> retry
          entity = inputBoolean "input_boolean.test"
          entityId = makeKnownEntityIdUnsafe (idOf entity)

      changes <-
        runWithClient $ do
          -- Start with the boolean off.
          turnOff domainInputBoolean entityId
          -- Set up the subscriber.
          subscribeToStateOf entityId (modifyTVar changesVar . (:))
          -- Trigger an update in the entity, so a change is generated.
          turnOn domainInputBoolean entityId
          -- Wait until the change is received.
          atomically retryUntilChange

      let change = NE.head changes
          getchangeEntityId = triggeredEntityId . variablesTrigger . changeVariables
      (getchangeEntityId change) `Syd.shouldBe` entityId
