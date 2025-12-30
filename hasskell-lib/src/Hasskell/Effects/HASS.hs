{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.HASS
  ( -- Interactions with Home Assistant
    HASS (..),
    runHASS,
    -- Actions
    getConfig,
    getStates,
    getEntities,
    getDevices,
    getServices,
    getSupportedServicesOf,
    turnOn,
    turnOff,
    subscribeToStateOf,
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List.Extra
import Data.Maybe
import Data.Text qualified as T
import Data.Tuple.HT
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Hasskell.Effects.Counter (CorrelationId)
import Hasskell.Effects.HASSConnection
import Hasskell.Effects.Logging
import Hasskell.Effects.Profiling
import Hasskell.HomeAssistant.API
import Prettyprinter

type StateChangeEventHandler = HASSEvent -> STM ()

data HASS :: Effect where
  GetConfig :: HASS m HASSConfig
  GetStates :: HASS m [HASSState]
  GetEntities :: HASS m [HASSEntity]
  GetDevices :: HASS m [HASSDevice]
  GetServices :: HASS m HASSServiceActions
  GetSupportedServicesOf :: EntityId -> HASS m (HashMap HASSDomain (HashSet HASSServiceName))
  TurnOn :: HASSDomain -> EntityId -> HASS m ()
  TurnOff :: HASSDomain -> EntityId -> HASS m ()
  SubscribeToStateOf :: EntityId -> StateChangeEventHandler -> HASS m ()

makeEffect ''HASS

instance Pretty (HASS a b) where
  pretty = \case
    GetConfig -> "get config"
    GetStates -> "get states"
    GetEntities -> "get entities"
    GetDevices -> "get devices"
    GetServices -> "get services"
    GetSupportedServicesOf eId -> "get supported services of" <+> pretty eId
    TurnOn domain eId -> "turn on" <+> pretty domain <+> pretty eId
    TurnOff domain eId -> "turn off" <+> pretty domain <+> pretty eId
    SubscribeToStateOf eId _ -> "subscribe to state of" <+> pretty eId

runHASS ::
  ( HASSConnection :> es,
    Profiling :> es,
    Logger :> es,
    Concurrent :> es
  ) =>
  Eff (HASS : es) a ->
  Eff es a
runHASS action = do
  subscriptionsVar <- atomically $ newTVar (mempty :: HashMap CorrelationId (Async ()))

  result <- interpretWith_ action $ \command -> profile (T.show $ pretty command) $ case command of
    GetConfig -> sendMessage CommandGetConfig
    GetStates -> sendMessage CommandGetStates
    GetEntities -> sendMessage CommandGetEntityRegistry
    GetDevices -> sendMessage CommandGetDeviceRegistry
    GetServices -> sendMessage CommandGetServices
    GetSupportedServicesOf entity -> sendGetSupportedServicesOf entity
    TurnOn domain entity -> callService domain serviceTurnOn entity
    TurnOff domain entity -> callService domain serviceTurnOff entity
    SubscribeToStateOf entity handler -> createStateSubscription subscriptionsVar entity handler

  subscriptions <- atomically $ readTVar subscriptionsVar
  logDebug $
    "HASS interpreter finished, cancelling "
      <> T.show (HM.size subscriptions)
      <> " subscription event handlers..."
  cancelMany (HM.elems subscriptions)

  pure result

sendGetSupportedServicesOf ::
  (HASSConnection :> es) =>
  EntityId ->
  Eff es (HashMap HASSDomain (HashSet HASSServiceName))
sendGetSupportedServicesOf =
  (HM.fromList . map (mapSnd HS.fromList) . groupSort . mapMaybe splitQualifiedServiceName <$>)
    . sendMessage
    . CommandGetServicesForTarget
    . targetEntity

createStateSubscription ::
  ( HASSConnection :> es,
    Concurrent :> es
  ) =>
  TVar (HashMap CorrelationId (Async ())) ->
  EntityId ->
  StateChangeEventHandler ->
  Eff es ()
createStateSubscription subscriptionsVar eId handler = do
  (subscriptionId, ()) <-
    sendMessageGetId
      ( CommandSubscribeTrigger
          { commandTrigger =
              Trigger
                { triggerPlatform = "state",
                  triggerEntityId = eId,
                  triggerFrom = Nothing,
                  triggerTo = Nothing
                }
          }
      )

  eventListener <- async $ do
    event <- receiveEvent subscriptionId
    atomically $ handler event
  link eventListener

  atomically $ modifyTVar subscriptionsVar (HM.insert subscriptionId eventListener)

callService :: (HASSConnection :> es) => HASSDomain -> HASSServiceName -> EntityId -> Eff es ()
callService domain service entityId =
  sendMessage
    ( CommandCallService
        { commandReturnResponse = False,
          commandTarget = targetEntity entityId,
          commandServiceData = Nothing,
          commandService = service,
          commandDomain = domain
        }
    )
