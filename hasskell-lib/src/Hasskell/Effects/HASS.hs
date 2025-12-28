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
    turnOnLight,
    turnOffLight,
    subscribeToStateOf,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Hasskell.Effects.HASSConnection
import Hasskell.Effects.Profiling
import Hasskell.HomeAssistant.API
import Prettyprinter

data HASS :: Effect where
  GetConfig :: HASS m HASSConfig
  GetStates :: HASS m [HASSState]
  GetEntities :: HASS m [HASSEntity]
  GetDevices :: HASS m [HASSDevice]
  GetServices :: HASS m HASSServiceActions
  TurnOnLight :: EntityId -> HASS m ()
  TurnOffLight :: EntityId -> HASS m ()
  SubscribeToStateOf :: EntityId -> HASS m ()

makeEffect ''HASS

instance Pretty (HASS a b) where
  pretty = \case
    GetConfig -> "get config"
    GetStates -> "get states"
    GetEntities -> "get entities"
    GetDevices -> "get devices"
    GetServices -> "get services"
    TurnOnLight eId -> "turn on" <+> pretty eId
    TurnOffLight eId -> "turn off" <+> pretty eId
    SubscribeToStateOf eId -> "subscribe to state of" <+> pretty eId

runHASS ::
  ( HASSConnection :> es,
    Profiling :> es
  ) =>
  Eff (HASS : es) a ->
  Eff es a
runHASS = interpret_ $ \command -> profile (T.show $ pretty command) $ case command of
  GetConfig -> sendMessage CommandGetConfig
  GetStates -> sendMessage CommandGetStates
  GetEntities -> sendMessage CommandGetEntityRegistry
  GetDevices -> sendMessage CommandGetDeviceRegistry
  GetServices -> sendMessage CommandGetServices
  TurnOnLight entity -> callService domainLight serviceTurnOn entity
  TurnOffLight entity -> callService domainLight serviceTurnOff entity
  SubscribeToStateOf entity -> createStateSubscription entity

createStateSubscription :: (HASSConnection :> es) => EntityId -> Eff es ()
createStateSubscription eId =
  sendMessage
    ( CommandSubscribeTrigger
        { commandTrigger =
            Trigger
              { triggerPlatform = "state",
                triggerEntityId = eId,
                triggerFrom = "off",
                triggerTo = "on"
              }
        }
    )

callService :: (HASSConnection :> es) => HASSDomain -> HASSServiceName -> EntityId -> Eff es ()
callService domain service entityId =
  sendMessage
    ( CommandCallService
        { commandReturnResponse = False,
          commandTarget =
            Just
              ( Target
                  { targetEntityId = L.singleton entityId,
                    targetDeviceId = mempty,
                    targetLabelId = mempty,
                    targetAreaId = mempty
                  }
              ),
          commandServiceData = Nothing,
          commandService = service,
          commandDomain = domain
        }
    )
