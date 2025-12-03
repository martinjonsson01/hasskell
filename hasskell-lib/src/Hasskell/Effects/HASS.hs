{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.HASS
  ( -- Interactions with Home Assistant
    HASS,
    runHASS,
    -- Actions
    getConfig,
    getStates,
    getEntities,
    getDevices,
    getServices,
    callService,
  )
where

import Data.List qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Hasskell.Effects.HASSConnection
import Hasskell.HomeAssistant.API

data HASS :: Effect where
  GetConfig :: HASS m HASSConfig
  GetStates :: HASS m [HASSState]
  GetEntities :: HASS m [HASSEntity]
  GetDevices :: HASS m [HASSDevice]
  GetServices :: HASS m HASSServiceActions
  CallService :: HASSDomain -> HASSServiceName -> EntityId -> HASS m ()

makeEffect ''HASS

runHASS ::
  (HASSConnection :> es) =>
  Eff (HASS : es) a ->
  Eff es a
runHASS = interpret_ $ \case
  GetConfig -> sendMessage CommandGetConfig
  GetStates -> sendMessage CommandGetStates
  GetEntities -> sendMessage CommandGetEntityRegistry
  GetDevices -> sendMessage CommandGetDeviceRegistry
  GetServices -> sendMessage CommandGetServices
  CallService domain service entityId ->
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
