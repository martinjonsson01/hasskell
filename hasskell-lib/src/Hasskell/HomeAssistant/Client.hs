{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client
  ( runClient,
    ClientM,
    ClientError (..),
    HASSAuthResponse,
    HASSDomain (..),
    HASSServiceName (..),
    getConfig,
    getStates,
    getEntities,
    getDevices,
    getServices,
    callService,
  )
where

import Data.List qualified as L
import Data.Text (Text)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Exception
import Hasskell.Config (Config (..), Configured, runConfigured)
import Hasskell.Effects.Counter
import Hasskell.Effects.HASSConnection
import Hasskell.Effects.Logging
import Hasskell.Effects.Profiling
import Hasskell.Effects.Utils
import Hasskell.HomeAssistant.API

newtype ClientM a = Client
  { unClient ::
      Eff
        '[ Configured,
           Counter,
           Logger,
           Error ClientError,
           Profiling,
           HASSConnection,
           IOE
         ]
        a
  }
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

data ClientError
  = UnknownResponse Text
  | CommandFailure HASSFailure
  | ClientLogError LogError
  | ClientWebSocketError HASSWebSocketError
  deriving (Eq, Show)

instance Exception ClientError

runClient :: Config -> ClientM a -> IO (Either ClientError a)
runClient config Client {unClient} =
  let logConfig = logging config
   in runEff
        . runConcurrent
        . runProfiling
        . runErrorNoCallStack @ClientError
        . runMapError ClientLogError
        . runLogger logConfig
        . runCounter
        . runConfigured config
        . runMapError ClientWebSocketError
        . runWithHASSWebSocket
        . inject
        $ unClient

--------------------------------------------------------------------------------

getConfig :: ClientM HASSConfig
getConfig = Client $ sendMessage CommandGetConfig

getStates :: ClientM [HASSState]
getStates = Client $ sendMessage CommandGetStates

getEntities :: ClientM [HASSEntity]
getEntities = Client $ sendMessage CommandGetEntityRegistry

getDevices :: ClientM [HASSDevice]
getDevices = Client $ sendMessage CommandGetDeviceRegistry

getServices :: ClientM HASSServiceActions
getServices = Client $ sendMessage CommandGetServices

callService :: HASSDomain -> HASSServiceName -> EntityId -> ClientM ()
callService domain service entityId =
  Client $
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
