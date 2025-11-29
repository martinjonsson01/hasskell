{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client
  ( runClient,
    ClientM,
    doHASSInteractions,
    ClientError,
    HASSAuthResponse,
    getConfig,
    getStates,
    getServices,
  )
where

import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
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
import Text.Show.Pretty

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

getServices :: ClientM HASSServiceActions
getServices = Client $ sendMessage CommandGetServices

doHASSInteractions :: ClientM ()
doHASSInteractions = Client $ do
  actionResult :: HASSActionResult <-
    sendMessage
      ( CommandCallService
          { commandReturnResponse = False,
            commandTarget =
              Just
                ( Target
                    { targetEntityId = L.singleton "light.flaktlampa",
                      targetDeviceId = mempty,
                      targetLabelId = mempty,
                      targetAreaId = mempty
                    }
                ),
            commandServiceData = Nothing,
            commandService = ServiceName "toggle",
            commandDomain = Domain "light"
          }
      )
  logDebug $ T.pack $ ppShow actionResult
