{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client
  ( runClient,
    doHASSInteractions,
    ClientError,
    HASSAuthResponse,
  )
where

import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Exception
import Hasskell.Config
import Hasskell.HomeAssistant.API
import Hasskell.Monad
import Text.Show.Pretty

newtype ClientM a = Client
  { unClient ::
      Eff
        '[ Configured,
           CorrelationIdSource,
           Logger,
           Error ClientError,
           Profiling,
           HASSConnection,
           IOE
         ]
        a
  }

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

doHASSInteractions :: ClientM ()
doHASSInteractions = Client $ do
  configResult :: HASSConfig <- sendMessage CommandGetConfig
  logDebug $ T.pack $ ppShow configResult

  states :: [HASSState] <- sendMessage CommandGetStates
  logDebug $ "state count: " <> (T.show $ length $ states)

  actions :: (HASSServiceActions) <- sendMessage CommandGetServices
  logDebug $ "action count: " <> (T.show $ length $ actions)

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
