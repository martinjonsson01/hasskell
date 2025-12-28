{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client
  ( runClient,
    ClientError (..),
    ClientM,
    HASSAuthResponse,
  )
where

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Exception
import Hasskell.Config (Config (..), Configured, runConfigured)
import Hasskell.Effects.Counter
import Hasskell.Effects.HASS
import Hasskell.Effects.HASSConnection
import Hasskell.Effects.Logging
import Hasskell.Effects.Profiling
import Hasskell.Effects.Utils
import Hasskell.HomeAssistant.API

type ClientM =
  Eff
    '[ Concurrent,
       Configured,
       Counter,
       Logger,
       Error ClientError,
       Profiling,
       HASSConnection,
       HASS,
       IOE
     ]

data ClientError
  = ClientLogError LogError
  | ClientWebSocketError HASSWebSocketError
  deriving (Eq, Show)

instance Exception ClientError

runClient :: Config -> ClientM a -> IO (Either ClientError a)
runClient config client =
  let logConfig = logging config
   in runEff
        . runConcurrent
        . runErrorNoCallStack @ClientError
        . runMapError ClientLogError
        . runLogger logConfig
        . runProfiling
        . runCounter
        . runConfigured config
        . runMapError ClientWebSocketError
        . runWithHASSWebSocket
        . runHASS
        . inject
        $ client
