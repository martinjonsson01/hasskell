{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.Logging
  ( -- Logging
    LoggingConfig (..),
    Logger,
    logDebug,
    logInfo,
    logError,
    runLogger,
    runDefaultLogger,
    LogError (..),
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.TH

data LoggingConfig = Logging
  { debugLogger :: !(Text -> IO ()),
    infoLogger :: !(Text -> IO ()),
    errorLogger :: !(Text -> IO ())
  }

data Logger :: Effect where
  LogDebug :: Text -> Logger m ()
  LogInfo :: Text -> Logger m ()
  LogError :: Text -> Logger m ()

makeEffect ''Logger

data LogError = LogException IOException
  deriving (Eq, Show)

runLogger ::
  (IOE :> es) =>
  LoggingConfig ->
  Eff (Logger : Error LogError : es) a ->
  Eff (Error LogError : es) a
runLogger Logging {debugLogger, infoLogger, errorLogger} = interpret_ $ \case
  LogDebug text -> adapt $ debugLogger text
  LogInfo text -> adapt $ infoLogger text
  LogError text -> adapt $ errorLogger text
  where
    adapt action = liftIO action `catchIO` (throwError . LogException)

runDefaultLogger ::
  (IOE :> es) =>
  Eff (Logger : Error LogError : es) a ->
  Eff (Error LogError : es) a
runDefaultLogger = runLogger defaultLogging
  where
    defaultLogging :: LoggingConfig
    defaultLogging =
      Logging
        { debugLogger = putStrLn . T.unpack,
          infoLogger = putStrLn . T.unpack,
          errorLogger = putStrLn . T.unpack
        }
