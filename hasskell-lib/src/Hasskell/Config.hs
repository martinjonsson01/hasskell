module Hasskell.Config
  ( Config (..),
    LoggingConfig (..),
    defaultLogging,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

data Config = Config
  { token :: !Text,
    baseUrl :: !Text,
    logging :: !LoggingConfig
  }

data LoggingConfig = Logging
  { debugLogger :: !(Text -> IO ()),
    infoLogger :: !(Text -> IO ()),
    errorLogger :: !(Text -> IO ())
  }

defaultLogging :: LoggingConfig
defaultLogging =
  Logging
    { debugLogger = putStrLn . T.unpack,
      infoLogger = putStrLn . T.unpack,
      errorLogger = putStrLn . T.unpack
    }
