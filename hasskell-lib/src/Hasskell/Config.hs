module Hasskell.Config
  ( Config (..),
    LoggingConfig (..),
    defaultLogging,
  )
where

import Control.Monad.Reader.Has
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data Config = Config
  { token :: !Text,
    baseUrl :: !Text,
    logging :: !LoggingConfig
  }
  deriving (Generic, Has LoggingConfig)

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
