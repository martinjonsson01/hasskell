{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Config
  ( -- Configuration from the user of the library.
    Config (..),
    LoggingConfig (..),
    Configured,
    getConfig,
    runConfigured,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Hasskell.Effects.Logging (LoggingConfig (..))

data Config = Config
  { token :: !Text,
    baseUrl :: !Text,
    logging :: !LoggingConfig
  }

data Configured :: Effect where
  GetConfig :: Configured m Config

makeEffect ''Configured

runConfigured :: Config -> Eff (Configured : es) a -> Eff es a
runConfigured config = interpret_ $ \case
  GetConfig -> pure config
